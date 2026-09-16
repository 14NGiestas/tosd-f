!! Defines module [[tosd_schema]].
!> Loads a TOML Schema (`.tosd`) document into a typed structure.
!>
!> The schema is itself TOML. This module reads `[toml-schema]` and the `[elements]`
!> tree, resolving the `children` escape namespace (a document key whose name collides
!> with a schema property is written as `children.<key>`).
!>
!> Supported properties: `type`, `optional`, `allowedvalues`, `itemtype`,
!> `dependentrequired`, `description`, `default`, `deprecated`. The rest of the
!> language (SPEC.md) is *recognised and refused*, never ignored.
module tosd_schema
  use tosd_kinds
  use tosd_errors
  use tomlf, only: toml_table, toml_array, toml_keyval, toml_value, toml_key, &
                   toml_load, get_value, toml_error
  use tomlf, only: tosd_len => len
  implicit none
  private

  public :: tosd_element_t, tosd_dep_t, tosd_schema_t

  !> One definition under `[elements.*]`.
  type :: tosd_element_t
    character(:), allocatable :: path(:)        !! key path in the DOCUMENT
    integer :: kind = tosd_any
    logical :: optional = .false.
    logical :: has_children = .false.
    logical :: is_enum = .false.
    character(:), allocatable :: allowed(:)     !! allowedvalues (strings)
    integer :: itemtype = tosd_any              !! for arrays
  end type tosd_element_t

  !> One `dependentrequired` rule: in the table `path`, if `trigger` is present then
  !! every key in `requires` must be present too.
  type :: tosd_dep_t
    character(:), allocatable :: path(:)
    character(:), allocatable :: trigger
    character(:), allocatable :: requires(:)
  end type tosd_dep_t

  !> A loaded schema.
  type :: tosd_schema_t
    character(:), allocatable :: version
    type(tosd_element_t), allocatable :: elements(:)
    type(tosd_dep_t), allocatable :: dependencies(:)
    type(tosd_error_list_t) :: errors
  contains
    procedure :: load => schema_load
    procedure :: free => schema_free
    procedure :: is_ok => schema_is_ok
    procedure :: report => schema_report
  end type tosd_schema_t

contains

  !! Load a schema from a `.tosd` file. Diagnostics land in `self % errors`.
  subroutine schema_load(self, filename)
    class(tosd_schema_t), intent(inout) :: self
    character(*), intent(in) :: filename
    type(toml_table), allocatable :: root
    type(toml_error), allocatable :: error
    class(toml_value), pointer :: child
    character(tosd_path_len) :: empty(0)

    nullify (child)
    call self % free()
    call toml_load(root, filename, error=error)
    if (allocated(error)) then
      call self % errors % add(tosd_err_schema, "", "cannot parse schema "//filename//": "//error%message)
      return
    end if
    call load_meta(self, root)
    if (root % has_key("elements")) then
    call root % get("elements", child)
      select type (child)
      type is (toml_table)
        call walk(self, child, empty)
      end select
    else
      call self % errors % add(tosd_err_schema, "", "schema has no [elements] table")
    end if
  end subroutine schema_load

  !! `[toml-schema] version` is required by the language.
  subroutine load_meta(self, root)
    class(tosd_schema_t), intent(inout) :: self
    type(toml_table), intent(in) :: root
    class(toml_value), pointer :: v, w

    nullify (v, w)
    if (.not. root % has_key("toml-schema")) then
      call self % errors % add(tosd_err_schema, "", "schema lacks the [toml-schema] table")
      return
    end if
    call root % get("toml-schema", v)
    select type (v)
    type is (toml_table)
      if (.not. v % has_key("version")) then
        call self % errors % add(tosd_err_schema, "", "[toml-schema] has no version")
        return
      end if
      call v % get("version", w)
      select type (w)
      type is (toml_keyval)
        call get_value(w, self % version)
      end select
    end select
  end subroutine load_meta

  !! Recursively read one level of `[elements]`: properties first, then children.
  recursive subroutine walk(self, table, path)
    class(tosd_schema_t), intent(inout) :: self
    type(toml_table), intent(in) :: table
    character(*), intent(in) :: path(:)

    type(toml_key), allocatable :: keys(:)
    type(tosd_element_t) :: element
    character(:), allocatable :: name
    class(toml_value), pointer :: child
    integer :: i

    nullify (child)
    element % path = path
    call read_properties(self, table, path, element)
    if (size(path) > 0) call push_element(self, element)

    call table % get_keys(keys)
    do i = 1, size(keys)
      name = trim(adjustl(keys(i) % key))
      if (is_property(name)) cycle                       ! already consumed above
      if (name == tosd_children) then
        ! the escape namespace: what is inside are LITERAL document keys
        call table % get("children", child)
        if (.not. associated(child)) cycle
        select type (child)
        type is (toml_table)
          call walk_literal(self, child, path)
        end select
        cycle
      end if
      call table % get(trim(name), child)
      if (.not. associated(child)) cycle
      select type (child)
      type is (toml_table)
        call walk(self, child, tosd_extend_path(path, name))
      class default
        ! a scalar (or array) entry that is neither a property nor a child
        ! table is malformed: refuse it instead of silently ignoring it
        call self % errors % add(tosd_err_schema, join(tosd_extend_path(path, name)), &
             "unknown schema entry '"//trim(name)//"' (not a property, not a child table)")
      end select
    end do
  end subroutine walk

  !! Inside `children`, the keys are literal document keys: append and keep walking.
  recursive subroutine walk_literal(self, table, path)
    class(tosd_schema_t), intent(inout) :: self
    type(toml_table), intent(in) :: table
    character(*), intent(in) :: path(:)

    type(toml_key), allocatable :: keys(:)
    type(tosd_element_t) :: element
    character(:), allocatable :: name
    class(toml_value), pointer :: child
    integer :: i

    nullify (child)
    element % path = path
    call read_properties(self, table, path, element)
    if (size(path) > 0) call push_element(self, element)
    call table % get_keys(keys)
    do i = 1, size(keys)
      name = trim(adjustl(keys(i) % key))
      ! `dependentrequired` is the only table-valued property: it belongs to
      ! this level (already consumed above), never to a literal document key.
      if (trim(name) == "dependentrequired") cycle
      call table % get(trim(name), child)
      if (.not. associated(child)) cycle
      select type (child)
      type is (toml_table)
        call walk_literal(self, child, tosd_extend_path(path, trim(name)))
      end select
    end do
  end subroutine walk_literal

  !! Read the properties of one definition (children are handled by the caller).
  subroutine read_properties(self, table, path, element)
    class(tosd_schema_t), intent(inout) :: self
    type(toml_table), intent(in) :: table
    character(*), intent(in) :: path(:)
    type(tosd_element_t), intent(inout) :: element

    type(toml_key), allocatable :: keys(:)
    class(toml_value), pointer :: v
    type(toml_array), pointer :: arr
    type(toml_table), pointer :: dep
    character(:), allocatable :: name, tipo, item
    integer :: i, j, n, stat

    nullify (v, arr, dep)
    call table % get_keys(keys)
    do i = 1, size(keys)
      name = trim(adjustl(keys(i) % key))
      if (.not. is_property(name)) cycle
      if (.not. is_supported(name)) then
        call self % errors % add(tosd_err_unsupported, join(path), &
             "property '"//trim(name)//"' is recognised by TOML Schema 1.0 but not implemented here")
        cycle
      end if
      call table % get(trim(name), v)
      if (.not. associated(v)) cycle
      select case (trim(name))
      case ("type")
        select type (v)
        type is (toml_keyval)
          tipo = ""
          call get_value(v, tipo, stat=stat)
          element % kind = kind_from_name(tipo)
          if (element % kind == tosd_collection) &
            call self % errors % add(tosd_err_unsupported, join(path), &
                 "'collection' is not implemented here")
          if (element % kind < 0) &
            call self % errors % add(tosd_err_schema, join(path), &
                 "unknown built-in type '"//trim(tipo)//"'")
        end select
      case ("optional")
        select type (v)
        type is (toml_keyval)
          call get_value(v, element % optional, stat=stat)
          if (stat /= 0) &
            call self % errors % add(tosd_err_schema, join(path), "'optional' must be a boolean")
        end select
      case ("itemtype")
        select type (v)
        type is (toml_keyval)
          item = ""
          call get_value(v, item, stat=stat)
          element % itemtype = kind_from_name(item)
          if (element % itemtype < 0) &
            call self % errors % add(tosd_err_schema, join(path), &
                 "unknown built-in type '"//trim(item)//"'")
        end select
      case ("allowedvalues")
        select type (v)
        type is (toml_array)
          arr => v
          n = tosd_len(arr)
          allocate (character(256) :: element % allowed(n))
          do j = 1, n
            item = ""
            call get_value(arr, j, item, stat=stat)
            if (stat /= 0) then
              ! only string enums are enforced here: refuse, never ignore
              call self % errors % add(tosd_err_unsupported, join(path), &
                   "non-string 'allowedvalues' are not implemented here")
              deallocate (element % allowed)
              exit
            end if
            element % allowed(j) = item
          end do
          if (allocated(element % allowed)) element % is_enum = .true.
        end select
      case ("dependentrequired")
        select type (v)
        type is (toml_table)
          dep => v
          call read_dependent(self, dep, path)
        end select
      case ("description", "default", "deprecated")
        continue                                          ! annotations: accepted, inert
      end select
    end do
  end subroutine read_properties

  !! `dependentrequired = { trigger = ["a", "b"] }` on a table definition.
  subroutine read_dependent(self, table, path)
    class(tosd_schema_t), intent(inout) :: self
    type(toml_table), intent(in) :: table
    character(*), intent(in) :: path(:)

    type(toml_key), allocatable :: keys(:)
    class(toml_value), pointer :: v
    type(toml_array), pointer :: arr
    type(tosd_dep_t) :: rule
    character(:), allocatable :: item
    integer :: i, j, n, stat

    nullify (v, arr)
    call table % get_keys(keys)
    do i = 1, size(keys)
      if (allocated(rule % requires)) deallocate (rule % requires)
      rule % path = path
      rule % trigger = trim(adjustl(keys(i) % key))
      call table % get(trim(rule % trigger), v)
      if (.not. associated(v)) cycle
      select type (v)
      type is (toml_array)
        arr => v
        n = tosd_len(arr)
        allocate (character(256) :: rule % requires(n))
        do j = 1, n
          item = ""
          call get_value(arr, j, item, stat=stat)
          if (stat /= 0) then
            call self % errors % add(tosd_err_schema, join(path), &
                 "'dependentrequired' entries must be strings")
            deallocate (rule % requires)
            exit
          end if
          rule % requires(j) = item
        end do
        if (allocated(rule % requires)) call push_dependency(self, rule)
      end select
    end do
  end subroutine read_dependent

  !! Grow `elements` by one.
  subroutine push_element(self, element)
    class(tosd_schema_t), intent(inout) :: self
    type(tosd_element_t), intent(in) :: element
    type(tosd_element_t), allocatable :: tmp(:)
    integer :: n

    n = 0
    if (allocated(self % elements)) n = size(self % elements)
    allocate (tmp(n + 1))
    if (n > 0) tmp(:n) = self % elements
    tmp(n + 1) = element
    call move_alloc(tmp, self % elements)
  end subroutine push_element

  !! Grow `dependencies` by one.
  subroutine push_dependency(self, rule)
    class(tosd_schema_t), intent(inout) :: self
    type(tosd_dep_t), intent(in) :: rule
    type(tosd_dep_t), allocatable :: tmp(:)
    integer :: n

    n = 0
    if (allocated(self % dependencies)) n = size(self % dependencies)
    allocate (tmp(n + 1))
    if (n > 0) tmp(:n) = self % dependencies
    tmp(n + 1) = rule
    call move_alloc(tmp, self % dependencies)
  end subroutine push_dependency

  pure logical function is_property(name) result(yes)
    character(*), intent(in) :: name
    integer :: i
    yes = .false.
    do i = 1, size(tosd_properties)
      if (trim(tosd_properties(i)) == trim(name)) then
        yes = .true.
        return
      end if
    end do
  end function is_property

  !! Implemented in this version (the rest is recognised and refused).
  pure logical function is_supported(name) result(yes)
    character(*), intent(in) :: name
    select case (trim(name))
    case ("type", "optional", "allowedvalues", "itemtype", "dependentrequired", &
          "description", "default", "deprecated")
      yes = .true.
    case default
      yes = .false.
    end select
  end function is_supported

  pure integer function kind_from_name(name) result(kind)
    character(*), intent(in) :: name
    select case (trim(name))
    case ("any");       kind = tosd_any
    case ("string");    kind = tosd_string
    case ("integer");   kind = tosd_integer
    case ("float");     kind = tosd_float
    case ("boolean");   kind = tosd_boolean
    case ("table");     kind = tosd_table
    case ("array");     kind = tosd_array
    case ("collection"); kind = tosd_collection
    case default;       kind = -1
    end select
  end function kind_from_name

  pure function join(path) result(s)
    character(*), intent(in) :: path(:)
    character(:), allocatable :: s
    integer :: i
    s = ""
    do i = 1, size(path)
      if (i > 1) s = s//"."
      s = s//trim(path(i))
    end do
  end function join

  logical function is_table(v) result(yes)
    class(toml_value), pointer, intent(in) :: v
    select type (v)
    type is (toml_table); yes = .true.
    class default;        yes = .false.
    end select
  end function is_table

  function lookup(table, name) result(v)
    type(toml_table), intent(in) :: table
    character(*), intent(in) :: name
    class(toml_value), pointer :: v
    nullify (v)
    call table % get(trim(name), v)
  end function lookup

  function as_table(table, name) result(t)
    type(toml_table), intent(in) :: table
    character(*), intent(in) :: name
    type(toml_table), pointer :: t
    class(toml_value), pointer :: v
    nullify (t, v)
    call table % get(trim(name), v)
    select type (v)
    type is (toml_table)
      t => v
    end select
  end function as_table

  subroutine schema_free(self)
    class(tosd_schema_t), intent(inout) :: self
    if (allocated(self % elements)) deallocate (self % elements)
    if (allocated(self % dependencies)) deallocate (self % dependencies)
    if (allocated(self % errors % items)) deallocate (self % errors % items)
  end subroutine schema_free

  pure logical function schema_is_ok(self) result(ok)
    class(tosd_schema_t), intent(in) :: self
    ok = self % errors % is_ok() .and. allocated(self % elements)
  end function schema_is_ok

  subroutine schema_report(self, unit)
    class(tosd_schema_t), intent(in) :: self
    integer, intent(in), optional :: unit
    call self % errors % report(unit)
  end subroutine schema_report

end module tosd_schema
