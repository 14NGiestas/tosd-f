!! Defines module [[tosd_validator]].
!> Validates a parsed TOML document against a [[tosd_schema]].
!>
!> Implemented checks (the rest of SPEC.md is refused at schema-load time, never
!> silently ignored): required/optional presence, built-in kinds, `allowedvalues`,
!> `dependentrequired`, and undeclared keys.
module tosd_validator
  use tosd_kinds
  use tosd_errors
  use tosd_schema
  use tomlf, only: toml_table, toml_array, toml_keyval, toml_value, toml_key, &
                   toml_load, get_value, toml_error, toml_stat
  use tomlf_constants, only: toml_type
  implicit none
  private

  public :: tosd_validate, tosd_validate_file

contains

  !! Validate a document already parsed by toml-f.
  subroutine tosd_validate(schema, doc, errors)
    type(tosd_schema_t), intent(in) :: schema
    type(toml_table), intent(inout) :: doc
    type(tosd_error_list_t), intent(out) :: errors

    integer :: i
    type(tosd_element_t) :: e

    if (.not. allocated(schema % elements)) return
    do i = 1, size(schema % elements)
      e = schema % elements(i)
      call check_element(schema, doc, e, errors)
    end do
    if (allocated(schema % dependencies)) then
      do i = 1, size(schema % dependencies)
        call check_dependency(doc, schema % dependencies(i), errors)
      end do
    end if
  end subroutine tosd_validate

  !! Convenience: load and validate a file.
  subroutine tosd_validate_file(schema, filename, errors)
    type(tosd_schema_t), intent(in) :: schema
    character(*), intent(in) :: filename
    type(tosd_error_list_t), intent(out) :: errors

    type(toml_table), allocatable :: doc
    type(toml_error), allocatable :: error

    call toml_load(doc, filename, error=error)
    if (allocated(error)) then
      call errors % add(tosd_err_schema, "", "cannot parse "//filename//": "//error%message)
      return
    end if
    call tosd_validate(schema, doc, errors)
  end subroutine tosd_validate_file

  !! One definition: presence, kind, allowedvalues, then the subtree.
  subroutine check_element(schema, doc, e, errors)
    type(tosd_schema_t), intent(in) :: schema
    type(toml_table), intent(inout) :: doc
    type(tosd_element_t), intent(in) :: e
    type(tosd_error_list_t), intent(inout) :: errors

    class(toml_value), pointer :: v
    character(:), allocatable :: p
    logical :: present

    p = path_string(e % path)
    present = present_in(doc, e % path, v)
    if (.not. present) then
      if (.not. e % optional) then
        call errors % add(tosd_err_missing, p, "required key is absent"//kind_hint(e))
      end if
      return
    end if
    call check_kind(e, v, p, errors)
    if (e % is_enum) call check_allowed(e, v, p, errors)
    ! `any` accepts any value, including tables with undeclared keys
    if (e % kind /= tosd_any) call check_children_declared(schema, e, v, p, errors)
  end subroutine check_element

  !! The TOML kind of the value must match the declared built-in type.
  subroutine check_kind(e, v, p, errors)
    type(tosd_element_t), intent(in) :: e
    class(toml_value), pointer, intent(in) :: v
    character(*), intent(in) :: p
    type(tosd_error_list_t), intent(inout) :: errors
    character(:), allocatable :: got

    if (e % kind == tosd_any) return
    got = kind_of(v)
    if (kind_matches(e % kind, got)) return
    call errors % add(tosd_err_type, p, "expected "//type_name(e % kind)//", found "//got)
  end subroutine check_kind

  !! `allowedvalues` membership (strings only, the common case).
  subroutine check_allowed(e, v, p, errors)
    type(tosd_element_t), intent(in) :: e
    class(toml_value), pointer, intent(in) :: v
    character(*), intent(in) :: p
    type(tosd_error_list_t), intent(inout) :: errors
    character(:), allocatable :: s
    integer :: i, stat

    select type (v)
    type is (toml_keyval)
      call get_value(v, s, stat=stat)
      if (stat /= toml_stat % success) return
    class default
      return
    end select
    do i = 1, size(e % allowed)
      if (trim(e % allowed(i)) == trim(s)) return
    end do
    call errors % add(tosd_err_allowed, p, "'"//trim(s)//"' is not one of the allowed values ("// &
         join_allowed(e % allowed)//")")
  end subroutine check_allowed

  !! Keys of a document table must be declared under this element's path.
  subroutine check_children_declared(schema, e, v, p, errors)
    type(tosd_schema_t), intent(in) :: schema
    type(tosd_element_t), intent(in) :: e
    class(toml_value), pointer, intent(in) :: v
    character(*), intent(in) :: p
    type(tosd_error_list_t), intent(inout) :: errors

    type(toml_table), pointer :: t
    type(toml_key), allocatable :: keys(:)
    integer :: i
    character(:), allocatable :: child, cp

    select type (v)
    type is (toml_table)
      t => v
    class default
      return
    end select
    call t % get_keys(keys)
    do i = 1, size(keys)
      child = trim(adjustl(keys(i) % key))
      if (declared(schema, tosd_extend_path(e % path, child))) cycle
      cp = p//"."//child
      if (len(p) == 0) cp = child
      call errors % add(tosd_err_unexpected, cp, "key is not described by the schema")
    end do
  end subroutine check_children_declared

  !! `dependentrequired`: in the rule's table, `trigger` present implies `requires` present.
  subroutine check_dependency(doc, rule, errors)
    type(toml_table), intent(inout) :: doc
    type(tosd_dep_t), intent(in) :: rule
    type(tosd_error_list_t), intent(inout) :: errors

    class(toml_value), pointer :: v, w
    character(:), allocatable :: p, q
    integer :: i

    p = path_string(rule % path)
    if (.not. present_in(doc, rule % path, v)) return
    if (.not. present_in(doc, tosd_extend_path(rule % path, rule % trigger), w)) return
    do i = 1, size(rule % requires)
      q = p//"."//trim(rule % requires(i))
      if (len(p) == 0) q = trim(rule % requires(i))
      if (.not. present_in(doc, tosd_extend_path(rule % path, rule % requires(i)), w)) then
        call errors % add(tosd_err_dependent, q, "required by dependentrequired triggered by sibling '"// &
             trim(rule % trigger)//"'")
      end if
    end do
  end subroutine check_dependency

  !--------------------------------------------------------------- helpers

  !! Resolve a dotted path inside a parsed document. Uses only the
  !! `intent(in)`-safe `% get` / `% has_key` bindings.
  logical function present_in(doc, path, v) result(found)
    type(toml_table), intent(inout) :: doc
    character(*), intent(in) :: path(:)
    class(toml_value), pointer, intent(out) :: v
    class(toml_value), pointer :: w
    type(toml_table), pointer :: t
    integer :: i

    nullify (v, w, t)
    if (size(path) == 0) then
      found = .false.
      return
    end if
    if (.not. doc % has_key(trim(path(1)))) then
      found = .false.
      return
    end if
    call doc % get(trim(path(1)), v)
    if (.not. associated(v)) then
      found = .false.
      return
    end if
    do i = 2, size(path)
      select type (v)
      type is (toml_table)
        t => v
      class default
        nullify (v)
        found = .false.
        return
      end select
      if (.not. t % has_key(trim(path(i)))) then
        nullify (v)
        found = .false.
        return
      end if
      call t % get(trim(path(i)), w)
      v => w
      if (.not. associated(v)) then
        found = .false.
        return
      end if
    end do
    found = .true.
  end function present_in

  !! Is this exact path declared in the schema (or is it on the way to one)?
  logical function declared(schema, path) result(yes)
    type(tosd_schema_t), intent(in) :: schema
    character(*), intent(in) :: path(:)
    integer :: i, n

    yes = .false.
    if (.not. allocated(schema % elements)) return
    do i = 1, size(schema % elements)
      n = size(schema % elements(i) % path)
      if (n < size(path)) cycle
      if (all(schema % elements(i) % path(:size(path)) == path)) then
        yes = .true.
        return
      end if
    end do
  end function declared

  logical function is_table(v) result(yes)
    class(toml_value), pointer, intent(in) :: v
    select type (v)
    type is (toml_table); yes = .true.
    class default;        yes = .false.
    end select
  end function is_table

  !! The TOML kind actually found, as a name. Key-value scalars are
  !! distinguished by the stored type (`get_type`), not by probing values.
  function kind_of(v) result(name)
    class(toml_value), pointer, intent(in) :: v
    character(:), allocatable :: name

    select type (v)
    type is (toml_table)
      name = "table"
    type is (toml_array)
      name = "array"
    type is (toml_keyval)
      select case (v % get_type())
      case (toml_type % string)
        name = "string"
      case (toml_type % int)
        name = "integer"
      case (toml_type % float)
        name = "float"
      case (toml_type % boolean)
        name = "boolean"
      case (toml_type % datetime)
        name = "datetime"
      case default
        name = "unknown"
      end select
    class default
      name = "unknown"
    end select
  end function kind_of

  pure logical function kind_matches(kind, got) result(yes)
    integer, intent(in) :: kind
    character(*), intent(in) :: got
    select case (kind)
    case (tosd_string);  yes = trim(got) == "string"
    case (tosd_integer); yes = trim(got) == "integer"
    case (tosd_float);   yes = trim(got) == "float" .or. trim(got) == "integer"
    case (tosd_boolean); yes = trim(got) == "boolean"
    case (tosd_table);   yes = trim(got) == "table"
    case (tosd_array);   yes = trim(got) == "array"
    case default;        yes = .true.
    end select
  end function kind_matches

  pure function type_name(kind) result(name)
    integer, intent(in) :: kind
    character(:), allocatable :: name
    select case (kind)
    case (tosd_string);  name = "string"
    case (tosd_integer); name = "integer"
    case (tosd_float);   name = "float"
    case (tosd_boolean); name = "boolean"
    case (tosd_table);   name = "table"
    case (tosd_array);   name = "array"
    case default;        name = "any"
    end select
  end function type_name

  pure function kind_hint(e) result(s)
    type(tosd_element_t), intent(in) :: e
    character(:), allocatable :: s
    s = " (expected "//type_name(e % kind)//")"
  end function kind_hint

  pure function path_string(path) result(s)
    character(*), intent(in) :: path(:)
    character(:), allocatable :: s
    integer :: i
    s = ""
    do i = 1, size(path)
      if (i > 1) s = s//"."
      s = s//trim(path(i))
    end do
  end function path_string

  pure function join_allowed(allowed) result(s)
    character(*), intent(in) :: allowed(:)
    character(:), allocatable :: s
    integer :: i
    s = ""
    do i = 1, size(allowed)
      if (i > 1) s = s//", "
      s = s//""""//trim(allowed(i))//""""
    end do
  end function join_allowed

end module tosd_validator
