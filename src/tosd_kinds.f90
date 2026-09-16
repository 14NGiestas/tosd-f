!! Defines module [[tosd_kinds]].
!> Shared kind parameters and the schema-language constants.
module tosd_kinds
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private

  public :: wp
  public :: tosd_any, tosd_string, tosd_integer, tosd_float, tosd_boolean
  public :: tosd_table, tosd_array, tosd_collection
  public :: tosd_properties, tosd_children
  public :: tosd_path_len, tosd_extend_path
  public :: tosd_type_name

  integer, parameter :: wp = real64

  !> The built-in types of TOML Schema 1.0 that this implementation understands.
  enum, bind(c)
    enumerator :: tosd_any = 0        !! `any` (unconstrained)
    enumerator :: tosd_string = 1
    enumerator :: tosd_integer = 2
    enumerator :: tosd_float = 3
    enumerator :: tosd_boolean = 4
    enumerator :: tosd_table = 5
    enumerator :: tosd_array = 6
    enumerator :: tosd_collection = 7 !! recognised, not implemented yet
  end enum

  !> The complete property set of the language (SPEC.md: "The set is closed").
  !> A schema that names anything else is malformed and MUST be rejected.
  character(*), parameter :: tosd_properties(*) = [character(24) :: &
      "type", "description", "format", "itemtype", "items", "oneof", "anyof", &
      "if", "then", "else", "allof", "allowedvalues", "pattern", "keypattern", &
      "optional", "min", "max", "minlength", "maxlength", "uniqueitems", &
      "dependentrequired", "mutuallyexclusive", "exactlyone", "default", "deprecated"]

  !> Escape namespace segment: a document key that collides with a property name is
  !> written as `children.<key>` in the schema (SPEC.md, "The first `children`
  !> segment is the escape namespace").
  character(*), parameter :: tosd_children = "children"

  !> Fixed width for key-path components passed between procedures.
  !>
  !> INTERNAL USE (stays public only because `tosd_schema` and `tosd_validator`
  !> share it; not part of the user-facing API).
  !>
  !> Rationale (measured 2026-09-16, gfortran 15): an array constructor such as
  !> `[path, name]` where `path` is an assumed-length (`character(*)`) dummy
  !> silently yields zero-length, content-less elements. All path extension goes
  !> through [[tosd_extend_path]], which only uses scalar assignments into an
  !> explicitly-sized buffer.
  integer, parameter :: tosd_path_len = 256

contains

  !> Return `path` with `key` appended (pure, no array constructor).
  !> INTERNAL USE, see [[tosd_path_len]].
  pure function tosd_extend_path(path, key) result(newpath)
    character(*), intent(in) :: path(:)
    character(*), intent(in) :: key
    character(tosd_path_len), allocatable :: newpath(:)
    integer :: n, i

    n = size(path)
    allocate (newpath(n + 1))
    do i = 1, n
      newpath(i) = path(i)
    end do
    newpath(n + 1) = key
  end function tosd_extend_path

  !> Name of a built-in schema type, for diagnostics and dumps.
  pure function tosd_type_name(kind) result(name)
    integer, intent(in) :: kind
    character(:), allocatable :: name
    select case (kind)
    case (tosd_string);     name = "string"
    case (tosd_integer);    name = "integer"
    case (tosd_float);      name = "float"
    case (tosd_boolean);    name = "boolean"
    case (tosd_table);      name = "table"
    case (tosd_array);      name = "array"
    case (tosd_collection); name = "collection"
    case default;           name = "any"
    end select
  end function tosd_type_name

end module tosd_kinds
