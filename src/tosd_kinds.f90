!! Defines module [[tosd_kinds]].
!> Shared kind parameters and the schema-language constants.
module tosd_kinds
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private

  public :: wp
  public :: tosd_any, tosd_string, tosd_integer, tosd_float, tosd_boolean
  public :: tosd_table, tosd_array, tosd_collection

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

end module tosd_kinds
