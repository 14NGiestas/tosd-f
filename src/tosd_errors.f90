!! Defines module [[tosd_errors]].
!> The diagnostics produced by schema loading and by validation.
module tosd_errors
  use tosd_kinds, only: wp
  implicit none
  private

  public :: tosd_error_t, tosd_error_list_t
  public :: tosd_ok, tosd_err_schema, tosd_err_missing, tosd_err_type
  public :: tosd_err_allowed, tosd_err_dependent, tosd_err_unexpected
  public :: tosd_err_unsupported, tosd_err_value

  integer, parameter :: tosd_ok = 0
  integer, parameter :: tosd_err_schema = 1        !! the .tosd itself is malformed
  integer, parameter :: tosd_err_missing = 2       !! required key absent
  integer, parameter :: tosd_err_type = 3          !! wrong TOML kind
  integer, parameter :: tosd_err_allowed = 4       !! value outside allowedvalues
  integer, parameter :: tosd_err_dependent = 5     !! dependentrequired violated
  integer, parameter :: tosd_err_unexpected = 6    !! key not described by the schema
  integer, parameter :: tosd_err_unsupported = 7   !! property recognised but not here
  integer, parameter :: tosd_err_value = 8         !! other value constraint

  !> One diagnostic. `path` is the dotted key path in the DOCUMENT ("" for the root).
  type :: tosd_error_t
    integer :: code = tosd_ok
    character(:), allocatable :: path
    character(:), allocatable :: message
  end type tosd_error_t

  !> Growable list of diagnostics.
  type :: tosd_error_list_t
    type(tosd_error_t), allocatable :: items(:)
  contains
    procedure :: add => error_list_add
    procedure :: size => error_list_size
    procedure :: is_ok => error_list_is_ok
    procedure :: report => error_list_report
  end type tosd_error_list_t

contains

  !! Append one diagnostic, growing the list.
  subroutine error_list_add(self, code, path, message)
    class(tosd_error_list_t), intent(inout) :: self
    integer, intent(in) :: code
    character(*), intent(in) :: path, message
    type(tosd_error_t), allocatable :: tmp(:)

    if (.not. allocated(self % items)) then
      allocate(self % items(1))
    else
      allocate(tmp(size(self % items) + 1))
      tmp(:size(self % items)) = self % items
      call move_alloc(tmp, self % items)
    end if
    associate (last => self % items(size(self % items)))
      last % code = code
      last % path = path
      last % message = message
    end associate
  end subroutine error_list_add

  !! Number of diagnostics collected.
  pure integer function error_list_size(self) result(n)
    class(tosd_error_list_t), intent(in) :: self
    n = 0
    if (allocated(self % items)) n = size(self % items)
  end function error_list_size

  !! True when nothing was collected.
  pure logical function error_list_is_ok(self) result(ok)
    class(tosd_error_list_t), intent(in) :: self
    ok = .not. allocated(self % items)
  end function error_list_is_ok

  !! One line per diagnostic, `path: message`.
  subroutine error_list_report(self, unit)
    class(tosd_error_list_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer :: u, i

    u = 6
    if (present(unit)) u = unit
    if (.not. allocated(self % items)) return
    do i = 1, size(self % items)
      if (len(self % items(i) % path) > 0) then
        write(u, '(a,a,a)') trim(self % items(i) % path), ": ", self % items(i) % message
      else
        write(u, '(a)') self % items(i) % message
      end if
    end do
  end subroutine error_list_report

end module tosd_errors
