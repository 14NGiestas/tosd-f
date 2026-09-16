!! Unit tests for tosd-f: schema loading and validation.
module test_tosd_f
  use tosd_kinds, only: tosd_err_missing, tosd_err_allowed, tosd_err_dependent, tosd_err_unsupported
  use tosd_errors
  use tosd_schema
  use tosd_validator
  use tomlf, only: toml_table, toml_load
  use testdrive, only: new_unittest, unittest_type, error_type, check
  implicit none
  private

  public :: collect

contains

  subroutine collect(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("schema_loads", test_schema_loads), &
      new_unittest("required_key_reported", test_required), &
      new_unittest("allowedvalues_enforced", test_allowed), &
      new_unittest("dependentrequired_enforced", test_dependent), &
      new_unittest("unsupported_property_refused", test_unsupported) &
    ]
  end subroutine collect

  subroutine write_temp(name, content)
    character(*), intent(in) :: name, content
    integer :: u
    open (newunit=u, file=name, status="replace", action="write")
    write (u, '(a)') trim(content)
    close (u)
  end subroutine write_temp

  subroutine test_schema_loads(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s

    call write_temp("/tmp/tosd_a.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.settings]'//new_line('a')//'type = "table"'//new_line('a')//'optional = true'//new_line('a')// &
      '[elements.settings.enabled]'//new_line('a')//'type = "boolean"'//new_line('a')//'optional = true')
    call s % load("/tmp/tosd_a.tosd")
    call check(error, s % errors % size(), 0, message="schema should load clean")
    if (allocated(error)) return
    call check(error, s % version, "1.0.0")
    if (allocated(error)) return
    call check(error, size(s % elements), 2, message="two elements")
  end subroutine test_schema_loads

  subroutine test_required(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s
    type(tosd_error_list_t) :: errs

    call write_temp("/tmp/tosd_b.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.settings]'//new_line('a')//'type = "table"'//new_line('a')//'optional = true'//new_line('a')// &
      '[elements.settings.max_iterations]'//new_line('a')//'type = "integer"')
    call write_temp("/tmp/tosd_b.toml", &
      '[settings]'//new_line('a')//'enabled = true')
    call s % load("/tmp/tosd_b.tosd")
    call tosd_validate_file(s, "/tmp/tosd_b.toml", errs)
    call check(error, errs % size() >= 1, message="max_iterations is required and absent")
    if (allocated(error)) return
    call check(error, errs % items(1) % code, tosd_err_missing)
  end subroutine test_required

  subroutine test_allowed(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s
    type(tosd_error_list_t) :: errs

    call write_temp("/tmp/tosd_c.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.settings]'//new_line('a')//'type = "table"'//new_line('a')//'optional = true'//new_line('a')// &
      '[elements.settings.major]'//new_line('a')//'type = "string"'//new_line('a')// &
      'allowedvalues = ["hole", "electron"]'//new_line('a')//'optional = true')
    call write_temp("/tmp/tosd_c.toml", &
      '[settings]'//new_line('a')//'major = "banana"')
    call s % load("/tmp/tosd_c.tosd")
    call tosd_validate_file(s, "/tmp/tosd_c.toml", errs)
    call check(error, errs % size(), 1, message="banana is not allowed")
    if (allocated(error)) return
    call check(error, errs % items(1) % code, tosd_err_allowed)
    if (allocated(error)) return
    call check(error, errs % items(1) % path, "settings.major")
  end subroutine test_allowed

  subroutine test_dependent(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s
    type(tosd_error_list_t) :: errs

    call write_temp("/tmp/tosd_d.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.settings.self_consistency]'//new_line('a')//'type = "table"'//new_line('a')// &
      'optional = true'//new_line('a')// &
      'dependentrequired = { major_concentration = ["major"] }'//new_line('a')// &
      '[elements.settings.self_consistency.major]'//new_line('a')//'type = "string"'//new_line('a')//'optional = true'//new_line('a')// &
      '[elements.settings.self_consistency.major_concentration]'//new_line('a')//'type = "any"'//new_line('a')//'optional = true')
    call write_temp("/tmp/tosd_d.toml", &
      '[settings.self_consistency]'//new_line('a')//'major_concentration = { value = 6.0e11, unit = "cm^-2" }')
    call s % load("/tmp/tosd_d.tosd")
    call tosd_validate_file(s, "/tmp/tosd_d.toml", errs)
    call check(error, errs % size(), 1, message="major is required by dependentrequired")
    if (allocated(error)) return
    call check(error, errs % items(1) % code, tosd_err_dependent)
    if (allocated(error)) return
    call check(error, errs % items(1) % path, "settings.self_consistency.major")
  end subroutine test_dependent

  subroutine test_unsupported(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s

    call write_temp("/tmp/tosd_e.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.x]'//new_line('a')//'type = "string"'//new_line('a')//'pattern = "^a+$"')
    call s % load("/tmp/tosd_e.tosd")
    call check(error, s % errors % size(), 1, message="pattern is recognised but not implemented")
    if (allocated(error)) return
    call check(error, s % errors % items(1) % code, tosd_err_unsupported)
  end subroutine test_unsupported

end module test_tosd_f

program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite
  use test_tosd_f, only: collect
  implicit none
  integer :: stat
  stat = 0
  call run_testsuite(collect, error_unit, stat)
  if (stat > 0) then
    write (error_unit, '(i0,a)') stat, " test(s) failed"
    error stop 1
  end if
end program tester
