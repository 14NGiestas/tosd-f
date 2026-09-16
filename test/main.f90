!! Unit tests for tosd-f: schema loading and validation.
module test_tosd_f
  use tosd_errors, only: tosd_err_missing, tosd_err_allowed, tosd_err_dependent, &
                      tosd_err_unsupported, tosd_err_schema
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
      new_unittest("unsupported_property_refused", test_unsupported), &
      new_unittest("spec_compliance_refused", test_compliance), &
      new_unittest("unknown_entry_refused", test_unknown_entry), &
      new_unittest("docs_dir_scan", test_docs_dir_scan) &
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
    logical :: found
    integer :: i

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
    ! error order is not contractual: find the missing-key diagnostic
    found = .false.
    do i = 1, errs % size()
      if (errs % items(i) % code == tosd_err_missing .and. &
          errs % items(i) % path == "settings.max_iterations") found = .true.
    end do
    call check(error, found, message="missing settings.max_iterations is reported")
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

  !> SPEC compliance: every TOML Schema 1.0 property outside the supported
  !> subset must be recognised AND refused with `tosd_err_unsupported` --
  !> never silently ignored. One schema exercises all of them at once.
  subroutine test_compliance(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s
    integer :: i, n

    call write_temp("/tmp/tosd_f.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.a]'//new_line('a')//'type = "string"'//new_line('a')// &
      'pattern = "^a+$"'//new_line('a')//'format = "email"'//new_line('a')// &
      'keypattern = "^k"'//new_line('a')//'minlength = 1'//new_line('a')// &
      'maxlength = 9'//new_line('a')// &
      '[elements.b]'//new_line('a')//'type = "integer"'//new_line('a')// &
      'min = 0'//new_line('a')//'max = 10'//new_line('a')// &
      '[elements.c]'//new_line('a')//'type = "array"'//new_line('a')// &
      'uniqueitems = true'//new_line('a')//'items = ["string"]'//new_line('a')// &
      '[elements.d]'//new_line('a')//'type = "string"'//new_line('a')// &
      'oneof = [{ type = "string" }]'//new_line('a')// &
      'anyof = [{ type = "string" }]'//new_line('a')// &
      'allof = [{ type = "string" }]'//new_line('a')// &
      '[elements.e]'//new_line('a')//'type = "string"'//new_line('a')// &
      '[elements.e.if]'//new_line('a')//'type = "string"'//new_line('a')// &
      '[elements.e.then]'//new_line('a')//'type = "string"'//new_line('a')// &
      '[elements.e.else]'//new_line('a')//'type = "string"'//new_line('a')// &
      '[elements.f]'//new_line('a')//'type = "string"'//new_line('a')// &
      'mutuallyexclusive = ["a", "b"]'//new_line('a')// &
      'exactlyone = ["a", "b"]'//new_line('a')// &
      '[elements.g]'//new_line('a')//'type = "collection"')
    call s % load("/tmp/tosd_f.tosd")
    ! 5 + 2 + 2 + 3 + 3 + 2 + 1 = 18 refused properties
    call check(error, s % errors % size(), 18, message="all unimplemented properties refused")
    if (allocated(error)) return
    n = 0
    do i = 1, s % errors % size()
      if (s % errors % items(i) % code == tosd_err_unsupported) n = n + 1
    end do
    call check(error, n, 18, message="every diagnostic is tosd_err_unsupported")
    if (allocated(error)) return
    call check(error, s % is_ok(), .false., message="non-conforming schema is not ok")
  end subroutine test_compliance

  !> A schema entry that is neither a known property nor a child table is
  !> malformed and must be rejected (closed property set), not skipped.
  subroutine test_unknown_entry(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: s

    call write_temp("/tmp/tosd_g.tosd", &
      '[toml-schema]'//new_line('a')//'version = "1.0.0"'//new_line('a')// &
      '[elements.x]'//new_line('a')//'type = "string"'//new_line('a')//'frobnicate = 1')
    call s % load("/tmp/tosd_g.tosd")
    call check(error, s % errors % size(), 1, message="unknown entry is rejected")
    if (allocated(error)) return
    call check(error, s % errors % items(1) % code, tosd_err_schema)
  end subroutine test_unknown_entry

  !> Generic document-directory scan (no hardcoded paths, no network).
  !>
  !> Set `TOSD_SCHEMA_FILE` and `TOSD_DOCS_DIR` to validate every `*.toml`
  !> under the directory against the schema. Files that fail to parse are
  !> skipped (fragments); `unexpected key` diagnostics are ignored, mirroring
  !> a rules-only run against a partial schema. `TOSD_EXPECTED_VIOLATIONS`
  !> (default 0) is the expected number of files with RULE violations
  !> (missing / type / allowed / dependent). Unset variables: skip quietly,
  !> so the package test suite stays green on its own.
  subroutine test_docs_dir_scan(error)
    type(error_type), allocatable, intent(out) :: error
    type(tosd_schema_t) :: schema
    type(tosd_error_list_t) :: errs
    character(1024) :: schema_file, docs_dir, expected_s, line
    character(2048) :: cmd
    integer :: st, xs, u, ios, nfiles, nskip, nviol, expected, i, rule_err
    logical :: has_rule_violation

    call get_environment_variable("TOSD_SCHEMA_FILE", schema_file, status=st)
    if (st /= 0 .or. len_trim(schema_file) == 0) return
    call get_environment_variable("TOSD_DOCS_DIR", docs_dir, status=st)
    if (st /= 0 .or. len_trim(docs_dir) == 0) return
    expected = 0
    call get_environment_variable("TOSD_EXPECTED_VIOLATIONS", expected_s, status=st)
    if (st == 0 .and. len_trim(expected_s) > 0) read (expected_s, *, iostat=ios) expected

    call schema % load(trim(schema_file))
    call check(error, schema % is_ok(), message="scan schema loads clean")
    if (allocated(error)) then
      call schema % report()
      return
    end if
    cmd = 'find "'//trim(docs_dir)//'" -name ''*.toml'' > /tmp/tosd_scan_list.txt 2>/dev/null'
    call execute_command_line(cmd, exitstat=xs)
    call check(error, xs, 0, message="docs dir is listable")
    if (allocated(error)) return
    nfiles = 0
    nskip = 0
    nviol = 0
    open (newunit=u, file="/tmp/tosd_scan_list.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) return
    do
      read (u, '(a)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle
      nfiles = nfiles + 1
      call tosd_validate_file(schema, trim(line), errs)
      if (.not. allocated(errs % items)) cycle
      has_rule_violation = .false.
      do i = 1, size(errs % items)
        rule_err = errs % items(i) % code
        if (rule_err == tosd_err_missing .or. rule_err == tosd_err_type .or. &
            rule_err == tosd_err_allowed .or. rule_err == tosd_err_dependent .or. &
            rule_err == tosd_err_value) then
          has_rule_violation = .true.
        end if
        if (errs % items(i) % code == tosd_err_schema) then
          ! unparsable document: fragment, skip like the reference runner
          has_rule_violation = .false.
          nskip = nskip + 1
          exit
        end if
      end do
      if (allocated(errs % items)) deallocate (errs % items)
      if (has_rule_violation) then
        nviol = nviol + 1
        write (*, '(a,a)') "  rule violations in ", trim(line)
      end if
    end do
    close (u)
    write (*, '(a,i0,a,i0,a,i0,a)') "  scanned ", nfiles, " files, ", nviol, &
      " with rule violations (", nskip, " skipped)"
    call check(error, nviol, expected, message="rule-violation file count")
  end subroutine test_docs_dir_scan

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
