!! Validate two documents against `example/schema.tosd`, showing a pass,
!! a fail, and what the schema declares. Run with:
!! `fortran-fpm run --example demo` (from the package root).
program demo
  use tosd_f
  implicit none
  type(tosd_schema_t) :: schema
  type(tosd_error_list_t) :: errs

  call schema % load("example/schema.tosd")
  if (.not. schema % is_ok()) then
    call schema % report()
    error stop 1
  end if
  print '(a)', "--- schema declares:"
  call schema % dump()
  call tosd_validate_file(schema, "example/good.toml", errs)
  print '(a,l1)', "--- good.toml valid: ", errs % is_ok()
  call tosd_validate_file(schema, "example/bad.toml", errs)
  print '(a,l1)', "--- bad.toml valid: ", errs % is_ok()
  call errs % report()
end program demo
