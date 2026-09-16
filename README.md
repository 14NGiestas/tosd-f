# tosd-f

**TOML Schema (`.tosd`) validation in pure Fortran.** Validates TOML documents against a
schema that is itself TOML — the same idea as [toml-schema.org](https://toml-schema.org)
("Validate TOML without leaving TOML"), which today has reference implementations in Java,
Go, .NET, Python, Rust and TypeScript, but **none in Fortran**.

Usage target:

```fortran
use tosd_f
type(tosd_schema_t) :: schema
type(tosd_error_list_t) :: errs

call schema % load("schema.tosd")
if (.not. schema % is_ok()) then
  call schema % report()          ! o proprio schema esta' malformado
  stop 1
end if
call tosd_validate_file(schema, "input.toml", errs)
if (.not. errs % is_ok()) then
  call errs % report()            ! caminho da chave + mensagem
  stop 1
end if
```

## Status: WIP — does not compile yet

The design and ~600 lines are written (`src/tosd_kinds.f90`, `src/tosd_errors.f90`,
`src/tosd_schema.f90`, `src/tosd_validator.f90`, `test/main.f90`). The remaining work is
purely getting the toml-f API details right (measured errors, 2026-09-16):

1. `get_keys` is **type-bound**: use `call table % get_keys(list)`, not a module procedure.
2. There is no `get_len()` on `toml_value` for arrays: read array length and elements via
   the type-bound API (`arr % get_len()` is wrong; check `toml_array`'s methods).
3. `tosd_properties` and `tosd_children` (in `src/tosd_kinds.f90`) are **private** — add
   them to the `public ::` list.
4. Reading a string out of a `toml_array` element: `v => arr % get(j)`, then
   `select type (v); type is (toml_keyval); call get_value(v, s)`.

## Dependency choice (deliberate)

`fpm.toml` depends on `https://github.com/14NGiestas/toml-f` — **the same source**
CheesyHam uses, so that when CheesyHam depends on this package the fpm resolves a single
toml-f (two different sources of the same Fortran module names would collide).

## Scope

Supported properties: `type`, `optional`, `allowedvalues`, `itemtype`, `dependentrequired`,
`description`, `default`, `deprecated`, plus the `children` escape namespace (a document key
that collides with a property name, e.g. `type`, is written `children.type` in the schema).

The rest of SPEC.md (`oneof`, `anyof`, `if`/`then`/`else`, `allof`, `collection`,
`keypattern`, `pattern`, `format`, `min`/`max`, `minlength`/`maxlength`, `uniqueitems`,
`mutuallyexclusive`, `exactlyone`) is **recognised and refused with a diagnostic** — never
silently ignored, which is the failure mode this library exists to prevent.

## License

Not decided yet (deliberately left out of `fpm.toml`).
