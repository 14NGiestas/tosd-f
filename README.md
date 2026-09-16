# tosd-f

**TOML Schema (`.tosd`) validation in pure Fortran.** Validates TOML documents against a
schema that is itself TOML — the same idea as [toml-schema.org](https://toml-schema.org)
("Validate TOML without leaving TOML"), which today has reference implementations in Java,
Go, .NET, Python, Rust and TypeScript, but **none in Fortran**.

```fortran
use tosd_f
type(tosd_schema_t) :: schema
type(tosd_error_list_t) :: errs

call schema % load("schema.tosd")
if (.not. schema % is_ok()) then
  call schema % report()          ! the schema itself is malformed
  stop 1
end if
call tosd_validate_file(schema, "input.toml", errs)
if (.not. errs % is_ok()) then
  call errs % report()            ! dotted key path + message, one per line
  stop 1
end if
```

Build and test (note: the `fpm` on a default PATH may be an unrelated tool —
use the Fortran Package Manager). With Nix (pinned toolchain):

```bash
nix develop --command bash -c "fortran-fpm test"
```

Without Nix: install `gfortran` + `fortran-fpm` (0.13+) and run `fortran-fpm test`.
On CI (`.github/workflows/fpm.yml`) the Nix route above is used.

## Dependency choice (deliberate)

`fpm.toml` depends on `https://github.com/14NGiestas/toml-f` — **the same source**
our downstream users pin, so that when this package is used as a dependency the fpm
resolves a single toml-f (two different sources of the same Fortran module names
would collide).

## Scope and standard compliance

Supported properties: `type`, `optional`, `allowedvalues` (strings),
`itemtype`, `dependentrequired`, `description`, `default`, `deprecated`,
plus the `children` escape namespace (a document key that collides with a
property name, e.g. `type`, is written `children.type` in the schema).

Everything else in the TOML Schema 1.0 property set (`oneof`, `anyof`,
`if`/`then`/`else`, `allof`, `collection`, `keypattern`, `pattern`, `format`,
`min`/`max`, `minlength`/`maxlength`, `uniqueitems`, `mutuallyexclusive`,
`exactlyone`, non-string `allowedvalues`) is **recognised and refused with a
`tosd_err_unsupported` diagnostic** — never silently ignored, which is the
failure mode this library exists to prevent. A schema entry that is neither a
known property nor a child table is likewise rejected (`tosd_err_schema`:
the property set is closed). The `spec_compliance_refused` and
`unknown_entry_refused` tests pin this behaviour (18 refused properties in
one schema, plus the unknown-entry case).

Checks performed on documents: required/optional presence, built-in kinds
(`string`, `integer`, `float` — integers accepted where floats are declared —
`boolean`, `table`, `array`, `any`, `datetime` reported by kind), string
`allowedvalues` membership, `dependentrequired`, and undeclared keys under
table-typed elements (`tosd_err_unexpected`; `any`-typed elements accept
anything, including tables with undeclared keys).

## Directory scan test (no hardcoded paths)

`docs_dir_scan` validates every `*.toml` found recursively under a directory
against a schema, driven entirely by environment variables — set them for a
real cross-check, unset them and the test skips quietly so the suite stays
green standalone:

```bash
TOSD_SCHEMA_FILE=/path/to/schema.tosd \
TOSD_DOCS_DIR=/path/to/inputs \
TOSD_EXPECTED_VIOLATIONS=0 \
  fortran-fpm test
```

Files that fail to parse are skipped (fragments); `unexpected key`
diagnostics are ignored, mirroring a rules-only run against a partial schema;
`TOSD_EXPECTED_VIOLATIONS` (default 0) is the expected number of files with
RULE violations (missing / type / allowed / dependent / value).

## Measured toml-f API notes (2026-09-16)

Details that had to be read off the dependency source rather than guessed:

- `get_keys` is **type-bound**: `call table % get_keys(list)` with
  `type(toml_key), allocatable :: list(:)` (field `% key`).
- Array length is the `len` generic (`use tomlf, only: my_len => len`, to avoid
  shadowing the intrinsic); elements come from `call arr % get(j, ptr)`
  (subroutine form) or directly via `call get_value(arr, j, string)`.
- All `get_value` overloads on tables take the table as `intent(inout)` —
  this library therefore reads tables only through the `intent(in)`-safe
  `% get` / `% has_key` bindings plus `get_value` on `toml_keyval`/arrays.
- `toml_load(table, file, error=error)` — the error dummy must be passed by
  keyword (positionally it would bind to `config`).
- Scalar kinds are distinguished with `toml_keyval % get_type()` against
  `toml_constants:toml_type`, not by probing values.
- gfortran quirk: an array constructor `[path, name]` where `path` is an
  assumed-length (`character(*)`) dummy silently yields zero-length,
  content-less elements. All path extension goes through
  `tosd_extend_path` (scalar assignments into a `tosd_path_len` buffer).

## License

Dual-licensed `Apache-2.0 OR MIT` (`LICENSE-Apache`, `LICENSE-MIT`), the same
pair as toml-f.
