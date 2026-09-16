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

## Dependencies

`fpm.toml` pulls the upstream [`toml-f`](https://github.com/toml-f/toml-f)
(parsing) and `test-drive` (tests) automatically. Validated against toml-f
0.5.x — see the API notes below if you hit something odd at that boundary.

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

Introspection: `schema % dump(unit)` writes one `path : kind` line per declared
element (plus `dependentrequired` rules) — the debugging companion to
`unexpected key` diagnostics.

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

## Notes on the toml-f API (validated against 0.5.x)

Non-obvious points at the dependency boundary:

- `get_keys` is **type-bound**: `call table % get_keys(list)` with
  `type(toml_key), allocatable :: list(:)` (field `% key`).
- Array length is the `len` generic (`use tomlf, only: tosd_len => len`, to avoid
  shadowing the intrinsic); elements come from `call arr % get(j, ptr)`
  (subroutine form) or directly via `call get_value(arr, j, string)`.
- In 0.5.x **every** table/array read binding (`% get`, `% get_keys`, `% has_key`,
  all `get_value` table/array overloads) takes its object as `intent(inout)`.
  Consequently this library's table dummies — including the public
  `tosd_validate(schema, doc, errors)` document — are `intent(inout)`.
  Only `toml_keyval` getters and the `len` generic stay `intent(in)`-safe.
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
