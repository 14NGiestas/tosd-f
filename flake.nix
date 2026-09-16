{
  description = "tosd-f — TOML Schema (.tosd) validation in pure Fortran";

  inputs = {
    # Floating, like the CheesyHam dev shell: keeps the same gfortran /
    # fortran-fpm family the library is validated against.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Wrapper: provide `fpm` pointing at fortran-fpm (a bare `fpm` on
        # PATH is often an unrelated tool and must not be used here).
        fpmAlias = pkgs.writeShellScriptBin "fpm" ''
          exec ${pkgs.fortran-fpm}/bin/fortran-fpm "$@"
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.gfortran
            pkgs.fortran-fpm
            fpmAlias
          ];
          shellHook = ''
            export FPM_FC="${pkgs.gfortran}/bin/gfortran"
          '';
        };
      }
    );
}
