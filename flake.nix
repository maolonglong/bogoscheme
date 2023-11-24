{
  description = "OCaml learn";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;
      in {
        devShells.default = pkgs.mkShell {
          # build tools
          nativeBuildInputs =
            (with pkgs; [
              fswatch
              rlwrap
            ])
            ++ (with ocamlPackages; [
              ocaml
              findlib
              dune_3
              ocaml-lsp
              ocamlformat
            ]);
          # dependencies
          buildInputs = with ocamlPackages; [
            menhir
            ounit2
          ];
        };
      }
    );
}
