{
  description = "Org mathematics with Typst and a persistent Rust helper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          cargoNix = pkgs.callPackage ./Cargo.nix { };
        in
        {
          default = cargoNix.rootCrate.build;
          elisp =
            pkgs.runCommand "org-typst-elisp-packages"
              {
                nativeBuildInputs = [ pkgs.gnutar ];
              }
              ''
                mkdir -p "$out"
                name=org-typst-math
                source=${./lisp}/$name.el
                version=$(sed -n 's/^;; Version: //p' "$source")
                dependencies=$(sed -n 's/^;; Package-Requires: //p' "$source")
                directory="$name-$version"
                mkdir "$directory"
                cp ${./lisp}/*.el "$directory/"
                cat > "$directory/$name-pkg.el" <<EOF
                ;;; -*- no-byte-compile: t; lexical-binding: t; -*-
                (define-package "$name" "$version" "$name" '$dependencies)
                EOF
                tar --sort=name --mtime=@1 --owner=0 --group=0 --numeric-owner \
                  -cf "$out/$directory.tar" "$directory"
              '';
        }
      );
      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cargo
              rustc
              rustfmt
              clippy
              crate2nix
              ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.ox-typst ]))
              typst
              git
            ];
            shellHook = ''
              export PATH="$PWD/target/debug:$PATH"
            '';
          };
        }
      );
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };
}
