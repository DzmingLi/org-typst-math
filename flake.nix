{
  description = "Org mathematics with Typst and a persistent Rust helper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      packages = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.rustPlatform.buildRustPackage {
            pname = "org-typst-math-helper";
            version = "0.1.0";
            src = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [ ./Cargo.toml ./Cargo.lock ./helper ];
            };
            cargoLock.lockFile = ./Cargo.lock;
            doCheck = false;
          };
          elisp = pkgs.runCommand "org-typst-elisp-packages" {
            nativeBuildInputs = [ pkgs.gnutar ];
          } ''
            mkdir -p "$out"
            for name in org-typst-math org-fragtog-plus; do
              source=${./lisp}/$name.el
              version=$(sed -n 's/^;; Version: //p' "$source")
              dependencies=$(sed -n 's/^;; Package-Requires: //p' "$source")
              directory="$name-$version"
              mkdir "$directory"
              cp ${./lisp}/$name*.el "$directory/"
              if [ "$name" = org-typst-math ]; then
                cp ${./lisp}/typst-client.el "$directory/"
              fi
              cat > "$directory/$name-pkg.el" <<EOF
            ;;; -*- no-byte-compile: t; lexical-binding: t; -*-
            (define-package "$name" "$version" "$name" '$dependencies)
            EOF
              tar --sort=name --mtime=@1 --owner=0 --group=0 --numeric-owner \
                -cf "$out/$directory.tar" "$directory"
            done
          '';
        });
      devShells = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cargo rustc rustfmt clippy
              ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.ox-typst ]))
              typst git
            ];
            shellHook = ''
              export PATH="$PWD/target/debug:$PATH"
            '';
          };
        });
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };
}
