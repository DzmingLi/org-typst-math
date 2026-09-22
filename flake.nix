{
  description = "Org mathematics with Typst and a persistent Rust helper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.cargo2nix.url = "github:cargo2nix/cargo2nix";
  inputs.cargo2nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    {
      self,
      nixpkgs,
      cargo2nix,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ cargo2nix.overlays.default ];
          };
          rustToolchain = pkgs.symlinkJoin {
            name = "rust-toolchain";
            inherit (pkgs.rustc) version;
            paths = [
              pkgs.rustc
              pkgs.cargo
            ];
          };
          rustPkgs = pkgs.rustBuilder.makePackageSet {
            inherit rustToolchain;
            packageFun = import ./Cargo.nix;
            workspaceSrc = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [
                ./Cargo.toml
                ./Cargo.lock
                ./helper
              ];
            };
          };
        in
        {
          default = rustPkgs.workspace.org-typst-math-helper { };
          cargo2nix =
            (pkgs.rustBuilder.makePackageSet {
              inherit rustToolchain;
              packageFun = import "${cargo2nix}/Cargo.nix";
              workspaceSrc = cargo2nix;
            }).workspace.cargo2nix
              { };
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
              self.packages.${system}.cargo2nix
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
