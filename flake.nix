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
        });
      devShells = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cargo rustc rustfmt clippy
              ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.ox-typst ]))
              typst python3 git
            ];
            shellHook = ''
              export PATH="$PWD/target/debug:$PATH"
            '';
          };
        });
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };
}
