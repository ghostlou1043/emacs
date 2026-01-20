{
  description = "Python environment for emacs org-babel";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.python312
          # pkgs.python311Packages.pip
          # pkgs.python311Packages.virtualenv
        ];
      };
    };
}
