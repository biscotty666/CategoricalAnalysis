{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/25.11";
  inputs.systems.url = "github:nix-systems/default";
  inputs.flake-utils = {
    url = "github:numtide/flake-utils";
    inputs.systems.follows = "systems";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            R
            quarto
            chromium
            pandoc
            texlive.combined.scheme-full
            rstudio
            (with rPackages; [
              quarto
              pagedown
              styler
              AmesHousing
              DescTools
              Hmisc
              ROCR
              ROCit
              VGAM
              car
              ggeffects
              givitiR
              gmodels
              lmtest
              mgcv
              sandwich
              tidyverse
              vcd
              vcdExtra
              visreg
            ])
          ];
        };
      }
    );
}
