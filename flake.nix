{
  description = "r5rs-hs: a bare-bones, embeddable R5RS Scheme reader";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hsPkgs = pkgs.haskell.packages.ghc910.override {
          overrides = hfinal: hprev: { };
        };

        r5rs-hs = hsPkgs.callCabal2nix "r5rs-hs" ./. { };
      in
      {
        packages.default = r5rs-hs;

        apps.default = flake-utils.lib.mkApp {
          drv = r5rs-hs;
          exePath = "/bin/r5rs-hs-parse";
        };

        devShells.default = hsPkgs.shellFor {
          packages = _: [ r5rs-hs ];
          withHoogle = true;
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            hsPkgs.fourmolu
          ];
        };

        checks = {
          build = r5rs-hs;
          hlint = pkgs.runCommand "hlint" { nativeBuildInputs = [ hsPkgs.hlint ]; } ''
            hlint ${./.}/src ${./.}/app ${./.}/test
            touch $out
          '';
        };

        formatter = pkgs.nixfmt-rfc-style;
      });
}
