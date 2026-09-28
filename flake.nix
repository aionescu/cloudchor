{
  inputs.nixpkgs.url = "https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.zst";

  outputs = { nixpkgs, ... }: let
    inherit (nixpkgs) lib;
    forEachSystem = f: lib.foldl' lib.recursiveUpdate { } (lib.forEach lib.systems.flakeExposed (system:
      lib.mapAttrs (_: outputs: { ${system} = outputs; }) (f nixpkgs.legacyPackages.${system})
    ));
  in forEachSystem (pkgs: let
    inherit (pkgs.haskellPackages) callCabal2nix;
  in {
    packages = rec {
      cloudchor = callCabal2nix "cloudchor" ./. { };
      cloudchor-examples = callCabal2nix "cloudchor-examples" ./examples { inherit cloudchor; };
      cloudchor-benchmark = callCabal2nix "cloudchor-benchmark" ./benchmark { inherit cloudchor; };

      default = cloudchor;
    };
  });
}
