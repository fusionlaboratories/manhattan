{ system ? builtins.currentSystem,
  pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/addae2044494e1fd3acf5c1684f4fc967406961d.tar.gz";
    sha256 = "sha256:1jxz25hk4c63ji0gzzp09ci3z2pj9577jb3bpv3a6h81wflzd87b";
  }) { inherit system; }
}:
# let
#     haskellPackages = pkgs.haskell.packages.ghc926.override {
#         overrides = self: super: {
#             wasm = pkgs.haskell.lib.dontCheck (self.callHackage "wasm" "1.1.1" {});
#         };
#     };

    # ghc = haskellPackages.ghcWithPackages;
    # ghc = pkgs.haskell.packages.ghc926;
    # python = pkgs.python39;
    
# in
    # { inherit pkgs haskellPackages ghc; }

pkgs.mkShell {
    name = "manhattan-dev";
    buildInputs = [
        pkgs.cabal-install
        
        pkgs.ghc
        pkgs.zlib
        pkgs.gnupg
        # ghc
    ];
}