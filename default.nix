let pkgs = import <nixpkgs> {};
    hp  = pkgs.haskellPackages.override { 
      overrides = self: super: {
        utc = pkgs.haskell.lib.appendPatch super.utc (pkgs.fetchpatch {
          url = "https://github.com/lpeterse/haskell-utc/pull/3.patch";
          sha256 = "0v6kv1d4syjzgzc2s7a76c6k4vminlcq62n7jg3nn9xd00gwmmv7"; 
        }); 
      }; 
    };
in hp.callPackage ./ccchan.nix { }
