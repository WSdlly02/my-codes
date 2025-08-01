/*
  Notice:
  <nixpkgs/pkgs/top-level/all-packages.nix>:
  haskell = callPackage ./haskell-packages.nix { };
  haskellPackages =
    dontRecurseIntoAttrs
      (
        if stdenv.hostPlatform.isStatic then
          haskell.packages.native-bignum.ghc94
        # JS backend can't use gmp
        else if stdenv.hostPlatform.isGhcjs then
          haskell.packages.native-bignum.ghc98
        else if stdenv.hostPlatform.isi686 then
          haskell.packages.ghc96
        else
          haskell.packages.ghc98
      )
  <nixpkgs/pkgs/top-level/haskell-modules.nix>:
  compiler = ...
  packages = ...

  <nixpkgs/pkgs/development/haskell-modules/make-package-set.nix>:
  ghcWithPackages = buildHaskellPackages.callPackage ./with-packages-wrapper.nix {
    haskellPackages = self;
    inherit (self) hoogleWithPackages;
  };
*/
{
  extraPackages ? f: [ ],
  ghcWithPackages,
}:
ghcWithPackages (
  # it will filter packages with attribute "isHaskellLibrary"
  f: # f <- haskellPackges
  with f;
  [
    cabal-gild
    cabal-install
    haskell-language-server
  ]
  ++ (extraPackages f)
)
