{
  extraPackages ? [ ],
  haskellPackages,
}:
haskellPackages.ghcWithPackages (
  haskellPackages:
  with haskellPackages;
  [
    cabal-gild
    cabal-install
    fourmolu # Formatter
    haskell-language-server
    stack
  ]
  ++ extraPackages
)
