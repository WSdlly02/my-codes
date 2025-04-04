{
  extraPackages ? [ ],
  haskellPackages,
}:
haskellPackages.ghcWithPackages (
  haskellPackages:
  with haskellPackages;
  [
    cabal-install
    fourmolu # Formatter
    haskell-language-server
    stack
  ]
  ++ extraPackages
)
