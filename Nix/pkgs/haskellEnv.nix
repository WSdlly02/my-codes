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
    stack
  ]
  ++ extraPackages
)
