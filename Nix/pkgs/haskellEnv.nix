{
  extraPackages ? [ ],
  haskellPackages,
}:
haskellPackages.ghcWithPackages (
  haskellPackages:
  with haskellPackages;
  [
    # Package manager
    cabal-install
    stack
    ormolu # Formatter
  ]
  ++ extraPackages
)
