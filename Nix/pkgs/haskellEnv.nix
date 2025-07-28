{
  extraPackages ? f: [ ],
  ghcWithPackages,
}:
ghcWithPackages (
  f:
  with f;
  [
    cabal-gild
    cabal-install
    fourmolu # Formatter
    haskell-language-server
    stack
  ]
  ++ (extraPackages f)
)
