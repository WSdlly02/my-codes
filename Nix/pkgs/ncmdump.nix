{
  cmake,
  taglib,
  zlib,
  fetchFromGitHub,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "ncmdump";
  version = "1.5.1";
  src = fetchFromGitHub {
    owner = "taurusxin";
    repo = pname;
    rev = version;
    hash = "sha256-OU5jfZj/2S2bVyguPfvGH2DR5ZgszH6mSbJiQgrhGU8=";
  };
  buildInputs = [
    taglib
    zlib
  ];
  nativeBuildInputs = [ cmake ];
}
