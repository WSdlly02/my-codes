{
  cmake,
  fetchFromGitHub,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "ncmdump";
  version = "1.5.0";
  src = fetchFromGitHub {
    owner = "taurusxin";
    repo = pname;
    rev = version;
    fetchSubmodules = true;
    hash = "sha256-aKMcuApvnPrRpTDCd83vNWYrB8rTU/5Mgyhzal8n61A=";
  };
  nativeBuildInputs = [ cmake ];
  cmakeFlags = [ "-DCMAKE_BUILD_TYPE=Release" ];
}
