{
  lib,
  buildPythonPackage,
  fetchPypi,
  setuptools,
  numpy,
  typing-extensions,
  pkgs,
}:
buildPythonPackage rec {
  pname = "sox";
  version = "1.5.0";
  pyproject = true;

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-Ese+W7H1SNiR/hHoLAjPXxoddOIlKY9gCC5a6yRpraA=";
  };

  build-system = [ setuptools ];
  dependencies = [ numpy typing-extensions ];
  nativeCheckInputs = [ pkgs.sox ];
  pythonImportsCheck = [ "sox" ];

  meta = {
    description = "Python interface to the SoX command line tools";
    homepage = "https://github.com/marl/pysox";
    license = lib.licenses.bsd3;
  };
}
