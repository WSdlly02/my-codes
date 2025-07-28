{
  buildPythonPackage,
  fetchPypi,
  setuptools,
}:
buildPythonPackage rec {
  pname = "pystun3";
  version = "2.0.0";
  pyproject = true;
  build-system = [ setuptools ];
  src = fetchPypi {
    inherit pname version;
    hash = "sha256-mav+g99p/Q9ieOm+FFaCf6+55OJe/tgHTRW3VKybH+4=";
  };
}
