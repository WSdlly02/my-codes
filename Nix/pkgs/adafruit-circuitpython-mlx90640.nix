{
  lib,
  python3Packages,
  fetchPypi,
}:
python3Packages.buildPythonPackage rec {
  pname = "adafruit-circuitpython-mlx90640";
  version = "1.3.4";
  format = "pyproject";
  src = fetchPypi {
    pname = "adafruit_circuitpython_mlx90640";
    inherit version;
    hash = "sha256-FKLOShMTJ/QJrYQ92okRO5FcrjTarCYA6hgImWN5Ozo=";
  };
  propagatedBuildInputs = with python3Packages; [
    setuptools
    setuptools-scm
  ];
  doCheck = false;

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
