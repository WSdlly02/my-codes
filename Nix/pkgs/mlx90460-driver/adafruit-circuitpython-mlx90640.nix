{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "adafruit-circuitpython-mlx90640";
  version = "1.3.4";
  format = "wheel";
  src = fetchPypi rec {
    pname = "adafruit_circuitpython_mlx90640";
    inherit version format;
    dist = python;
    python = "py3";
    hash = "sha256-Jwlqmy+02cXUaGxINZqe7bRpCoUeu++eRLAaUpF+5xI=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
