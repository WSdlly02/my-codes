{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "adafruit-circuitpython-requests";
  version = "4.1.10";
  format = "wheel";
  src = fetchPypi rec {
    pname = "adafruit_circuitpython_requests";
    inherit version format;
    dist = python;
    python = "py3";
    hash = "sha256-alSqOUNug+DcddYMhh5b76Mzv/JluUfqJjFW5k5GcSI=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
