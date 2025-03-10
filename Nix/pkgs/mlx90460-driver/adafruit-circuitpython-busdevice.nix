{
  lib,
  python3Packages,
  fetchPypi,
}:
python3Packages.buildPythonPackage rec {
  pname = "adafruit-circuitpython-busdevice";
  version = "5.2.11";
  format = "wheel";
  src = fetchPypi rec {
    pname = "adafruit_circuitpython_busdevice";
    inherit version format;
    dist = python;
    python = "py3";
    hash = "sha256-1DecmuhqFfcETeqBWpRSXKntpqfAsvoOdc+ecAyThLg=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
