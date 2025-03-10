{
  lib,
  python3Packages,
  fetchPypi,
}:
python3Packages.buildPythonPackage rec {
  pname = "adafruit-circuitpython-connectionmanager";
  version = "3.1.3";
  format = "wheel";
  src = fetchPypi rec {
    pname = "adafruit_circuitpython_connectionmanager";
    inherit version format;
    dist = python;
    python = "py3";
    hash = "sha256-nfOkxhfa4nutGshgfxoIQxLISY2DHr4caiyNXLMJ2uo=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
