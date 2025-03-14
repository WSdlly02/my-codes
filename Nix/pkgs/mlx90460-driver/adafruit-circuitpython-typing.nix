{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "adafruit-circuitpython-typing";
  version = "1.11.2";
  format = "wheel";
  src = fetchPypi rec {
    pname = "adafruit_circuitpython_typing";
    inherit version format;
    dist = python;
    python = "py3";
    hash = "sha256-4UAaCbv99n5Dh1zGdVs68O2oOBsSycj3Wb12drdCXhw=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
