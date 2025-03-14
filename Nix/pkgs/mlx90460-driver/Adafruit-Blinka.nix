{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "Adafruit-Blinka";
  version = "8.55.0";
  format = "wheel";
  src = fetchPypi rec {
    pname = "adafruit_blinka";
    inherit version format;
    dist = python;
    python = "py3";
    hash = "sha256-GTJfTmpzQxe0cZ/e6+cZia0wKgkFFYWEH9Af912gZJg=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
