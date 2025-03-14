{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "picamera2";
  version = "0.3.25";
  # format = "wheel";
  src = fetchPypi rec {
    pname = "picamera2";
    inherit version;
    # dist = python;
    # python = "py3";
    hash = "sha256-ys88QLgkDwuAICac6Wa6kRxw/ph8Sivy+5ac4010Zpo=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
