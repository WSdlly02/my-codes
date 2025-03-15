{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "v4l2-python3";
  version = "0.3.5";
  # format = "wheel";
  src = fetchPypi rec {
    pname = "v4l2-python3";
    inherit version;
    # dist = python;
    # python = "py3";
    hash = "sha256-5+JHOcGBbWSoKSm4F4GtpOVkCXQVejlYWtLgLf2xAv0=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
