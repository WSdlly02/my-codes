{
  lib,
  python3Packages,
  fetchPypi,
}:
python3Packages.buildPythonPackage rec {
  pname = "v4l2";
  version = "0.2";
  # format = "wheel";
  src = fetchPypi rec {
    pname = "v4l2";
    inherit version;
    # dist = python;
    # python = "py3";
    hash = "sha256-DY8x+dVU3tTQtQoxp75VkLhh354bolbudX4cCRdd1KI=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
