{
  lib,
  python312Packages,
  fetchPypi,
}:
python312Packages.buildPythonPackage rec {
  pname = "rpi-ws281x";
  version = "5.0.0";
  src = fetchPypi rec {
    pname = "rpi_ws281x";
    inherit version;
    hash = "sha256-AM5tt3FDa3eNCTAkXPjqKq4RAIzF/WfVd4nFQirz7lU=";
  };

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
