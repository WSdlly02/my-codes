/*
  ── adafruit-circuitpython-mlx90640 [required: Any, installed: 1.3.4] √
  │   ├── Adafruit-Blinka [required: Any, installed: 8.55.0] √
  │   │   ├── adafruit-circuitpython-typing [required: Any, installed: 1.11.2] √
  │   │   │   ├── adafruit-circuitpython-busdevice [required: Any, installed: 5.2.11] √
  │   │   │   ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10] √
  │   │   │   │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3] √
  │   │   │   └── typing_extensions [required: ~=4.0, installed: 4.12.2] √ in Nixpkgs
  │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0] √ in Nixpkgs
  │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11] √ in Nixpkgs
  │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6] √ in Nixpkgs
  │   │   │   └── pyserial [required: Any, installed: 3.5] √ in Nixpkgs
  │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0] √ in Nixpkgs
  │   │   │   ├── pyserial [required: >=3.0, installed: 3.5] √ in Nixpkgs
  │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1] √ in Nixpkgs
  │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1] √ in Nixpkgs
  │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0] √
  │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0] √ in Nixpkgs
  │   ├── adafruit-circuitpython-busdevice [required: Any, installed: 5.2.11] √
  │   │   ├── Adafruit-Blinka [required: >=7.0.0, installed: 8.55.0] √
  │   │   │   ├── adafruit-circuitpython-typing [required: Any, installed: 1.11.2] √
  │   │   │   │   ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10] √
  │   │   │   │   │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3] √
  │   │   │   │   └── typing_extensions [required: ~=4.0, installed: 4.12.2] √ in Nixpkgs
  │   │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0] √ in Nixpkgs
  │   │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11] √ in Nixpkgs
  │   │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6] √ in Nixpkgs
  │   │   │   │   └── pyserial [required: Any, installed: 3.5] √ in Nixpkgs
  │   │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0] √ in Nixpkgs
  │   │   │   │   ├── pyserial [required: >=3.0, installed: 3.5] √ in Nixpkgs
  │   │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1] √ in Nixpkgs
  │   │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1] √ in Nixpkgs
  │   │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0] √
  │   │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0] √ in Nixpkgs
  │   │   └── adafruit-circuitpython-typing [required: Any, installed: 1.11.2]
  │   │       ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │   │       │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │   │       │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │   │       │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │   │       │   │   └── pyserial [required: Any, installed: 3.5]
  │   │       │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │   │       │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │   │       │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │   │       │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │   │       │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │   │       │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │   │       ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10]
  │   │       │   ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │   │       │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │   │       │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │   │       │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │   │       │   │   │   └── pyserial [required: Any, installed: 3.5]
  │   │       │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │   │       │   │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │   │       │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │   │       │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │   │       │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │   │       │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │   │       │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3]
  │   │       │       └── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │   │       │           ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │   │       │           ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │   │       │           ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │   │       │           │   └── pyserial [required: Any, installed: 3.5]
  │   │       │           ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │   │       │           │   ├── pyserial [required: >=3.0, installed: 3.5]
  │   │       │           │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │   │       │           ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │   │       │           ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │   │       │           └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │   │       └── typing_extensions [required: ~=4.0, installed: 4.12.2]
  │   └── adafruit-circuitpython-register [required: Any, installed: 1.10.2]
  │       ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │   ├── adafruit-circuitpython-typing [required: Any, installed: 1.11.2]
  │       │   │   ├── adafruit-circuitpython-busdevice [required: Any, installed: 5.2.11]
  │       │   │   ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10]
  │       │   │   │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3]
  │       │   │   └── typing_extensions [required: ~=4.0, installed: 4.12.2]
  │       │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │   │   └── pyserial [required: Any, installed: 3.5]
  │       │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       ├── adafruit-circuitpython-busdevice [required: Any, installed: 5.2.11]
  │       │   ├── Adafruit-Blinka [required: >=7.0.0, installed: 8.55.0]
  │       │   │   ├── adafruit-circuitpython-typing [required: Any, installed: 1.11.2]
  │       │   │   │   ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10]
  │       │   │   │   │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3]
  │       │   │   │   └── typing_extensions [required: ~=4.0, installed: 4.12.2]
  │       │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │   │   │   └── pyserial [required: Any, installed: 3.5]
  │       │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │   │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │   └── adafruit-circuitpython-typing [required: Any, installed: 1.11.2]
  │       │       ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │       │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │       │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │       │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │       │   │   └── pyserial [required: Any, installed: 3.5]
  │       │       │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │       │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │       │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │       │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │       │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │       │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │       ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10]
  │       │       │   ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │       │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │       │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │       │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │       │   │   │   └── pyserial [required: Any, installed: 3.5]
  │       │       │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │       │   │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │       │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │       │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │       │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │       │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │       │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3]
  │       │       │       └── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │       │           ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │       │           ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │       │           ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │       │           │   └── pyserial [required: Any, installed: 3.5]
  │       │       │           ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │       │           │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │       │           │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │       │           ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │       │           ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │       │           └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │       └── typing_extensions [required: ~=4.0, installed: 4.12.2]
  │       ├── adafruit-circuitpython-typing [required: >=1.3.1,==1.*, installed: 1.11.2]
  │       │   ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │   │   │   └── pyserial [required: Any, installed: 3.5]
  │       │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │   │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │   ├── adafruit-circuitpython-busdevice [required: Any, installed: 5.2.11]
  │       │   │   ├── Adafruit-Blinka [required: >=7.0.0, installed: 8.55.0]
  │       │   │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │   │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │   │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │   │   │   │   └── pyserial [required: Any, installed: 3.5]
  │       │   │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │   │   │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │   │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │   │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │   │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │   │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │   ├── adafruit-circuitpython-requests [required: Any, installed: 4.1.10]
  │       │   │   ├── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │   │   │   ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │   │   │   ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │   │   │   ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │   │   │   │   └── pyserial [required: Any, installed: 3.5]
  │       │   │   │   ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │   │   │   │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │   │   │   │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │   │   │   ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │   │   │   ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │   │   │   └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │   │   └── adafruit-circuitpython-connectionmanager [required: Any, installed: 3.1.3]
  │       │   │       └── Adafruit-Blinka [required: Any, installed: 8.55.0]
  │       │   │           ├── Adafruit-PlatformDetect [required: >=3.70.1, installed: 3.77.0]
  │       │   │           ├── Adafruit-PureIO [required: >=1.1.7, installed: 1.1.11]
  │       │   │           ├── binho-host-adapter [required: >=0.1.6, installed: 0.1.6]
  │       │   │           │   └── pyserial [required: Any, installed: 3.5]
  │       │   │           ├── pyftdi [required: >=0.40.0, installed: 0.56.0]
  │       │   │           │   ├── pyserial [required: >=3.0, installed: 3.5]
  │       │   │           │   └── pyusb [required: >=1.0.0,!=1.2.0, installed: 1.3.1]
  │       │   │           ├── RPi.GPIO [required: Any, installed: 0.7.1]
  │       │   │           ├── rpi-ws281x [required: >=4.0.0, installed: 5.0.0]
  │       │   │           └── sysv-ipc [required: >=1.1.0, installed: 1.1.0]
  │       │   └── typing_extensions [required: ~=4.0, installed: 4.12.2]
  │       └── typing_extensions [required: ~=4.0, installed: 4.12.2]
*/

{
  lib,
  python3Packages,
  fetchPypi,
}:
python3Packages.buildPythonPackage rec {
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
