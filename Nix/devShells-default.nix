{
  haskellPackages,
  inputs,
  mkShell,
  python312Packages,
  system,
}:

mkShell {
  packages = [
    (inputs.self.legacyPackages."${system}".haskellEnv.override {
      extraPackages = with haskellPackages; [
        # Libs
        JuicyPixels
        http-types
        warp
        mime-types
        websockets
      ];
    })
    (inputs.self.legacyPackages."${system}".python312Env.override {
      extraPackages =
        with python312Packages;
        with inputs.self.legacyPackages."${system}";
        [
          # Drivers from self
          mlx90460-driver.Adafruit-Blinka
          mlx90460-driver.adafruit-circuitpython-busdevice
          mlx90460-driver.adafruit-circuitpython-connectionmanager
          mlx90460-driver.adafruit-circuitpython-mlx90640
          mlx90460-driver.adafruit-circuitpython-requests
          mlx90460-driver.adafruit-circuitpython-typing
          mlx90460-driver.rpi-ws281x
          picamera2
          pidng
          simplejpeg
          v4l2-python3
          # Drivers from Nixpkgs
          adafruit-platformdetect
          adafruit-pureio
          av
          binho-host-adapter
          piexif
          pillow
          pyftdi
          pyserial
          python-prctl
          pyusb
          rpi-gpio
          sysv-ipc
          typing-extensions
          # Daily runtimes
          flask
          icalendar # For generating calendar files
          opencv4
        ];
      extraPostBuild =
        let
          my-codes-legacyPackages = inputs.self.legacyPackages."${system}";
        in
        ''
          ln -s ${my-codes-legacyPackages.libcamera}/bin/cam $out/bin/
          ln -s ${my-codes-legacyPackages.libcamera}/bin/libcamerify $out/bin/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/python3.12/site-packages/libcamera $out/lib/python3.12/site-packages/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/gstreamer-1.0 $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera-base.so $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera-base.so.0.3 $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera-base.so.0.3.1 $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera.so $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera.so.0.3 $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/lib/libcamera.so.0.3.1 $out/lib/
          ln -s ${my-codes-legacyPackages.libcamera}/libexec $out/
          ln -s ${my-codes-legacyPackages.libcamera}/share/libcamera $out/share/
          ln -s ${my-codes-legacyPackages.rpi-kms}/lib/python3.12/site-packages/pykms $out/lib/python3.12/site-packages/'';
    })
  ];
  shellHook = ''
    fish
  '';
}
