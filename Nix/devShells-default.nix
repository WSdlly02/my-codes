{
  haskellEnv,
  mkShell,
  python3Env,
}:

mkShell {
  packages = [
    (haskellEnv.override {
      extraPackages =
        f: with f; [
          # Libs
          JuicyPixels
          http-types
          warp
          mime-types
          websockets
        ];
    })
    (python3Env.override {
      extraPackages =
        f: with f; [
          # Daily runtimes
          flask
          icalendar # For generating calendar files
          opencv4
          ultralytics # YOLO
        ];
    })
  ];
  shellHook = ''
    fish
  '';
}
