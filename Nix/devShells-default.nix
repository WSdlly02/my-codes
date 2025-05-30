{
  haskellEnv,
  haskellPackages,
  inputs,
  mkShell,
  python312Env,
  python312Packages,
  stdenv,
}:

mkShell {
  packages = [
    (haskellEnv.override {
      extraPackages = with haskellPackages; [
        # Libs
        JuicyPixels
        http-types
        warp
        mime-types
        websockets
      ];
    })
    (python312Env.override {
      extraPackages =
        with python312Packages;
        with inputs.self.legacyPackages."${stdenv.hostPlatform.system}";
        [
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
