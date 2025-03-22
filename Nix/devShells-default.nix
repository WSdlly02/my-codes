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
