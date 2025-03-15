/*
  Notice:

  python3 = python312; # which is defined in <nixpkgs/pkgs/top-level/all-packages.nix>
  python3Packages = dontRecurseIntoAttrs python312Packages; # same as above
  python312Packages = recurseIntoAttrs python312.pkgs; # same as above
  some packages requires old python-modules will specific {python3 = python311;} in args

  buildEnv = callPackage ./wrapper.nix {
    python = self;
    inherit (pythonPackages) requiredPythonModules;
  };
  withPackages = import ./with-packages.nix { inherit buildEnv pythonPackages; }; which is defined in <nixpkgs/pkgs/development/interpreters/python/passthrufun.nix>
  pkgs = pythonPackages; # same as above

  virtualenv = with python3Packages; toPythonApplication virtualenv; # virtualenv = python3Packages.virtualenv
  "toPythonApplication" converts a Python library to an application, which is defined in <nixpkgs/pkgs/development/interpreters/python/python-packages-base.nix>

  python3.buildEnv.override {
    extraLibs = [ python3Packages.pyramid ];
    ignoreCollisions = true;
  }

  is equals to

  python3.withPackages (ps: with ps; [
    numpy
    requests
  ])
*/
{
  extraPackages ? [ ],
  inputs,
  python312,
  python312Packages,
  system,
}:
python312.buildEnv.override {
  extraLibs =
    with python312Packages;
    [
      numpy
      pandas
      psutil
      requests
      scikit-learn
      scipy
      scrapy
      sympy
      virtualenv
    ]
    ++ extraPackages;
  postBuild = ''
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/bin/cam $out/bin/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/bin/libcamerify $out/bin/
    ln -s ${
      inputs.self.legacyPackages."${system}".libcamera
    }/lib/python3.12/site-packages/libcamera $out/lib/python3.12/site-packages/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/gstreamer-1.0 $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera-base.so $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera-base.so.0.3 $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera-base.so.0.3.1 $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera.so $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera.so.0.3 $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/lib/libcamera.so.0.3.1 $out/lib/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/libexec $out/
    ln -s ${inputs.self.legacyPackages."${system}".libcamera}/share/libcamera $out/share/
    ln -s ${
      inputs.self.legacyPackages."${system}".rpi-kms
    }/lib/python3.12/site-packages/pykms $out/lib/python3.12/site-packages/
  '';
}
