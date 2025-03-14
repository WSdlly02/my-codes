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
  libcamera,
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
    ln -s ${libcamera}/lib/python3.12/site-packages/libcamera $out/lib/python3.12/site-packages/
  '';
}
