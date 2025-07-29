/*
  Notice:
  <nixpkgs/pkgs/top-level/all-packages.nix>:
  inherit (pythonInterpreters) python313;
  python3 = python313;
  python3Packages = dontRecurseIntoAttrs python313Packages; # same as above
  python313Packages = recurseIntoAttrs python313.pkgs; # same as above
  some packages requires old python-modules will specific {python3 = python311;} in args

  <nixpkgs/pkgs/development/interpreters/python>:
  python313 = {
    self = __splicedPackages.python313;
    inherit passthruFun; # withPackages,pkgs,...
  };

  <nixpkgs/pkgs/development/interpreters/passthrufun.nix>:
  buildEnv = callPackage ./wrapper.nix {
    python = self;
    inherit (pythonPackages) requiredPythonModules;
  };
  withPackages = import ./with-packages.nix { inherit buildEnv pythonPackages; }; which is defined in <nixpkgs/pkgs/development/interpreters/python/passthrufun.nix>
  pkgs = pythonPackages; # same as above

  virtualenv = with python3Packages; toPythonApplication virtualenv; # virtualenv = python3Packages.virtualenv
  "toPythonApplication" converts a Python library to an application, which is defined in <nixpkgs/pkgs/development/interpreters/python/python-packages-base.nix>

  python3.buildEnv.override {
    extraLibs = [ (f pythonPackages) ];
  }

  is equals to

  python3.withPackages (f: with f; [ # It will pass pythonPackages as this func's arg
    numpy
    requests
  ])
*/
{
  extraPackages ? f: [ ], # For devShells
  python, # generic python parameter, equals to python3Packages.python
}:
python.withPackages (
  # it will filter packages with attribute "pythonModule"
  f: # f <- python3Packages
  with f;
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
  ++ (extraPackages f)
)
