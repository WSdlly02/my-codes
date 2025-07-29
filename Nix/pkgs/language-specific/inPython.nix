{ lib, writeShellScriptBin }:
lib.genAttrs
  [
    "class-schedule"
    "duplicated-file-searcher"
    "fibonacci"
    "project-routine-scheduler"
    "roots-resolver"
    "teaching-week-reminder"
  ]
  (
    packageName:
    writeShellScriptBin "${packageName}-wrapper" ''
      python3 $MY_CODES_PATH/Python/${packageName}.py $@
    ''
  )
