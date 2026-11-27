{
  lib,
  buildPythonPackage,
  fetchPypi,
  setuptools,
  wheel,
  accelerate,
  einops,
  gradio,
  librosa,
  onnxruntime,
  pysox,
  soundfile,
  torch,
  torchaudio,
  transformers,
  soxCli,
  writableTmpDirAsHomeHook,
}:
buildPythonPackage rec {
  pname = "qwen-tts";
  version = "0.1.1";
  pyproject = true;

  src = fetchPypi {
    pname = "qwen_tts";
    inherit version;
    hash = "sha256-r7pfojWAamiD9Go4nmdUC0b4pV2kVyFr8dc0KQOBR4A=";
  };

  # BooleanOptionalAction generates the negative spelling itself.
  postPatch = ''
    substituteInPlace qwen_tts/cli/demo.py \
      --replace-fail '"--flash-attn/--no-flash-attn"' '"--flash-attn"'
  '';

  build-system = [ setuptools wheel ];
  dependencies = [
    accelerate
    einops
    gradio
    librosa
    onnxruntime
    pysox
    soundfile
    torch
    torchaudio
    transformers
  ];

  makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ soxCli ]) ];
  nativeCheckInputs = [ soxCli writableTmpDirAsHomeHook ];
  pythonImportsCheck = [ "qwen_tts" "qwen_tts.cli.demo" ];

  meta = {
    description = "Qwen3 text-to-speech inference library and local demo";
    homepage = "https://github.com/QwenLM/Qwen3-TTS";
    license = lib.licenses.asl20;
    mainProgram = "qwen-tts-demo";
    platforms = [ "x86_64-linux" ];
  };
}
