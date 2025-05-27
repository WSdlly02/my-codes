{
  mkShell,
  python312Env,
  python312Packages,
}:

mkShell {
  packages = [
    (python312Env.override {
      extraPackages = with python312Packages; [
        torch
        torchsde
        torchvision
        torchaudio
        einops
        transformers
        tokenizers
        sentencepiece
        safetensors
        aiohttp
        yarl
        pyyaml
        pillow
        tqdm
        kornia
        soundfile
        av
        pydantic
      ];
    })
  ];
  shellHook = ''
    fish
  '';
}
