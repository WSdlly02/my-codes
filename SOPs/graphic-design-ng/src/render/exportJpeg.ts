import { createRenderCanvas, toHtmlCanvas } from "./canvas";
import { renderScene } from "./renderScene";
import type { PosterScene } from "../types/scene";

export async function renderPoster(scene: PosterScene): Promise<HTMLCanvasElement> {
  const canvas = createRenderCanvas(scene.width, scene.height);
  const rendered = await renderScene(canvas, scene);
  return toHtmlCanvas(rendered);
}

export async function exportPosterAsJpeg(
  scene: PosterScene,
  fileName = "poster-a3-300dpi.jpg",
  quality = 0.92,
): Promise<void> {
  const canvas = await renderPoster(scene);

  await new Promise<void>((resolve, reject) => {
    canvas.toBlob(
      (blob) => {
        if (!blob) {
          reject(new Error("Unable to create JPEG blob."));
          return;
        }

        const link = document.createElement("a");
        link.href = URL.createObjectURL(blob);
        link.download = fileName;
        link.click();
        URL.revokeObjectURL(link.href);
        resolve();
      },
      "image/jpeg",
      quality,
    );
  });
}
