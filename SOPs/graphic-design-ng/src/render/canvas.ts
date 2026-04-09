export type RenderCanvas = HTMLCanvasElement | OffscreenCanvas;
export type RenderContext2D = CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;

export function createRenderCanvas(width: number, height: number): RenderCanvas {
  if (typeof OffscreenCanvas !== "undefined") {
    return new OffscreenCanvas(width, height);
  }

  const canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  return canvas;
}

export function get2DContext(canvas: RenderCanvas): RenderContext2D {
  const context = canvas.getContext("2d");

  if (!context) {
    throw new Error("2D context is unavailable.");
  }

  return context;
}

export function toHtmlCanvas(canvas: RenderCanvas): HTMLCanvasElement {
  if (canvas instanceof HTMLCanvasElement) {
    return canvas;
  }

  const htmlCanvas = document.createElement("canvas");
  htmlCanvas.width = canvas.width;
  htmlCanvas.height = canvas.height;

  const context = htmlCanvas.getContext("2d");
  if (!context) {
    throw new Error("Unable to copy OffscreenCanvas content.");
  }

  context.drawImage(canvas, 0, 0);
  return htmlCanvas;
}
