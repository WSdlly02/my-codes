import { get2DContext, type RenderCanvas, type RenderContext2D } from "./canvas";
import { loadImage } from "./image-cache";
import { renderMeshNode } from "./render3d";
import type { ImageNode, PosterScene, RectNode, SceneNode, TextNode } from "../types/scene";

function applyNodeTransform(
  context: RenderContext2D,
  node: SceneNode,
  draw: () => Promise<void> | void,
): Promise<void> | void {
  context.save();
  context.globalAlpha = node.opacity ?? 1;

  const centerX = node.x + node.width / 2;
  const centerY = node.y + node.height / 2;
  context.translate(centerX, centerY);
  context.rotate(((node.rotation ?? 0) * Math.PI) / 180);
  context.translate(-centerX, -centerY);

  const result = draw();
  if (result instanceof Promise) {
    return result.finally(() => context.restore());
  }

  context.restore();
}

function renderRect(context: RenderContext2D, node: RectNode): void {
  const radius = node.radius ?? 0;
  context.beginPath();
  context.roundRect(node.x, node.y, node.width, node.height, radius);
  context.fillStyle = node.fill;
  context.fill();

  if (node.stroke && node.strokeWidth) {
    context.strokeStyle = node.stroke;
    context.lineWidth = node.strokeWidth;
    context.stroke();
  }
}

function renderText(context: RenderContext2D, node: TextNode): void {
  const lines = node.text.split("\n");
  const lineHeight = node.lineHeight ?? node.fontSize * 1.15;

  context.fillStyle = node.color;
  context.font = `${node.fontWeight ?? 400} ${node.fontSize}px ${node.fontFamily}`;
  context.textAlign = node.align ?? "left";
  context.textBaseline = node.baseline ?? "top";

  lines.forEach((line, index) => {
    context.fillText(line, node.x, node.y + index * lineHeight, node.width);
  });
}

function computeImagePlacement(node: ImageNode, image: HTMLImageElement) {
  const fit = node.fit ?? "cover";

  if (fit === "stretch") {
    return {
      dx: node.x,
      dy: node.y,
      dw: node.width,
      dh: node.height,
      sx: 0,
      sy: 0,
      sw: image.width,
      sh: image.height,
    };
  }

  const imageRatio = image.width / image.height;
  const nodeRatio = node.width / node.height;

  if ((fit === "cover" && imageRatio > nodeRatio) || (fit === "contain" && imageRatio < nodeRatio)) {
    const sh = image.height;
    const sw = sh * nodeRatio;
    const sx = (image.width - sw) / 2;
    return { dx: node.x, dy: node.y, dw: node.width, dh: node.height, sx, sy: 0, sw, sh };
  }

  const sw = image.width;
  const sh = sw / nodeRatio;
  const sy = (image.height - sh) / 2;
  return { dx: node.x, dy: node.y, dw: node.width, dh: node.height, sx: 0, sy, sw, sh };
}

async function renderImage(context: RenderContext2D, node: ImageNode): Promise<void> {
  const image = await loadImage(node.src);
  const placement = computeImagePlacement(node, image);

  context.drawImage(
    image,
    placement.sx,
    placement.sy,
    placement.sw,
    placement.sh,
    placement.dx,
    placement.dy,
    placement.dw,
    placement.dh,
  );
}

async function renderNode(context: RenderContext2D, node: SceneNode): Promise<void> {
  switch (node.type) {
    case "rect":
      renderRect(context, node);
      return;
    case "text":
      renderText(context, node);
      return;
    case "image":
      await renderImage(context, node);
      return;
    case "mesh3d": {
      const subCanvas = renderMeshNode(node);
      context.drawImage(subCanvas, node.x, node.y, node.width, node.height);
      return;
    }
  }
}

export async function renderScene(canvas: RenderCanvas, scene: PosterScene): Promise<RenderCanvas> {
  const context = get2DContext(canvas);
  context.clearRect(0, 0, scene.width, scene.height);
  context.fillStyle = scene.background.fill;
  context.fillRect(0, 0, scene.width, scene.height);

  for (const node of scene.nodes) {
    await applyNodeTransform(context, node, () => renderNode(context, node));
  }

  return canvas;
}
