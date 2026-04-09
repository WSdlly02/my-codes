import type { PosterScene } from "../types/scene";

const MM_PER_INCH = 25.4;

export interface PaperSize {
  widthMm: number;
  heightMm: number;
}

export const A3_PORTRAIT: PaperSize = {
  widthMm: 297,
  heightMm: 420,
};

export function mmToPixels(mm: number, dpi: number): number {
  return Math.round((mm / MM_PER_INCH) * dpi);
}

export function createPosterSceneSize(
  paper: PaperSize,
  dpi: number,
): Pick<PosterScene, "width" | "height" | "dpi"> {
  return {
    width: mmToPixels(paper.widthMm, dpi),
    height: mmToPixels(paper.heightMm, dpi),
    dpi,
  };
}
