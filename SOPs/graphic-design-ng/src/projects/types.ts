import type { PosterScene } from "../types/scene";

export interface PosterProject {
  id: string;
  name: string;
  description: string;
  scene: PosterScene;
  exportFileName?: string;
}
