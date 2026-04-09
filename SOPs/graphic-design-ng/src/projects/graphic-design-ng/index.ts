import type { PosterProject } from "../types";
import { graphicDesignNgScene } from "./scene";

export const graphicDesignNgProject: PosterProject = {
  id: "graphic-design-ng",
  name: "Graphic Design NG",
  description: "Framework sample poster with mixed 2D layout and embedded 3D mesh rendering.",
  scene: graphicDesignNgScene,
  exportFileName: "graphic-design-ng-a3-300dpi.jpg",
};
