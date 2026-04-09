import { graphicDesignNgProject } from "./graphic-design-ng";
import type { PosterProject } from "./types";

export const posterProjects: PosterProject[] = [graphicDesignNgProject];

export const defaultProject = posterProjects[0];

export function getProjectById(projectId: string): PosterProject | undefined {
  return posterProjects.find((project) => project.id === projectId);
}
