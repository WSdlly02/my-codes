export type Fill = string;

export interface BaseNode {
  id: string;
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  opacity?: number;
}

export interface Background {
  fill: Fill;
}

export interface TextNode extends BaseNode {
  type: "text";
  text: string;
  color: string;
  fontFamily: string;
  fontSize: number;
  fontWeight?: string | number;
  lineHeight?: number;
  letterSpacing?: number;
  align?: CanvasTextAlign;
  baseline?: CanvasTextBaseline;
}

export interface RectNode extends BaseNode {
  type: "rect";
  fill: Fill;
  radius?: number;
  stroke?: string;
  strokeWidth?: number;
}

export interface ImageNode extends BaseNode {
  type: "image";
  src: string;
  fit?: "cover" | "contain" | "stretch";
}

export interface Mesh3DNode extends BaseNode {
  type: "mesh3d";
  mesh: "box" | "sphere" | "torusKnot";
  color: string;
  background?: string;
  cameraZ?: number;
  rotationX?: number;
  rotationY?: number;
  rotationZ?: number;
}

export type SceneNode = TextNode | RectNode | ImageNode | Mesh3DNode;

export interface PosterScene {
  width: number;
  height: number;
  dpi: number;
  background: Background;
  nodes: SceneNode[];
}
