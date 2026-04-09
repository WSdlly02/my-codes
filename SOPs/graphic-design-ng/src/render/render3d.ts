import * as THREE from "three";
import { createRenderCanvas, type RenderCanvas } from "./canvas";
import type { Mesh3DNode } from "../types/scene";

function createGeometry(mesh: Mesh3DNode["mesh"]): THREE.BufferGeometry {
  switch (mesh) {
    case "sphere":
      return new THREE.SphereGeometry(1.1, 64, 64);
    case "torusKnot":
      return new THREE.TorusKnotGeometry(0.8, 0.28, 180, 32);
    case "box":
    default:
      return new THREE.BoxGeometry(1.7, 1.7, 1.7, 2, 2, 2);
  }
}

export function renderMeshNode(node: Mesh3DNode): RenderCanvas {
  const canvas = createRenderCanvas(node.width, node.height);

  const renderer = new THREE.WebGLRenderer({
    canvas,
    antialias: true,
    alpha: true,
    preserveDrawingBuffer: true,
  });

  renderer.setSize(node.width, node.height, false);
  renderer.setPixelRatio(1);
  renderer.setClearColor(node.background ?? "#000000", 0);

  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(38, node.width / node.height, 0.1, 100);
  camera.position.z = node.cameraZ ?? 5;

  const ambient = new THREE.AmbientLight("#ffffff", 1.2);
  const key = new THREE.DirectionalLight("#ffffff", 2.4);
  key.position.set(3.2, 4.5, 5.5);

  const rim = new THREE.DirectionalLight("#8ec5ff", 1.1);
  rim.position.set(-4, -1.5, 2);

  const geometry = createGeometry(node.mesh);
  const material = new THREE.MeshPhysicalMaterial({
    color: node.color,
    roughness: 0.25,
    metalness: 0.55,
    clearcoat: 0.4,
  });
  const mesh = new THREE.Mesh(geometry, material);

  mesh.rotation.x = node.rotationX ?? -0.35;
  mesh.rotation.y = node.rotationY ?? 0.65;
  mesh.rotation.z = node.rotationZ ?? 0.1;

  scene.add(ambient, key, rim, mesh);
  renderer.render(scene, camera);

  geometry.dispose();
  material.dispose();
  renderer.dispose();

  return canvas;
}
