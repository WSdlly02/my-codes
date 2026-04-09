# Graphic Design NG

Browser-first poster rendering framework for A3, 300dpi+ JPG export with mixed 2D and 3D content.

## Recommended stack

- `React` for the editor shell and preview surface
- `TypeScript` for scene schema and renderer boundaries
- `Canvas 2D` for final poster compositing
- `Three.js` for 3D object rendering into sub-canvases
- `Vite` for local development

## Render pipeline

1. Author a poster as a typed scene object.
2. Render 2D nodes directly onto the target canvas.
3. Render 3D nodes with Three.js at export resolution.
4. Composite everything into one canvas.
5. Export the final bitmap as `image/jpeg`.

## A3 export baseline

- A3 portrait: `3508 x 4961 px`
- DPI: `300`
- Output: `JPG`

## Install

```bash
npm install
```

## Run locally

```bash
npm run dev
```

Then open the local Vite URL in your browser. The page shows:

- a scaled poster preview
- the current stack and export target
- an `Export A3 JPG` button

## Build

```bash
npm run build
```

This produces the production bundle in `dist/`.

## How to use

The renderer is driven by a typed scene object. The sample entry is in [src/demo/sampleScene.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/demo/sampleScene.ts).

### 1. Define poster size

Use the A3 helpers in [src/constants/paper.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/constants/paper.ts) to convert physical size to pixels at the target DPI.

### 2. Define scene nodes

Supported node types are defined in [src/types/scene.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/types/scene.ts):

- `rect`
- `text`
- `image`
- `mesh3d`

### 3. Preview in the app

The preview page is [src/App.tsx](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/App.tsx). Replace `sampleScene` with your own scene to preview another poster.

### 4. Export JPG

Call `exportPosterAsJpeg(scene)` from [src/render/exportJpeg.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/render/exportJpeg.ts) to generate the final A3 JPG in the browser.

## Project structure

- `src/types`: scene schema
- `src/constants`: paper and unit helpers
- `src/render`: 2D compositor, 3D renderer, export pipeline
- `src/demo`: sample poster scene
- `src/App.tsx`: preview and export UI

## Current scope

This scaffold is ready for:

- A3 300dpi JPG export
- 2D poster composition
- basic 3D mesh rendering and compositing
- browser preview

It does not yet include:

- `glTF/GLB` model loading
- advanced text layout
- template persistence
- batch rendering

## First build target

This initial scaffold covers:

- Scene schema
- 2D rect and text nodes
- 3D mesh nodes
- A3 300dpi render/export path
- Preview UI
