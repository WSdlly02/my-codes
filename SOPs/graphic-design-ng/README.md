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

## Layer split

The repository is organized so framework code and poster content do not live in the same place.

- Framework layer: [src/types](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/types/scene.ts), [src/constants](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/constants/paper.ts), [src/render](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/render/renderScene.ts)
- Content layer: [src/projects](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/projects/index.ts)
- App shell: [src/App.tsx](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/App.tsx)

The app shell only loads a project definition and passes its scene to the renderer. The renderer does not import any project-specific content.

## How to use

The renderer is driven by a typed scene object. The current sample poster is registered as a project in [src/projects/graphic-design-ng/index.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/projects/graphic-design-ng/index.ts).

### 1. Define poster size

Use the A3 helpers in [src/constants/paper.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/constants/paper.ts) to convert physical size to pixels at the target DPI.

### 2. Define scene nodes

Supported node types are defined in [src/types/scene.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/types/scene.ts):

- `rect`
- `text`
- `image`
- `mesh3d`

### 3. Create a poster project

Create a new folder under `src/projects/<your-project-id>/` with:

- `scene.ts`: the poster scene
- `index.ts`: project metadata and export filename

Use [src/projects/graphic-design-ng/scene.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/projects/graphic-design-ng/scene.ts) as the reference template.

Then register it in [src/projects/index.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/projects/index.ts).

### 4. Preview in the app

The preview page is [src/App.tsx](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/App.tsx). It reads the active project from `src/projects` and renders its scene.

### 5. Export JPG

Call `exportPosterAsJpeg(scene)` from [src/render/exportJpeg.ts](/home/wsdlly02/Documents/my-codes/SOPs/graphic-design-ng/src/render/exportJpeg.ts) to generate the final A3 JPG in the browser.

## Project structure

- `src/types`: scene schema
- `src/constants`: paper and unit helpers
- `src/render`: 2D compositor, 3D renderer, export pipeline
- `src/projects`: poster content and project registration
- `src/App.tsx`: preview and export shell

## Add a new poster

1. Copy `src/projects/graphic-design-ng` to a new folder under `src/projects`.
2. Change the scene content in `scene.ts`.
3. Change `id`, `name`, `description`, and `exportFileName` in `index.ts`.
4. Register the project in `src/projects/index.ts`.
5. Point `defaultProject` to the new project, or replace the selection logic with router/state later.

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

## Next production step

With the layer split in place, you can start building real posters without contaminating the renderer core. The next practical step is to create the first real project under `src/projects/<poster-id>/` and keep all visual content, copy, and assets there.

## First build target

This initial scaffold covers:

- Scene schema
- 2D rect and text nodes
- 3D mesh nodes
- A3 300dpi render/export path
- Preview UI
