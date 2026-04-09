import { useEffect, useRef, useState } from "react";
import { exportPosterAsJpeg, renderPoster } from "./render/exportJpeg";
import { defaultProject } from "./projects";

const PREVIEW_WIDTH = 420;

function App() {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const [isRendering, setIsRendering] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const project = defaultProject;
  const scene = project.scene;

  useEffect(() => {
    let cancelled = false;

    async function drawPreview() {
      setIsRendering(true);
      setError(null);

      try {
        await document.fonts.ready;
        const posterCanvas = await renderPoster(scene);
        if (cancelled || !canvasRef.current) {
          return;
        }

        const previewScale = PREVIEW_WIDTH / scene.width;
        const previewHeight = Math.round(scene.height * previewScale);

        canvasRef.current.width = PREVIEW_WIDTH;
        canvasRef.current.height = previewHeight;

        const context = canvasRef.current.getContext("2d");
        if (!context) {
          throw new Error("Preview context is unavailable.");
        }

        context.clearRect(0, 0, PREVIEW_WIDTH, previewHeight);
        context.drawImage(posterCanvas, 0, 0, PREVIEW_WIDTH, previewHeight);
      } catch (renderError) {
        const message =
          renderError instanceof Error ? renderError.message : "Failed to render poster preview.";
        if (!cancelled) {
          setError(message);
        }
      } finally {
        if (!cancelled) {
          setIsRendering(false);
        }
      }
    }

    void drawPreview();

    return () => {
      cancelled = true;
    };
  }, [scene]);

  return (
    <main
      style={{
        minHeight: "100vh",
        display: "grid",
        gridTemplateColumns: "minmax(280px, 460px) minmax(320px, 1fr)",
        gap: "40px",
        padding: "40px",
        alignItems: "start",
      }}
    >
      <section
        style={{
          background: "rgba(255, 255, 255, 0.58)",
          border: "1px solid rgba(17, 24, 39, 0.08)",
          borderRadius: "28px",
          padding: "24px",
          boxShadow: "0 24px 60px rgba(17, 24, 39, 0.12)",
          backdropFilter: "blur(12px)",
        }}
      >
        <p style={{ margin: 0, fontSize: "13px", letterSpacing: "0.12em", textTransform: "uppercase" }}>
          Poster Preview
        </p>
        <h1 style={{ margin: "12px 0 8px", fontSize: "32px", lineHeight: 1.05 }}>
          {project.name}
        </h1>
        <p style={{ margin: 0, lineHeight: 1.6, color: "#374151" }}>
          {project.description}
        </p>
        <div
          style={{
            marginTop: "24px",
            padding: "18px",
            background: "#f8fafc",
            borderRadius: "24px",
            border: "1px solid rgba(17, 24, 39, 0.08)",
          }}
        >
          <canvas
            ref={canvasRef}
            style={{
              width: "100%",
              display: "block",
              borderRadius: "18px",
              background: "#ffffff",
              boxShadow: "0 18px 30px rgba(17, 24, 39, 0.16)",
            }}
          />
        </div>
      </section>

      <section
        style={{
          paddingTop: "24px",
          maxWidth: "720px",
        }}
      >
        <div style={{ display: "grid", gap: "18px" }}>
          <div>
            <p style={{ margin: 0, fontSize: "13px", letterSpacing: "0.12em", textTransform: "uppercase" }}>
              Recommended Stack
            </p>
            <h2 style={{ margin: "10px 0", fontSize: "40px", lineHeight: 1.05 }}>
              React + TypeScript + Canvas + Three.js
            </h2>
            <p style={{ margin: 0, color: "#1f2937", lineHeight: 1.7 }}>
              The framework layer stays in the renderer and schema modules. Poster-specific content now
              lives under <code>src/projects</code> so each new poster can be added without changing the
              render core.
            </p>
          </div>

          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(auto-fit, minmax(220px, 1fr))",
              gap: "16px",
            }}
          >
            {[
              ["UI shell", "React + Vite"],
              ["Language", "TypeScript"],
              ["2D output", "Canvas 2D"],
              ["3D rendering", "Three.js / WebGL"],
              ["Scene format", "JSON-like schema"],
              ["Export target", "A3 JPG @ 300dpi"],
            ].map(([label, value]) => (
              <article
                key={label}
                style={{
                  padding: "18px 20px",
                  background: "rgba(255, 255, 255, 0.55)",
                  borderRadius: "22px",
                  border: "1px solid rgba(17, 24, 39, 0.08)",
                }}
              >
                <p style={{ margin: 0, fontSize: "13px", color: "#6b7280" }}>{label}</p>
                <p style={{ margin: "8px 0 0", fontSize: "20px", fontWeight: 600 }}>{value}</p>
              </article>
            ))}
          </div>

          <div style={{ display: "flex", gap: "16px", alignItems: "center", flexWrap: "wrap" }}>
            <button
              type="button"
              onClick={() => void exportPosterAsJpeg(scene, project.exportFileName)}
              disabled={isRendering}
              style={{
                border: 0,
                borderRadius: "999px",
                padding: "14px 22px",
                background: "#111827",
                color: "#fff6eb",
                cursor: isRendering ? "wait" : "pointer",
              }}
            >
              Export A3 JPG
            </button>
            <span style={{ color: "#374151" }}>
              {isRendering ? "Rendering preview..." : `Export is configured for ${scene.width} x ${scene.height} px.`}
            </span>
          </div>

          {error ? (
            <p style={{ margin: 0, color: "#b91c1c" }}>
              Render error: {error}
            </p>
          ) : null}
        </div>
      </section>
    </main>
  );
}

export default App;
