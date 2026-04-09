import { useEffect, useRef, useState } from "react";
import { sampleScene } from "./demo/sampleScene";
import { exportPosterAsJpeg, renderPoster } from "./render/exportJpeg";

const PREVIEW_WIDTH = 420;

function App() {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const [isRendering, setIsRendering] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;

    async function drawPreview() {
      setIsRendering(true);
      setError(null);

      try {
        await document.fonts.ready;
        const posterCanvas = await renderPoster(sampleScene);
        if (cancelled || !canvasRef.current) {
          return;
        }

        const previewScale = PREVIEW_WIDTH / sampleScene.width;
        const previewHeight = Math.round(sampleScene.height * previewScale);

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
  }, []);

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
          A3 / 300dpi render pipeline
        </h1>
        <p style={{ margin: 0, lineHeight: 1.6, color: "#374151" }}>
          2D layout is composited on Canvas. 3D nodes are rendered with Three.js and merged into the
          same export surface before JPEG output.
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
              This repo is set up around a scene schema, a 2D compositor, and a 3D subscene renderer.
              That keeps poster templates reusable and avoids rebuilding the stack per project.
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
              onClick={() => void exportPosterAsJpeg(sampleScene)}
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
              {isRendering ? "Rendering preview..." : "Export is configured for 3508 x 4961 px."}
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
