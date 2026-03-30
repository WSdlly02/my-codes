import { useEffect, useMemo, useRef, useState } from 'react';
import './App.css';
import { backend, useDeckState, useFullscreen, useKeyboardBindings } from './framework';

type Theme = 'dark' | 'light';

type SystemInfo = {
  timestamp: string;
  hostname: string;
  os: string;
  arch: string;
  numCpu: number;
  goroutines: number;
  memoryAllocMb: number;
  appUptimeSec: number;
};

type Slide = {
  label?: string;
  kind:
    | 'hero'
    | 'why'
    | 'architecture'
    | 'comparison'
    | 'system-info'
    | 'getting-started'
    | 'ending';
};

const slides: Slide[] = [
  { kind: 'hero' },
  { label: '为什么', kind: 'why' },
  { label: '架构', kind: 'architecture' },
  { label: '对比', kind: 'comparison' },
  { label: '实时能力', kind: 'system-info' },
  { label: '开始', kind: 'getting-started' },
  { kind: 'ending' },
];

const whyCards = [
  {
    icon: '📊',
    title: '实时数据',
    description: 'Go 后端实时拉取 API 数据，图表自动更新，PowerPoint 无法覆盖这种动态表达。',
  },
  {
    icon: '⚡',
    title: '交互演示',
    description: '嵌入可操作的 Demo、代码编辑器、3D 模型，而不只是静态截图。',
  },
  {
    icon: '🎨',
    title: '无限表现力',
    description: 'CSS 动画、WebGL、Canvas，现代 Web 的全部能力都能直接进入你的 deck。',
  },
  {
    icon: '📦',
    title: '单文件分发',
    description: 'Go 二进制直接内嵌前端产物，启动浏览器即可运行，无需 WebView 适配。',
  },
];

const featureItems = [
  {
    icon: '🔥',
    title: '热重载',
    description: '前端仍然是标准 Vite 开发流，改 React 组件立即热更新。',
  },
  {
    icon: '🌐',
    title: '浏览器宿主',
    description: '直接使用系统浏览器，渲染稳定、WebGPU 可用、跨平台表现更一致。',
  },
  {
    icon: '📦',
    title: '一键打包',
    description: '前端构建后由 Go embed 打进单二进制，部署链路依然简单。',
  },
];

const comparisonRows = [
  ['Browser Host ✦', '< 20 MB', 'Go', '✓', '快', '低'],
  ['Wails v2', '< 5 MB', 'Go', '✓', '快', '低'],
  ['Electron', '~150 MB', 'Node.js', '✗', '慢', '低'],
  ['PowerPoint', '—', '无', '✓', '—', '低但功能有限'],
] as const;

function LogoMark() {
  return (
    <div className="logo-mark" aria-label="PitchDeck">
      <svg width="24" height="24" viewBox="0 0 24 24" fill="none" aria-hidden="true">
        <rect x="2" y="2" width="9" height="9" rx="2" fill="var(--accent)" opacity="0.9" />
        <rect x="13" y="2" width="9" height="9" rx="2" fill="var(--accent)" opacity="0.4" />
        <rect x="2" y="13" width="9" height="9" rx="2" fill="var(--accent)" opacity="0.4" />
        <rect x="13" y="13" width="9" height="9" rx="2" fill="var(--accent)" opacity="0.7" />
      </svg>
      <span>PitchDeck</span>
    </div>
  );
}

function App() {
  const [theme, setTheme] = useState<Theme>('dark');
  const { currentIndex, total, goTo } = useDeckState(slides);
  const [exiting, setExiting] = useState<number | null>(null);
  const [isAnimating, setIsAnimating] = useState(false);
  const [systemInfo, setSystemInfo] = useState<SystemInfo | null>(null);
  const [isQuitting, setIsQuitting] = useState(false);
  const touchStartX = useRef<number | null>(null);
  const dots = useMemo(() => Array.from({ length: total }, (_, index) => index), [total]);
  const { isFullscreen, enter: enterFullscreen, exit: exitFullscreen } = useFullscreen();
  const isPreviewPage = currentIndex === 0;
  const canGoPreviousInShow = currentIndex > 0;
  const canGoNextInShow = currentIndex < total - 1;

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
  }, [theme]);

  useEffect(() => {
    const onTouchStart = (event: TouchEvent) => {
      touchStartX.current = event.touches[0]?.clientX ?? null;
    };

    const onTouchEnd = (event: TouchEvent) => {
      if (touchStartX.current === null) {
        return;
      }

      const endX = event.changedTouches[0]?.clientX;
      if (typeof endX !== 'number') {
        touchStartX.current = null;
        return;
      }

      const deltaX = endX - touchStartX.current;
      touchStartX.current = null;

      if (Math.abs(deltaX) > 50) {
        if (isPreviewPage) {
          return;
        }

        animateTo(deltaX < 0 ? currentIndex + 1 : currentIndex - 1);
      }
    };

    document.addEventListener('touchstart', onTouchStart, { passive: true });
    document.addEventListener('touchend', onTouchEnd);

    return () => {
      document.removeEventListener('touchstart', onTouchStart);
      document.removeEventListener('touchend', onTouchEnd);
    };
  }, [currentIndex, isAnimating, isPreviewPage]);

  const animateTo = (nextIndex: number) => {
    if (isAnimating || nextIndex === currentIndex || nextIndex < 0 || nextIndex >= total) {
      return;
    }

    setIsAnimating(true);
    setExiting(currentIndex);

    window.setTimeout(() => {
      goTo(nextIndex);
      window.setTimeout(() => {
        setExiting(null);
        setIsAnimating(false);
      }, 650);
    }, 350);
  };

  useKeyboardBindings([
    { keys: ['ArrowRight'], onKey: () => animateTo(currentIndex + 1) },
    { keys: ['ArrowLeft'], onKey: () => animateTo(currentIndex - 1) },
    { keys: ['f', 'F'], onKey: () => void toggleFullscreenMode() },
  ]);

  useEffect(() => {
    void backend.getJSON<{ ok: boolean; filesystem?: unknown; realtime?: unknown; os?: unknown }>(
      '/api/capabilities',
    ).catch(() => undefined);
  }, []);

  useEffect(() => {
    let mounted = true;

    void backend.call<SystemInfo>('get_system_info')
      .then((data) => {
        if (mounted) {
          setSystemInfo(data);
        }
      })
      .catch(() => undefined);

    const unsubscribe = backend.on<SystemInfo>('system_info_updated', (data) => {
      setSystemInfo(data);
    });

    return () => {
      mounted = false;
      unsubscribe();
    };
  }, []);

  const toggleTheme = () => {
    setTheme((value) => (value === 'dark' ? 'light' : 'dark'));
  };

  const startShow = async () => {
    await enterFullscreen();
    goTo(1);
  };

  const toggleFullscreenMode = async () => {
    if (isFullscreen) {
      await exitFullscreen();
      return;
    }

    await enterFullscreen();
  };

  const quitApp = async () => {
    if (isQuitting) {
      return;
    }

    setIsQuitting(true);

    try {
      await backend.call<{ ok: boolean }>('quit_app');
      window.close();
    } catch {
      setIsQuitting(false);
    }
  };

  const presentButtonLabel = isFullscreen ? '结束放映' : '进入放映';

  const renderSlide = (slide: Slide, index: number) => {
    const className = [
      'slide',
      currentIndex === index ? 'active' : '',
      exiting === index ? 'exit' : '',
    ]
      .filter(Boolean)
      .join(' ');

    switch (slide.kind) {
      case 'hero':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="glow-ring" />
            <div className="slide-inner-center">
              <div className="hero-badge">
                <div className="hero-badge-dot" />
                React + Go + Browser
              </div>
              <h1 className="cta-title">
                用代码写
                <br />
                <em>你的演示</em>
              </h1>
              <p className="slide-body slide-body-center">
                比 PowerPoint 更强大、比 Electron 更轻量。
                <br />
                一个本地浏览器宿主，承载你的前端演示。
              </p>
              <div className="hero-actions hero-actions-center">
                <button className="primary-action" onClick={() => void startShow()} type="button">
                  开始放映
                </button>
              </div>
            </div>
          </section>
        );
      case 'why':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="slide-inner">
              <div className="slide-label">{slide.label}</div>
              <h2 className="slide-subtitle">
                幻灯片不该只是静态页面
                <br />
                也不该被宿主拖慢
              </h2>
              <div className="card-grid">
                {whyCards.map((card) => (
                  <article className="card" key={card.title}>
                    <div className="card-icon">{card.icon}</div>
                    <div className="card-title">{card.title}</div>
                    <div className="card-desc">{card.description}</div>
                  </article>
                ))}
              </div>
            </div>
          </section>
        );
      case 'architecture':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="slide-inner">
              <div className="slide-label">{slide.label}</div>
              <div className="two-col">
                <div>
                  <h2 className="slide-subtitle">
                    前端写界面
                    <br />
                    浏览器承载演示壳
                  </h2>
                  <p className="slide-body">
                    保留现有 React + Vite 前端，由 Go 启动本地 HTTP 服务并拉起系统浏览器。
                    这样渲染能力直接交给浏览器，Linux 下也不再依赖 WebView 适配。
                  </p>
                  <div className="stat-row stat-row-tight">
                    <div className="stat">
                      <div className="stat-num">
                        <span>1</span>套
                      </div>
                      <div className="stat-label">前端代码</div>
                    </div>
                    <div className="stat">
                      <div className="stat-num">
                        <span>0</span>个
                      </div>
                      <div className="stat-label">Wails 绑定依赖</div>
                    </div>
                    <div className="stat">
                      <div className="stat-num">
                        Go
                      </div>
                      <div className="stat-label">原生宿主</div>
                    </div>
                  </div>
                </div>
                <div>
                  <div className="code-block">
                    <span className="cm">// frontend/src/App.tsx</span>
                    <br />
                    <span className="kw">const</span> next = () =&gt; <span className="fn">setCurrent</span>(index + 1)
                    <br />
                    <br />
                    <span className="cm">// main.go</span>
                    <br />
                    <span className="kw">func</span> <span className="fn">main</span>() {'{'}
                    <br />
                    {'  '}go <span className="fn">openBrowser</span>(url)
                    <br />
                    {'  '}http.<span className="fn">ListenAndServe</span>(addr, mux)
                    <br />
                    {'}'}
                  </div>
                </div>
              </div>
            </div>
          </section>
        );
      case 'comparison':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="slide-inner">
              <div className="slide-label">{slide.label}</div>
              <h2 className="slide-subtitle">
                这次迁移解决什么
                <br />
                又保留什么
              </h2>
              <table className="compare-table">
                <thead>
                  <tr>
                    <th>方案</th>
                    <th>体积</th>
                    <th>宿主语言</th>
                    <th>单文件</th>
                    <th>构建速度</th>
                    <th>学习曲线</th>
                  </tr>
                </thead>
                <tbody>
                  {comparisonRows.map((row) => (
                    <tr key={row[0]}>
                      <td>{row[0]}</td>
                      <td>{row[1]}</td>
                      <td>{row[2]}</td>
                      <td>
                        <span className={row[3] === '✓' ? 'badge-yes' : 'badge-no'}>{row[3]}</span>
                      </td>
                      <td>{row[4]}</td>
                      <td>{row[5]}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </section>
        );
      case 'system-info':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="slide-inner">
              <div className="slide-label">{slide.label}</div>
              <h2 className="slide-subtitle">
                后端只负责
                <br />
                浏览器拿不到的实时能力
              </h2>
              <p className="slide-body">
                这一页的数据由 Go 通过 WebSocket 持续推送，前端只负责展示，不把页码、动画和导航状态回写给后端。
              </p>
              <div className="system-grid">
                <article className="metric-card">
                  <div className="metric-label">主机</div>
                  <div className="metric-value metric-value-compact">{systemInfo?.hostname ?? 'loading'}</div>
                </article>
                <article className="metric-card">
                  <div className="metric-label">平台</div>
                  <div className="metric-value metric-value-compact">
                    {systemInfo ? `${systemInfo.os} / ${systemInfo.arch}` : 'loading'}
                  </div>
                </article>
                <article className="metric-card">
                  <div className="metric-label">CPU</div>
                  <div className="metric-value">{systemInfo?.numCpu ?? '...'}</div>
                </article>
                <article className="metric-card">
                  <div className="metric-label">Goroutines</div>
                  <div className="metric-value">{systemInfo?.goroutines ?? '...'}</div>
                </article>
                <article className="metric-card">
                  <div className="metric-label">Heap MB</div>
                  <div className="metric-value">
                    {systemInfo ? systemInfo.memoryAllocMb.toFixed(2) : '...'}
                  </div>
                </article>
                <article className="metric-card">
                  <div className="metric-label">Uptime</div>
                  <div className="metric-value">{systemInfo ? `${systemInfo.appUptimeSec}s` : '...'}</div>
                </article>
              </div>
              <div className="system-footnote">
                最新采样时间：{systemInfo ? new Date(systemInfo.timestamp).toLocaleTimeString() : 'waiting for websocket'}
              </div>
            </div>
          </section>
        );
      case 'getting-started':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="slide-inner">
              <div className="slide-label">{slide.label}</div>
              <h2 className="slide-subtitle">
                保留现有前端
                <br />
                只替换运行宿主
              </h2>
              <div className="code-block code-block-spaced">
                <span className="cm"># 1. 构建前端产物</span>
                <br />
                <span className="fn">cd</span> <span className="str">frontend</span> &amp;&amp; <span className="fn">npm install</span> &amp;&amp; <span className="fn">npm run</span> <span className="str">build</span>
                <br />
                <br />
                <span className="cm"># 2. 启动本地服务并拉起浏览器</span>
                <br />
                <span className="fn">go run</span> <span className="str">.</span>
                <br />
                <br />
                <span className="cm"># 3. 构建单文件分发</span>
                <br />
                <span className="fn">go build</span>
              </div>
              <ul className="feature-list">
                {featureItems.map((item) => (
                  <li className="feature-item" key={item.title}>
                    <div className="feature-item-icon">{item.icon}</div>
                    <div className="feature-item-text">
                      <strong>{item.title}</strong>
                      <span>{item.description}</span>
                    </div>
                  </li>
                ))}
              </ul>
            </div>
          </section>
        );
      case 'ending':
        return (
          <section className={className} key={slide.kind}>
            <div className="bg-grid" />
            <div className="glow-ring glow-ring-secondary" />
            <div className="slide-inner-center">
              <p className="slide-label slide-label-center">迁移完成</p>
              <h2 className="cta-title">
                React 前端保留
                <br />
                <em className="accent-secondary">宿主切到浏览器</em>
              </h2>
              <p className="slide-body slide-body-center">
                这版项目已经摆脱 Linux WebView 的缩放和兼容性问题。
                <br />
                放映结束后可以直接退出应用。
              </p>
              <div className="hero-actions">
                <button className="danger-action" onClick={() => void quitApp()} type="button" disabled={isQuitting}>
                  {isQuitting ? '正在退出...' : '退出程序'}
                </button>
              </div>
            </div>
          </section>
        );
    }
  };

  return (
    <div className="deck-shell">
      <LogoMark />

      <button
        id="theme-toggle"
        aria-label="切换主题"
        title="切换明暗主题"
        onClick={toggleTheme}
        type="button"
      >
        {theme === 'dark' ? '🌙' : '☀️'}
      </button>

      <div id="fs-hint">
        {isPreviewPage ? (
          <>
            <kbd>F</kbd> 切换全屏 <kbd>→</kbd> 下一页 <kbd>←</kbd> 上一页
          </>
        ) : (
          <>
            <kbd>F</kbd> 切换全屏 <kbd>→</kbd> 下一页 <kbd>←</kbd> 上一页
          </>
        )}
      </div>

      <main id="app">{slides.map(renderSlide)}</main>

      <div id="controls" role="navigation" aria-label="幻灯片控制">
        <button
          className="ctrl-btn"
          aria-label="上一页"
          title="← 上一页"
          onClick={() => animateTo(currentIndex - 1)}
          disabled={!canGoPreviousInShow}
          type="button"
        >
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round">
            <path d="m15 18-6-6 6-6" />
          </svg>
        </button>
        <div id="progress-dots">
          {dots.map((index) => (
            <button
              key={index}
              className={`dot${index === currentIndex ? ' active' : ''}`}
              aria-label={`跳到第 ${index + 1} 页`}
              onClick={() => animateTo(index)}
              type="button"
            />
          ))}
        </div>
        <span id="slide-counter">
          {currentIndex + 1} / {total}
        </span>
        <button
          className="ctrl-btn"
          aria-label="下一页"
          title="→ 下一页"
          onClick={() => animateTo(currentIndex + 1)}
          disabled={!canGoNextInShow}
          type="button"
        >
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round">
            <path d="m9 18 6-6-6-6" />
          </svg>
        </button>
      </div>

      {!isPreviewPage ? (
        <button
          className="present-fab"
          aria-label={presentButtonLabel}
          title={presentButtonLabel}
          onClick={() => void toggleFullscreenMode()}
          type="button"
        >
          {presentButtonLabel}
        </button>
      ) : null}
    </div>
  );
}

export default App;
