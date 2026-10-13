// @ts-nocheck
// Shows billion-context compression stats in the pi footer, e.g.
//   Nerd: \uF066 3 \uF063 120k   (folds, tokens saved)
//   ASCII: ⊛ 3 folds · 120k saved
// Cache hit rate is shown by the powerline `cache_read` segment instead.
// Polls the billion-context proxy (/__bili/sessions) for the current session.
// Surfaced by pi-powerline-footer through its `extension_statuses` segment.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "billion";
const POLL_MS = 5000;
const FETCH_TIMEOUT_MS = 1000;
// bili zone ports start at 18787; 8787 is the documented standalone default.
const PORTS = [
  ...(process.env.ACP_PORT ? [Number(process.env.ACP_PORT)] : []),
  ...Array.from({ length: 12 }, (_, i) => 18787 + i),
  8787,
];

let goodPort: number | undefined;
let timer: ReturnType<typeof setInterval> | undefined;

// Mirror pi-powerline-footer's Nerd Font detection (icons.ts hasNerdFonts).
function useNerd(): boolean {
  const e = process.env;
  if (e.POWERLINE_NERD_FONTS === "1") return true;
  if (e.POWERLINE_NERD_FONTS === "0") return false;
  if (e.GHOSTTY_RESOURCES_DIR) return true;
  const term = (e.TERM_PROGRAM ?? e.TERM ?? "").toLowerCase();
  return ["iterm", "wezterm", "kitty", "ghostty", "alacritty", "kaku"].some((t) => term.includes(t));
}

function fmt(n: number): string {
  if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1).replace(/\.0$/, "")}M`;
  if (n >= 1_000) return `${Math.round(n / 1_000)}k`;
  return String(n);
}

async function fetchSessions(port: number): Promise<any[] | undefined> {
  try {
    const res = await fetch(`http://127.0.0.1:${port}/__bili/sessions`, {
      signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
    });
    if (!res.ok) return undefined;
    const json = await res.json();
    return Array.isArray(json?.sessions) ? json.sessions : undefined;
  } catch {
    return undefined;
  }
}

async function findSession(sessionId: string): Promise<any | undefined> {
  const ports = goodPort ? [goodPort, ...PORTS.filter((p) => p !== goodPort)] : PORTS;
  for (const port of ports) {
    const sessions = await fetchSessions(port);
    if (!sessions) continue;
    const match = sessions.find((s) => s.id === sessionId);
    if (match) {
      goodPort = port;
      return match;
    }
  }
  return undefined;
}

function currentSessionId(ctx: any): string | undefined {
  try {
    const id = ctx.sessionManager?.getSessionId?.();
    if (id) return id;
  } catch {}
  return process.env.PI_SESSION_ID;
}

export default function (pi: ExtensionAPI) {
  let busy = false;

  async function refresh(ctx: any) {
    if (busy) return;
    busy = true;
    try {
      const id = currentSessionId(ctx);
      const s = id ? await findSession(id) : undefined;
      if (!s) {
        ctx.ui.setStatus(STATUS_KEY, undefined);
        return;
      }
      const blocks = s.blocks ?? 0;
      const saved = fmt(s.tokensSaved ?? 0);
      const text = useNerd()
        ? // nf-fa-compress (folds) · nf-fa-arrow_down (tokens saved)
          `\uF066 ${blocks} \uF063 ${saved}`
        : `⊛ ${blocks} fold${blocks === 1 ? "" : "s"} · ${saved} saved`;
      ctx.ui.setStatus(STATUS_KEY, text);
    } finally {
      busy = false;
    }
  }

  pi.on("session_start", async (_event, ctx) => {
    if (timer) clearInterval(timer);
    void refresh(ctx);
    timer = setInterval(() => void refresh(ctx), POLL_MS);
    timer.unref?.();
  });

  pi.on("session_shutdown", async (_event, ctx) => {
    if (timer) clearInterval(timer);
    timer = undefined;
    try {
      ctx.ui.setStatus(STATUS_KEY, undefined);
    } catch {}
  });
}
