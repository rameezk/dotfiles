// @ts-nocheck
import { mkdirSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const POLL_MS = 5000;
const terminal = process.env.HC_TERMINAL_ID;
const dir = path.join(homedir(), ".local", "state", "herdr-fuel");

export default function (pi: ExtensionAPI) {
  if (!terminal) return;

  let last = "";
  let timer: ReturnType<typeof setInterval> | undefined;

  function write(ctx: any) {
    let usage;
    try {
      usage = ctx.getContextUsage?.();
    } catch {
      return;
    }
    if (typeof usage?.tokens !== "number") return;
    const reading = {
      harness: "pi",
      model: ctx.model?.name ?? ctx.model?.id ?? null,
      tokens: usage.tokens,
      window: usage.contextWindow ?? ctx.model?.contextWindow ?? null,
    };
    const key = JSON.stringify(reading);
    if (key === last) return;
    last = key;
    try {
      mkdirSync(dir, { recursive: true });
      const file = path.join(dir, `${terminal}.json`);
      writeFileSync(`${file}.tmp`, JSON.stringify({ ...reading, at: Math.floor(Date.now() / 1000) }));
      renameSync(`${file}.tmp`, file);
    } catch {}
  }

  pi.on("session_start", async (_event, ctx) => {
    write(ctx);
    if (timer) clearInterval(timer);
    timer = setInterval(() => write(ctx), POLL_MS);
    timer.unref?.();
  });

  pi.on("turn_end", async (_event, ctx) => write(ctx));
  pi.on("agent_end", async (_event, ctx) => write(ctx));

  pi.on("session_shutdown", async () => {
    if (timer) clearInterval(timer);
    timer = undefined;
  });
}
