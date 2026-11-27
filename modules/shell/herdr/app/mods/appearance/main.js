import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import {
  run,
  props,
  closeOverlay,
  theme,
  settings,
  appHome,
  exec,
  readTextFile,
  notify,
  checking,
} from "herdr";

const THEMES = { light: "rose-pine-dawn", dark: "rose-pine-moon" };
const SOLID = "-solid";

const MODES = [
  { value: null, label: "System", key: "s" },
  { value: "light", label: "Light", key: "l" },
  { value: "dark", label: "Dark", key: "d" },
];

const GLASS = [
  { value: true, label: "On", key: null },
  { value: false, label: "Off", key: null },
];

const WRITE_IN_PLACE = 'printf "%s" "$2" > "$1"';

async function writeInPlace(path, text, expected) {
  if ((await readTextFile(path)) !== expected)
    throw new Error("settings.json changed while editing it, try again");
  const result = await exec("sh", ["-c", WRITE_IN_PLACE, "sh", path, text]);
  if (result.code !== 0) throw new Error(result.stderr.trim() || "write failed");
}

const APPEARANCE = /"appearance"\s*:\s*"[^"]*"/;

function withAppearance(text, value) {
  const parsed = JSON.parse(text);
  const candidates = [];
  if (value === null) {
    candidates.push(
      text.replace(/\n[ \t]*"appearance"\s*:\s*"[^"]*"\s*,/, ""),
      text.replace(/,\s*\n[ \t]*"appearance"\s*:\s*"[^"]*"/, ""),
    );
  } else if ("appearance" in parsed) {
    candidates.push(text.replace(APPEARANCE, `"appearance": "${value}"`));
  } else {
    candidates.push(text.replace(/^\s*\{/, `{\n  "appearance": "${value}",`));
  }
  for (const candidate of candidates) {
    try {
      if (JSON.parse(candidate).appearance === (value ?? undefined))
        return candidate;
    } catch {}
  }
  if (value === null) delete parsed.appearance;
  else parsed.appearance = value;
  return JSON.stringify(parsed, null, 2) + "\n";
}

function glassOn() {
  const s = settings();
  return !String(s.theme_dark ?? s.theme ?? "").endsWith(SOLID);
}

export default class Appearance extends View {
  init(_props, cx) {
    this.handle = cx.focus_handle();
    this.handle.focus();
    this.row = 0;
    this.mode = undefined;
    this.path = null;
    this.busy = false;
    if (checking()) return;
    cx.spawn(async (cx) => {
      try {
        const link = `${appHome()}/settings.json`;
        const real = await exec("realpath", [link]);
        this.path = real.code === 0 ? real.stdout.trim() : link;
        const text = await readTextFile(this.path);
        this.mode = text ? (JSON.parse(text).appearance ?? null) : null;
      } catch (e) {
        this.fail(e);
      }
      cx.notify();
    });
  }

  fail(e) {
    notify({
      title: "Could not change appearance",
      body: String(e?.message ?? e),
      level: "error",
      timeout: 4000,
    });
  }

  close() {
    closeOverlay(props().overlay);
  }

  setMode(value, cx) {
    if (this.busy || !this.path || this.mode === value) return;
    this.busy = true;
    const previous = this.mode;
    this.mode = value;
    cx.notify();
    cx.spawn(async (cx) => {
      try {
        const text = (await readTextFile(this.path)) ?? "{}\n";
        await writeInPlace(this.path, withAppearance(text, value), text);
      } catch (e) {
        this.mode = previous;
        this.fail(e);
      }
      this.busy = false;
      cx.notify();
    });
  }

  setGlass(on, cx) {
    if (glassOn() === on) return;
    const suffix = on ? "" : SOLID;
    run("app.theme", { name: THEMES.light + suffix });
    run("app.theme", { name: THEMES.dark + suffix });
    cx.notify();
  }

  cycle(by, cx) {
    if (this.row === 0) {
      const i = MODES.findIndex((m) => m.value === this.mode);
      const next = MODES[(i + by + MODES.length) % MODES.length];
      this.setMode(next.value, cx);
    } else {
      this.setGlass(!glassOn(), cx);
    }
  }

  key(e, cx) {
    const k = e.keystroke;
    if (k === "escape" || k === "enter" || k === "q") return this.close();
    if (k === "down" || k === "j" || k === "up" || k === "k") {
      this.row = this.row === 0 ? 1 : 0;
      return cx.notify();
    }
    if (k === "left" || k === "shift-tab") return this.cycle(-1, cx);
    if (k === "right" || k === "tab" || k === "space") return this.cycle(1, cx);
    const mode = MODES.find((m) => m.key === k);
    if (mode) return this.setMode(mode.value, cx);
    if (k === "t") return this.setGlass(!glassOn(), cx);
  }

  segments(id, options, current, pick, t) {
    return h_flex()
      .gap(2)
      .p(2)
      .rounded(8)
      .bg(t.raised)
      .border_1()
      .border_color(t.border)
      .children(
        options.map((o, i) => {
          const on = o.value === current;
          return h_flex()
            .id(`${id}-${i}`)
            .gap(6)
            .px(12)
            .py(4)
            .rounded(6)
            .cursor_pointer()
            .when(on, (el) => el.bg(t.accent).text_color(t.accent_foreground))
            .when(!on, (el) =>
              el
                .text_color(t.muted)
                .hover((s) => s.bg(t.hover).text_color(t.foreground)),
            )
            .on_click((_e, cx) => pick(o.value, cx))
            .child(o.label)
            .when(Boolean(o.key), (el) =>
              el.child(
                div()
                  .text_size(10.5)
                  .text_color(on ? t.accent_foreground : t.dim)
                  .child(o.key),
              ),
            );
        }),
      );
  }

  line(index, title, detail, control, t) {
    const on = this.row === index;
    return h_flex()
      .id(`appearance-row-${index}`)
      .justify_between()
      .items_center()
      .gap(16)
      .px(12)
      .py(10)
      .rounded(8)
      .border_1()
      .border_color(on ? t.accent : "#00000000")
      .when(on, (el) => el.bg(t.selected))
      .on_click((_e, cx) => {
        this.row = index;
        cx.notify();
      })
      .child(
        v_flex()
          .gap(2)
          .min_w(0)
          .child(div().text_color(t.foreground).child(title))
          .child(div().text_size(11.5).text_color(t.dim).child(detail)),
      )
      .child(control);
  }

  render(cx) {
    const t = theme();
    const s = settings();
    const glass = glassOn();
    const modeDetail =
      this.mode === undefined
        ? "Reading settings…"
        : this.mode === null
          ? `Following the system (${s.dark ? "dark" : "light"} now)`
          : `Always ${this.mode}`;
    return v_flex()
      .id("appearance")
      .size_full()
      .p(14)
      .gap(10)
      .text_size(13)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        cx.stop_propagation();
        this.key(e, cx);
      })
      .child(
        h_flex()
          .justify_between()
          .px(2)
          .child(
            div().font_semibold().text_color(t.foreground).child("Appearance"),
          )
          .child(
            div()
              .text_color(t.dim)
              .child(String(s.theme ?? "")),
          ),
      )
      .child(
        v_flex()
          .gap(4)
          .child(
            this.line(
              0,
              "Mode",
              modeDetail,
              this.segments(
                "mode",
                MODES,
                this.mode,
                (v, cx) => this.setMode(v, cx),
                t,
              ),
              t,
            ),
          )
          .child(
            this.line(
              1,
              "Transparency",
              glass ? "Frosted glass over the desktop" : "Solid background",
              this.segments(
                "glass",
                GLASS,
                glass,
                (v, cx) => this.setGlass(v, cx),
                t,
              ),
              t,
            ),
          ),
      )
      .child(div().flex_1())
      .child(
        div()
          .px(2)
          .text_size(11.5)
          .text_color(t.dim)
          .child("s l d mode · t transparency · ←/→ change · esc close"),
      );
  }
}
