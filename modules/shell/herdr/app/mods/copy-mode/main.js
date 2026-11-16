import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { run, props, closeOverlay, theme, focused, notify } from "herdr";

const MOTIONS = {
  h: "left",
  left: "left",
  j: "down",
  down: "down",
  k: "up",
  up: "up",
  l: "right",
  right: "right",
  0: "line_start",
  "^": "line_start",
  home: "line_start",
  $: "line_end",
  end: "line_end",
  "ctrl-u": "page_up",
  "ctrl-b": "page_up",
  pageup: "page_up",
  "ctrl-d": "page_down",
  "ctrl-f": "page_down",
  pagedown: "page_down",
  g: "home",
  G: "end",
};

function same(a, b) {
  return a && b && a.row === b.row && a.col === b.col;
}

function keyName(e) {
  if (e.key.length > 1) return e.keystroke;
  const plain =
    !e.modifiers.control && !e.modifiers.alt && !e.modifiers.platform;
  if (plain && e.key_char && e.key_char.length === 1) return e.key_char;
  return e.keystroke;
}

export default class CopyMode extends View {
  init(_props, cx) {
    this.mode = "normal";
    this.count = "";
    this.collapse = false;
    this.handle = cx.focus_handle();
    this.handle.focus();
    this.terminal = focused()?.terminal ?? null;
    if (this.terminal)
      run("terminal.select", {
        terminal: this.terminal,
        at: "cursor",
        mode: "cell",
      });
  }

  selection() {
    const f = focused();
    return f && f.terminal === this.terminal ? f.selection : null;
  }

  exit(copy) {
    if (this.terminal) {
      if (copy) run("terminal.copy", {});
      run("terminal.clear_selection", { terminal: this.terminal });
    }
    closeOverlay(props().overlay);
    if (copy) notify({ title: "Copied selection", timeout: 1500 });
  }

  move(direction, times) {
    for (let i = 0; i < times; i++)
      run("terminal.adjust_selection", { terminal: this.terminal, direction });
    if (this.mode === "normal") this.collapse = true;
  }

  toNormal() {
    this.mode = "normal";
    const s = this.selection();
    if (s)
      run("terminal.select", {
        terminal: this.terminal,
        at: s.end,
        mode: "cell",
      });
  }

  key(e, cx) {
    if (!this.terminal) return this.exit(false);
    const k = keyName(e);
    if (/^[0-9]$/.test(k) && (k !== "0" || this.count)) {
      this.count += k;
      return cx.notify();
    }
    const times = Math.max(1, Math.min(500, parseInt(this.count || "1", 10)));
    this.count = "";
    if (k === "escape" || k === "q" || k === "ctrl-c") {
      if (this.mode !== "normal") this.toNormal();
      else this.exit(false);
      return cx.notify();
    }
    if (k === "y" || k === "enter") return this.exit(this.mode !== "normal");
    if (k === "v") {
      if (this.mode === "visual") this.toNormal();
      else this.mode = "visual";
      return cx.notify();
    }
    if (k === "V") {
      const s = this.selection();
      if (s)
        run("terminal.select", {
          terminal: this.terminal,
          at: s.end,
          mode: "line",
        });
      this.mode = "line";
      return cx.notify();
    }
    if (MOTIONS[k])
      this.move(
        MOTIONS[k],
        ["home", "end", "line_start", "line_end"].includes(MOTIONS[k])
          ? 1
          : times,
      );
    cx.notify();
  }

  render(cx) {
    const t = theme();
    const s = this.selection();
    if (this.collapse && s && !same(s.start, s.end)) {
      this.collapse = false;
      run("terminal.select", {
        terminal: this.terminal,
        at: s.end,
        mode: "cell",
      });
    }
    const label = { normal: "COPY", visual: "VISUAL", line: "V-LINE" }[
      this.mode
    ];
    const hint =
      this.mode === "normal"
        ? "hjkl move · v select · V line · esc exit"
        : "move to extend · y copy · esc back";
    return v_flex()
      .id("copy-mode-root")
      .size_full()
      .justify_end()
      .items_end()
      .p(14)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        cx.stop_propagation();
        this.key(e, cx);
      })
      .on_mouse_down("left", (_e, cx) => {
        cx.stop_propagation();
        this.exit(false);
      })
      .child(
        h_flex()
          .gap(10)
          .px(12)
          .py(5)
          .rounded(8)
          .bg(t.popover)
          .border_1()
          .border_color(t.accent)
          .text_size(12.5)
          .child(
            div()
              .text_color(t.accent)
              .font_semibold()
              .child(label + (this.count ? ` ${this.count}` : "")),
          )
          .child(
            div()
              .text_color(t.muted)
              .child(s ? `${s.end.row}:${s.end.col}` : "…"),
          )
          .child(div().text_color(t.dim).child(hint)),
      );
  }
}
