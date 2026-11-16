import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import {
  props,
  closeOverlay,
  theme,
  focused,
  hc,
  exec,
  notify,
  checking,
  settings,
} from "herdr";
import { findTokens } from "./patterns.js";

function visible(text) {
  return text.replace(/ /g, " ");
}

export default class Pluck extends View {
  init(_p, cx) {
    const p = props() ?? {};
    this.mode = p.mode === "url" ? "url" : "pluck";
    this.typed = "";
    this.lines = null;
    this.tokens = [];
    this.handle = cx.focus_handle();
    this.handle.focus();
    const terminal = focused()?.terminal;
    if (!terminal || checking()) {
      this.lines = [];
      return;
    }
    cx.spawn(async (cx) => {
      const r = await hc(["read", terminal, "--json"]);
      const lines = (JSON.parse(r.stdout).text ?? "").split("\n");
      while (lines.length && lines[lines.length - 1].trim() === "") lines.pop();
      this.lines = lines;
      this.tokens = findTokens(lines, this.mode);
      if (this.tokens.length === 0) {
        notify({
          title:
            this.mode === "url"
              ? "No URLs on screen"
              : "Nothing to pluck on screen",
          timeout: 2000,
        });
        this.close();
        return;
      }
      cx.notify();
    });
  }

  close() {
    closeOverlay(props().overlay);
  }

  choose(token, cx) {
    this.close();
    cx.spawn(async () => {
      if (this.mode === "url") {
        await exec("open", [token.text]);
      } else {
        await exec("sh", ["-c", 'printf %s "$1" | pbcopy', "sh", token.text]);
        notify({ title: "Copied", body: token.text, timeout: 2000 });
      }
    });
  }

  key(e, cx) {
    if (e.key === "escape" || e.keystroke === "ctrl-c") return this.close();
    if (e.key === "backspace") {
      this.typed = this.typed.slice(0, -1);
      return cx.notify();
    }
    const ch = e.key_char;
    if (!ch || ch.length !== 1 || !/[a-z]/.test(ch)) return;
    const typed = this.typed + ch;
    const exact = this.tokens.find((t) => t.hint === typed);
    if (exact) return this.choose(exact, cx);
    if (this.tokens.some((t) => t.hint.startsWith(typed))) this.typed = typed;
    cx.notify();
  }

  segments(line, index, t) {
    const marks = [];
    for (const token of this.tokens) {
      if (!token.hint.startsWith(this.typed)) continue;
      for (const s of token.spans)
        if (s.line === index) marks.push({ ...s, token });
    }
    marks.sort((a, b) => a.start - b.start);
    const out = [];
    let at = 0;
    for (const m of marks) {
      if (m.start < at) continue;
      if (m.start > at)
        out.push(
          div()
            .text_color(t.dim)
            .child(visible(line.slice(at, m.start))),
        );
      const hint = m.token.hint;
      const rest = line.slice(
        m.start + Math.min(hint.length, m.end - m.start),
        m.end,
      );
      out.push(
        div().bg(t.ansi3).text_color(t.background).font_weight(800).child(hint),
      );
      if (rest)
        out.push(
          div().text_color(t.foreground).font_weight(600).child(visible(rest)),
        );
      at = m.end;
    }
    if (at < line.length)
      out.push(
        div()
          .text_color(t.dim)
          .child(visible(line.slice(at))),
      );
    if (out.length === 0) out.push(div().child(" "));
    return out;
  }

  render(cx) {
    const t = theme();
    const s = settings();
    const font = s.font_family ?? "Menlo";
    const size = Number(s.font_size ?? 13);
    const title = this.mode === "url" ? "OPEN URL" : "PLUCK";
    const lines = this.lines ?? [];
    return v_flex()
      .id("pluck-root")
      .size_full()
      .bg(t.background)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        cx.stop_propagation();
        this.key(e, cx);
      })
      .on_mouse_down("left", (_e, cx) => {
        cx.stop_propagation();
        this.close();
      })
      .child(
        v_flex()
          .flex_1()
          .min_h(0)
          .overflow_hidden()
          .justify_end()
          .px(14)
          .pt(40)
          .pb(8)
          .font_family(font)
          .text_size(size)
          .children(
            this.lines === null
              ? [div().text_color(t.muted).child("Reading terminal…")]
              : lines.map((line, i) =>
                  h_flex()
                    .whitespace_nowrap()
                    .children(this.segments(line, i, t)),
                ),
          ),
      )
      .child(
        h_flex()
          .justify_end()
          .px(14)
          .pb(14)
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
              .child(div().text_color(t.accent).font_semibold().child(title))
              .child(
                div()
                  .text_color(t.dim)
                  .child(
                    this.typed
                      ? `hint: ${this.typed}`
                      : "type a hint · esc to cancel",
                  ),
              ),
          ),
      );
  }
}
