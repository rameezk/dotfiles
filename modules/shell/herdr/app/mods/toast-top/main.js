import { View, div, image } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { props, closeOverlay, openOverlay, run, settings, theme } from "herdr";

const MOD = "toast-top";
const WIDTH = 380;
const TOP = 12;
const TIMEOUT = 6000;
const DOT = { error: "blocked", warning: "working", success: "done", info: "accent" };

function notification(p) {
  const { overlay, stage, ...rest } = p;
  return rest;
}

export default class Toast extends View {
  init(_props, cx) {
    const p = props() ?? {};
    if (!p.stage && p.overlay) {
      openOverlay({
        mod: MOD,
        kind: "window",
        frame: "none",
        width: 1,
        height: 1,
        at: { x: 0, y: 0 },
        props: { ...notification(p), stage: "measure" },
      });
      closeOverlay(p.overlay);
      return;
    }
    if (p.stage === "card") {
      const timeout = p.timeout === null ? null : p.timeout ?? TIMEOUT;
      if (timeout) cx.timer.after(timeout, () => this.dismiss());
    }
  }

  measured(width) {
    if (this.relayed) return;
    this.relayed = true;
    const p = props() ?? {};
    const zoom = Number(settings()?.zoom) || 1;
    openOverlay({
      mod: MOD,
      kind: "window",
      width: WIDTH,
      at: { x: Math.max(0, Math.round((width - WIDTH * zoom) / 2)), y: Math.round(TOP * zoom) },
      props: { ...notification(p), stage: "card" },
    });
    closeOverlay(p.overlay);
  }

  dismiss() {
    if (this.dismissed) return;
    this.dismissed = true;
    closeOverlay(props()?.overlay);
  }

  render(cx) {
    const p = props() ?? {};
    if (p.stage === "measure") {
      return div().size_full().on_size_change((size) => {
        if (size.width > 1) this.measured(size.width);
      });
    }
    if (p.stage !== "card") return div();
    const t = theme();
    const login = /^([a-z0-9][a-z0-9_.-]{0,31}) needs login; run hc machine login \1$/.exec(p.body ?? "");
    const command = login ? `hc machine login ${login[1]}` : null;
    return h_flex()
      .id("toast")
      .w_full()
      .min_h(56)
      .px(18)
      .py(10)
      .gap(12)
      .items_center()
      .bg(t.popover)
      .cursor_pointer()
      .hover((s) => s.bg(t.hover))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click(() => {
        if (p.action?.name) run(p.action.name, p.action.args ?? {});
        this.dismiss();
      })
      .child(p.icon ? image(p.icon).w(18).h(18).flex_none() : div().size(8).rounded(4).flex_none().bg(t[DOT[p.level] ?? "accent"]))
      .child(
        v_flex()
          .flex_1()
          .overflow_hidden()
          .child(div().truncate().font_semibold().text_size(13).text_color(t.foreground).child(login ? `${login[1]} needs a login: run` : p.title ?? ""))
          .when(!login && Boolean(p.body), (el) => el.child(div().truncate().text_size(12).text_color(t.muted).child(p.body ?? "")))
          .when(Boolean(login), (el) => el.child(h_flex().gap(8).items_center()
            .child(div().flex_1().min_w(0).truncate().tooltip(command).text_size(12).text_color(t.muted).child(command))
            .child(div().id("toast-login-copy").flex_none().px(6).py(3).rounded(4).bg(t.raised)
              .text_size(12).text_color(t.foreground).cursor_pointer().hover(s => s.bg(t.hover))
              .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
              .on_click((_e, cx) => { cx.stop_propagation(); cx.write_to_clipboard(command); this.copied = true; cx.notify(); })
              .child(this.copied ? "Copied" : "Copy")))),
      );
  }
}
