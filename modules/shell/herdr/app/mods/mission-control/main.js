import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import {
  terminals,
  agents,
  run,
  hc,
  theme,
  settings,
  props,
  closeOverlay,
  checking,
} from "herdr";

const REFRESH_MS = 1000;
const MAX_LINES = 60;

function nbsp(text) {
  return text.replace(/ /g, " ");
}

function watched() {
  return terminals().filter((t) => t.state === "live" && t.status);
}

function describe(t) {
  const agent = agents().find((a) => a.address === t.address);
  if (agent)
    return { title: agent.space_name, detail: agent.tab_name, pane: true };
  return { title: t.name ?? t.address, detail: "helper", pane: false };
}

export default class MissionControl extends View {
  init(_props, cx) {
    this.screens = {};
    this.handle = cx.focus_handle();
    this.handle.focus();
    if (checking()) return;
    this.refresh(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.refresh(cx));
  }

  refresh(cx) {
    if (this.busy) return;
    this.busy = true;
    cx.spawn(async (cx) => {
      const list = watched();
      const reads = await Promise.all(
        list.map((t) =>
          hc(["read", t.address, "--json"]).then(
            (r) => [t.address, r],
            () => [t.address, null],
          ),
        ),
      );
      for (const [address, r] of reads) {
        if (!r || r.code !== 0) continue;
        const lines = (JSON.parse(r.stdout).text ?? "").split("\n");
        while (lines.length && lines[lines.length - 1].trim() === "")
          lines.pop();
        this.screens[address] = lines.slice(-MAX_LINES);
      }
      this.busy = false;
      cx.notify();
    });
  }

  go(address) {
    closeOverlay(props().overlay);
    if (agents().some((a) => a.address === address))
      run("agent.focus", { terminal: address });
    else run("terminal.peek", { terminal: address });
  }

  key(e) {
    if (e.key === "escape") return closeOverlay(props().overlay);
    const list = watched();
    if (/^[1-9]$/.test(e.key)) {
      const t = list[Number(e.key) - 1];
      if (t) this.go(t.address);
      return;
    }
    if (e.key === "enter") {
      const t =
        list.find((x) => x.status === "blocked") ??
        list.find((x) => x.status === "done");
      if (t) this.go(t.address);
    }
  }

  tile(t, index, th, font, size, w, h) {
    const info = describe(t);
    const color =
      { blocked: th.ansi1, working: th.ansi3, done: th.ansi4 }[t.status] ??
      th.border;
    const fit = Math.max(1, Math.floor((h - 44) / (size * 1.32)));
    const lines = this.screens[t.address]?.slice(-fit);
    const body =
      lines === undefined
        ? [div().text_color(th.dim).child("…")]
        : lines.map((line) =>
            div()
              .whitespace_nowrap()
              .child(line ? nbsp(line) : " "),
          );
    return v_flex()
      .id(`mc-${t.address}`)
      .w(w)
      .h(h)
      .flex_none()
      .rounded(8)
      .overflow_hidden()
      .bg(th.background)
      .border_2()
      .border_color(color)
      .cursor_pointer()
      .hover((s) => s.border_color(th.foreground))
      .on_click(() => this.go(t.address))
      .child(
        h_flex()
          .flex_none()
          .gap(8)
          .px(10)
          .py(5)
          .items_center()
          .bg(th.chrome)
          .border_b(1)
          .border_color(th.border)
          .text_size(12)
          .child(
            div()
              .w(8)
              .h(8)
              .rounded(4)
              .bg(color)
              .when(t.status === "blocked", (el) =>
                el.loop("opacity", { from: 0.35, to: 1, period: 700 }),
              ),
          )
          .child(
            div()
              .text_color(th.foreground)
              .font_semibold()
              .truncate()
              .child(info.title),
          )
          .child(div().text_color(th.dim).truncate().child(info.detail))
          .child(div().flex_1())
          .child(div().text_color(color).child(t.status))
          .child(
            div()
              .text_color(th.dim)
              .child(index < 9 ? String(index + 1) : ""),
          ),
      )
      .child(
        v_flex()
          .flex_1()
          .min_h(0)
          .overflow_hidden()
          .justify_start()
          .px(8)
          .py(6)
          .font_family(font)
          .text_size(size)
          .text_color(th.foreground)
          .children(body),
      );
  }

  render(cx) {
    const th = theme();
    const s = settings();
    const font = s.font_family ?? "Menlo";
    const list = watched();
    const cols = Math.max(1, Math.min(4, Math.ceil(Math.sqrt(list.length))));
    const rows = [];
    for (let i = 0; i < list.length; i += cols)
      rows.push(list.slice(i, i + cols));
    const size = Math.max(
      8,
      Math.round(Number(s.font_size ?? 13) * (cols >= 3 ? 0.62 : 0.75)),
    );
    const blocked = list.filter((t) => t.status === "blocked").length;
    const area = this.size ?? { width: 1200, height: 800 };
    const gap = 10;
    const tileW = Math.floor((area.width - 28 - gap * (cols - 1)) / cols);
    const tileH = Math.floor(
      (area.height - 28 - 30 - gap * Math.max(0, rows.length - 1)) /
        Math.max(1, rows.length),
    );
    return v_flex()
      .id("mission-control")
      .size_full()
      .on_size_change((size, cx) => {
        this.size = size;
        cx.notify();
      })
      .p(14)
      .gap(10)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        cx.stop_propagation();
        this.key(e);
      })
      .child(
        h_flex()
          .gap(12)
          .items_center()
          .text_size(13)
          .child(
            div()
              .text_color(th.foreground)
              .font_semibold()
              .child("Mission control"),
          )
          .child(div().text_color(th.dim).child(`${list.length} agents`))
          .when(blocked > 0, (el) =>
            el.child(div().text_color(th.ansi1).child(`${blocked} need you`)),
          )
          .child(div().flex_1())
          .child(
            div()
              .text_color(th.dim)
              .child(
                "click or 1–9 to jump · enter: first waiting · esc to close",
              ),
          ),
      )
      .child(
        list.length === 0
          ? div().text_color(th.dim).child("No agents running")
          : v_flex()
              .flex_1()
              .min_h(0)
              .overflow_hidden()
              .gap(gap)
              .children(
                rows.map((row, r) =>
                  h_flex()
                    .flex_none()
                    .h(tileH)
                    .gap(gap)
                    .children(
                      row.map((t, c) =>
                        this.tile(
                          t,
                          r * cols + c,
                          th,
                          font,
                          size,
                          tileW,
                          tileH,
                        ),
                      ),
                    ),
                ),
              ),
      );
  }
}
