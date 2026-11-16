import { View, div, svg } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { terminals, agents, run, theme, layout } from "herdr";

const FLAME_SIZE = { blocked: 30, done: 18, working: 14, idle: 0 };
const BEACON_WIDTH = 28;
const BEACON_HEIGHT = 46;
const BRAZIER = { w: 24, h: 16 };
const GAP = 12;
const PAD_X = 12;
const COUNT_TEXT = 16;
const MIN_SCALE = 0.4;
const SIDEBAR_MARGIN = 16;

function find(node, id) {
  if (node?.id === id) return node;
  for (const child of node?.children ?? []) {
    const found = find(child, id);
    if (found) return found;
  }
  return null;
}

function fitScale(count, lit) {
  const sidebar = find(layout(), "sidebar")?.size?.px;
  if (!sidebar) return 1;
  const available = sidebar - SIDEBAR_MARGIN;
  const digits = lit > 0 ? String(lit).length : 0;
  const scalable =
    count * BEACON_WIDTH +
    (count - 1) * GAP +
    2 * PAD_X +
    digits * COUNT_TEXT * 0.65;
  const fixed = 2 + (lit > 0 ? 2 : 0);
  return Math.max(MIN_SCALE, Math.min(1, (available - fixed) / scalable));
}

function px(value, scale) {
  return Math.round(value * scale);
}

function show(address) {
  if (agents().some((a) => a.address === address)) {
    run("agent.focus", { terminal: address });
  } else {
    run("terminal.peek", { terminal: address });
  }
}

function label(t) {
  const agent = agents().find((a) => a.address === t.address);
  if (agent) return `${agent.space_name} · ${agent.tab_name}`;
  return t.name ?? t.title ?? t.address;
}

function flameIcon(file, size, color) {
  return svg(`mods/beacons/${file}`)
    .absolute()
    .bottom(0)
    .w(size)
    .h(size)
    .text_color(color);
}

function beacon(t, theme, scale) {
  const status = t.status in FLAME_SIZE ? t.status : "idle";
  const size = px(FLAME_SIZE[status], scale);
  const blocked = status === "blocked";
  const flame = h_flex()
    .relative()
    .justify_center()
    .w(px(BEACON_WIDTH, scale))
    .h(size)
    .transition("height", 600);
  if (blocked) {
    flame
      .child(
        flameIcon("flame.svg", size, theme.ansi1).loop("opacity", {
          from: 0.7,
          to: 1,
          period: 520,
        }),
      )
      .child(
        flameIcon("core.svg", size, theme.ansi3).loop("opacity", {
          from: 0.45,
          to: 1,
          period: 380,
        }),
      );
  } else if (status === "working") {
    flame.child(
      flameIcon("flame.svg", size, theme.ansi3).loop("opacity", {
        from: 0.4,
        to: 0.85,
        period: 900,
      }),
    );
  } else if (status === "done") {
    flame.child(flameIcon("flame.svg", size, theme.ansi3).opacity(0.85));
  }
  return v_flex()
    .id(`beacon-${t.address}`)
    .items_center()
    .justify_end()
    .h(px(BEACON_HEIGHT, scale))
    .cursor_pointer()
    .tooltip(`${label(t)} · ${status}`)
    .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
    .on_click(() => show(t.address))
    .child(flame)
    .child(
      svg("mods/beacons/brazier.svg")
        .w(px(BRAZIER.w, scale))
        .h(px(BRAZIER.h, scale))
        .text_color(status === "idle" ? theme.dim : theme.muted),
    );
}

export default class Beacons extends View {
  render() {
    const t = theme();
    const watched = terminals().filter((x) => x.state === "live" && x.status);
    if (watched.length === 0) return div();
    const lit = watched.filter((x) => x.status === "blocked").length;
    const scale = fitScale(watched.length, lit);
    return h_flex()
      .id("beacons")
      .items_end()
      .flex_wrap()
      .gap(px(GAP, scale))
      .px(px(PAD_X, scale))
      .pt(px(4, scale))
      .pb(px(5, scale))
      .rounded(8)
      .bg(t.chrome)
      .border_1()
      .border_color(lit > 0 ? t.ansi1 : t.border)
      .children(watched.map((x) => beacon(x, t, scale)))
      .when(lit > 0, (el) =>
        el.child(
          div()
            .pl(2)
            .pb(1)
            .text_size(px(COUNT_TEXT, scale))
            .font_semibold()
            .text_color(t.ansi1)
            .child(String(lit)),
        ),
      );
  }
}
