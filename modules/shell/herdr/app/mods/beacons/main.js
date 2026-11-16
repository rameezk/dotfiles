import { View, div, svg } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { terminals, agents, run, theme } from "herdr";

const FLAME_SIZE = { blocked: 17, done: 10, working: 8, idle: 0 };

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

function beacon(t, theme) {
  const status = t.status in FLAME_SIZE ? t.status : "idle";
  const size = FLAME_SIZE[status];
  const blocked = status === "blocked";
  const flame = h_flex()
    .relative()
    .justify_center()
    .w(16)
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
    .h(26)
    .cursor_pointer()
    .tooltip(`${label(t)} · ${status}`)
    .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
    .on_click(() => show(t.address))
    .child(flame)
    .child(
      svg("mods/beacons/brazier.svg")
        .w(14)
        .h(9)
        .text_color(status === "idle" ? theme.dim : theme.muted),
    );
}

export default class Beacons extends View {
  render() {
    const t = theme();
    const watched = terminals().filter((x) => x.state === "live" && x.status);
    if (watched.length === 0) return div();
    const lit = watched.filter((x) => x.status === "blocked").length;
    return h_flex()
      .id("beacons")
      .items_end()
      .gap(6)
      .px(8)
      .pt(2)
      .pb(3)
      .rounded(8)
      .bg(t.chrome)
      .border_1()
      .border_color(lit > 0 ? t.ansi1 : t.border)
      .children(watched.map((x) => beacon(x, t)))
      .when(lit > 0, (el) =>
        el.child(
          div()
            .pl(2)
            .pb(1)
            .text_size(11)
            .font_semibold()
            .text_color(t.ansi1)
            .child(String(lit)),
        ),
      );
  }
}
