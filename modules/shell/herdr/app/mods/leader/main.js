import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import {
  run,
  props,
  closeOverlay,
  openOverlay,
  theme,
  agents,
  terminals,
  notify,
  focused,
  spaces,
} from "herdr";

const EDIT_SCROLLBACK =
  'd="$(mktemp -d -t herdr-scrollback)" && f="$d/scrollback.txt" && hc read "$1" --lines 100000 > "$f" || exit 1; e="${EDITOR:-vim}"; case "$e" in *vi|*vim) "$e" + "$f";; *) "$e" "$f";; esac; rm -rf "$d"';

function editScrollback() {
  const f = focused();
  if (!f?.terminal) return;
  const machine = spaces().find((s) => s.id === f.space)?.machine ?? null;
  if (machine)
    return notify({
      title: "Edit scrollback only works on this computer",
      timeout: 2500,
    });
  run("pane.split", {
    direction: "right",
    argv: ["sh", "-c", EDIT_SCROLLBACK, "sh", f.terminal],
  });
  run("pane.zoom", {});
}

const OPEN_HUNK = 'cd "$1" && shift && exec hunk "$@"';

function hunkDiff() {
  const cwd = focused()?.cwd;
  if (!cwd)
    return notify({ title: "The focused pane has no folder", timeout: 2000 });
  run("pane.split", {
    direction: "right",
    argv: ["sh", "-c", OPEN_HUNK, "sh", cwd, "diff", "--watch"],
  });
  run("pane.zoom", {});
}

function newTab() {
  run("tab.new", {});
  run("tab.rename", {});
}

function newWorktree() {
  const space = focused()?.space;
  if (space)
    openOverlay({
      mod: "worktree",
      kind: "dialog",
      width: 440,
      height: 260,
      props: { space, action: "new" },
    });
}

function pluck(mode) {
  return () =>
    openOverlay({
      mod: "pluck",
      props: { mode },
      kind: "dialog",
      width: 760,
      height: 0.5,
    });
}

function waiting(exclude) {
  const list = terminals().filter((t) => t.address !== exclude);
  return [
    ...list.filter((t) => t.status === "blocked"),
    ...list.filter((t) => t.status === "done"),
  ];
}

function show(address) {
  if (agents().some((a) => a.address === address)) {
    run("agent.focus", { terminal: address });
  } else {
    run("terminal.peek", { terminal: address });
  }
}

function closePeek() {
  run("overlay.close", { id: "terminal-peek" });
}

function keepPeek() {
  const terminal = focused()?.terminal;
  if (!terminal) return;
  if (agents().some((a) => a.address === terminal)) {
    run("pane.focus", { terminal });
  } else {
    run("terminal.keep", { terminal });
  }
  closePeek();
}

function triage() {
  const next = waiting(focused()?.terminal ?? null)[0];
  if (next) {
    run("terminal.peek", { terminal: next.address });
    return;
  }
  closePeek();
  notify({
    title: "All caught up",
    body: "No agents waiting on you",
    timeout: 2000,
  });
}

function nextWaiting() {
  const order = terminals().map((t) => t.address);
  const here = order.indexOf(focused()?.terminal ?? "");
  const rank = (address) =>
    (order.indexOf(address) - here + order.length) % order.length;
  const list = waiting(focused()?.terminal ?? null);
  const pick = (status) =>
    list
      .filter((t) => t.status === status)
      .sort((a, b) => rank(a.address) - rank(b.address))[0];
  const target = pick("blocked") ?? pick("done");
  if (target) show(target.address);
  else notify({ title: "No agents waiting on you", timeout: 2000 });
}

const BINDINGS = {
  left: ["pane.go", { direction: "left" }],
  down: ["pane.go", { direction: "down" }],
  up: ["pane.go", { direction: "up" }],
  right: ["pane.go", { direction: "right" }],
  h: ["pane.go", { direction: "left" }],
  j: ["pane.go", { direction: "down" }],
  k: ["pane.go", { direction: "up" }],
  l: ["pane.go", { direction: "right" }],
  "shift-h": ["pane.swap", { direction: "left" }],
  "shift-j": ["pane.swap", { direction: "down" }],
  "shift-k": ["pane.swap", { direction: "up" }],
  "shift-l": ["pane.swap", { direction: "right" }],
  "shift-down": ["agent.next", {}],
  "shift-up": ["agent.prev", {}],
  v: ["pane.split", { direction: "right" }],
  "-": ["pane.split", { direction: "down" }],
  x: ["pane.close", {}],
  z: ["pane.zoom", {}],
  c: newTab,
  n: ["tab.next", {}],
  "shift-t": ["tab.rename", {}],
  "shift-x": ["tab.close", {}],
  "shift-n": ["space.new", {}],
  "shift-w": ["space.rename", {}],
  "shift-d": ["space.close", {}],
  b: ["layout.toggle", { id: "sidebar" }],
  s: ["app.settings", {}],
  "?": ["app.help", {}],
  "/": ["terminal.search", {}],
  f: pluck("url"),
  p: pluck("pluck"),
  g: ["mod.open", { name: "picker" }],
  "[": () =>
    openOverlay({
      mod: "copy-mode",
      kind: "popup",
      frame: "none",
      width: 1,
      height: 1,
      at: { x: 0, y: 0 },
    }),
  e: editScrollback,
  "shift-g": newWorktree,
  q: ["app.hide", {}],
  space: () =>
    openOverlay({
      mod: "mission-control",
      kind: "dialog",
      width: 0.96,
      height: 0.94,
    }),
  d: hunkDiff,
  w: () =>
    openOverlay({ mod: "branches", kind: "dialog", width: 680, height: 0.7 }),
  r: ["mod.open", { name: "beacon-prs" }],
  "shift-a": ["mod.open", { name: "appearance" }],
  t: triage,
  enter: keepPeek,
  o: nextWaiting,
  a: nextWaiting,
  ...Object.fromEntries(
    [1, 2, 3, 4, 5, 6, 7, 8, 9].map((i) => [
      String(i),
      ["tab.select", { index: i }],
    ]),
  ),
};

function lookup(e) {
  const char = e.key_char && e.key_char.length === 1 ? e.key_char : null;
  const shifted =
    char &&
    !e.modifiers.control &&
    !e.modifiers.alt &&
    !e.modifiers.platform &&
    /[^a-zA-Z0-9]/.test(char);
  if (shifted && BINDINGS[char]) return BINDINGS[char];
  return BINDINGS[e.keystroke];
}

export default class Leader extends View {
  init(_props, cx) {
    this.handle = cx.focus_handle();
    this.handle.focus();
  }

  finish(binding, cx) {
    closeOverlay(props().overlay);
    if (typeof binding === "function") binding(cx);
    else if (binding) run(binding[0], binding[1]);
  }

  render(cx) {
    const t = theme();
    return v_flex()
      .id("leader-root")
      .size_full()
      .justify_end()
      .items_center()
      .pb(28)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        cx.stop_propagation();

        this.finish(lookup(e), cx);
      })
      .on_mouse_down("left", (_e, cx) => {
        cx.stop_propagation();
        this.finish(null, cx);
      })
      .child(
        h_flex()
          .gap(8)
          .px(12)
          .py(5)
          .rounded(8)
          .bg(t.popover)
          .border_1()
          .border_color(t.accent)
          .text_size(13)
          .child(div().text_color(t.accent).font_semibold().child("PREFIX"))
          .child(div().text_color(t.muted).child("waiting for key")),
      );
  }
}
