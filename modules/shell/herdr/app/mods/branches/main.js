import { View, div } from "gpui-kit";
import { v_flex, h_flex, ScrollHandle } from "gpui-base";
import {
  exec,
  focused,
  spaces,
  terminals,
  panes,
  run,
  theme,
  props,
  closeOverlay,
  openOverlay,
  notify,
  checking,
} from "herdr";

const OPEN_HUNK = 'cd "$1" && shift && exec hunk "$@"';

function base(path) {
  return path.split("/").filter(Boolean).pop() ?? path;
}

function inside(path, folder) {
  return (
    path === folder ||
    path.startsWith(folder.endsWith("/") ? folder : `${folder}/`)
  );
}

function parseWorktrees(text) {
  const out = [];
  let cur = null;
  for (const line of text.split("\n")) {
    if (line.startsWith("worktree ")) {
      cur = { path: line.slice(9), branch: null, detached: false };
      out.push(cur);
    } else if (cur && line.startsWith("branch ")) {
      cur.branch = line.slice(7).replace(/^refs\/heads\//, "");
    } else if (cur && line === "detached") {
      cur.detached = true;
    }
  }
  return out;
}

function openHunk(folder, args) {
  run("pane.split", {
    direction: "right",
    argv: ["sh", "-c", OPEN_HUNK, "sh", folder, ...args],
  });
  run("pane.zoom", {});
}

export default class Branches extends View {
  init(_props, cx) {
    this.items = null;
    this.error = null;
    this.selected = 0;
    this.busy = false;
    this.scroll = ScrollHandle.new();
    this.handle = cx.focus_handle();
    this.handle.focus();
    const f = focused();
    this.space = f?.space ?? null;
    this.cwd = f?.cwd ?? null;
    this.machine = spaces().find((s) => s.id === this.space)?.machine ?? null;
    if (checking()) return;
    if (!this.cwd) {
      this.error = "The focused pane has no folder";
      return;
    }
    cx.spawn(async (cx) => {
      try {
        await this.load();
      } catch (e) {
        this.error = String(e?.message ?? e);
      }
      cx.notify();
    });
  }

  git(args, cwd) {
    return exec("git", args, { cwd: cwd ?? this.cwd, on: this.machine });
  }

  async load() {
    const top = await this.git(["rev-parse", "--show-toplevel"]);
    if (top.code !== 0) throw new Error("Not inside a git repository");
    const [wt, refs] = await Promise.all([
      this.git(["worktree", "list", "--porcelain"]),
      this.git([
        "for-each-ref",
        "--sort=-committerdate",
        "--format=%(refname:short)%09%(committerdate:relative)%09%(subject)",
        "refs/heads",
      ]),
    ]);
    const worktrees = parseWorktrees(wt.stdout);
    this.current = worktrees
      .filter((w) => inside(this.cwd, w.path))
      .sort((a, b) => b.path.length - a.path.length)[0] ?? {
      path: top.stdout.trim(),
      branch: null,
    };
    const dirty = await Promise.all(
      worktrees.map((w) =>
        this.git(
          ["status", "--porcelain", "--untracked-files=no"],
          w.path,
        ).then(
          (r) => r.stdout.trim() !== "",
          () => false,
        ),
      ),
    );
    const where = new Map(
      panes()
        .filter((p) => p.terminal && p.cwd)
        .map((p) => [p.terminal, p.cwd]),
    );
    const live = terminals()
      .filter(
        (t) =>
          t.state === "live" &&
          (t.status === "working" || t.status === "blocked"),
      )
      .map((t) => where.get(t.address) ?? t.cwd)
      .filter(Boolean);
    const busy = (path) => live.some((cwd) => inside(cwd, path));
    const taken = new Set(worktrees.map((w) => w.branch).filter(Boolean));
    const items = worktrees.map((w, i) => ({
      kind: "worktree",
      path: w.path,
      branch: w.branch ?? (w.detached ? "detached" : "?"),
      dirty: dirty[i],
      agent: busy(w.path),
      current: w.path === this.current.path,
    }));
    for (const line of refs.stdout.split("\n").filter(Boolean)) {
      const [name, when, subject] = line.split("\t");
      if (taken.has(name)) continue;
      items.push({ kind: "branch", branch: name, when, subject });
    }
    this.items = items;
  }

  close() {
    closeOverlay(props().overlay);
  }

  move(by, cx) {
    if (!this.items?.length) return;
    this.selected =
      (this.selected + by + this.items.length) % this.items.length;
    const headers = this.items[this.selected].kind === "branch" ? 2 : 1;
    this.scroll.scroll_to_item(this.selected + headers);
    cx.notify();
  }

  goWorktree(item) {
    const space = spaces().find(
      (s) =>
        s.folder &&
        s.folder === item.path &&
        (s.machine ?? null) === this.machine,
    );
    if (space) run("space.select", { id: space.id });
    else
      run("space.new", {
        name: base(item.path),
        folder: item.path,
        machine: this.machine ?? "",
      });
    this.close();
  }

  async switchBranch(item, cx) {
    const cur = this.items.find((i) => i.kind === "worktree" && i.current);
    if (cur?.dirty)
      return this.refuse(
        "This worktree has uncommitted changes. Press n for a new worktree instead.",
        cx,
      );
    if (cur?.agent)
      return this.refuse(
        "An agent is working in this worktree. Press n for a new worktree instead.",
        cx,
      );
    this.busy = true;
    cx.notify();
    const r = await this.git(["switch", item.branch], this.current.path);
    this.busy = false;
    if (r.code !== 0)
      return this.refuse(
        (r.stderr || r.stdout).trim() || "git switch failed",
        cx,
      );
    notify({ title: `Switched to ${item.branch}`, timeout: 2000 });
    this.close();
  }

  async diff(item, cx) {
    if (item.kind === "worktree") {
      this.close();
      openHunk(item.path, ["diff", "--watch"]);
      return;
    }
    const mb = await this.git(
      ["merge-base", "HEAD", item.branch],
      this.current.path,
    );
    if (mb.code !== 0)
      return this.refuse(`No common history with ${item.branch}`, cx);
    this.close();
    openHunk(this.current.path, ["diff", mb.stdout.trim(), item.branch]);
  }

  newWorktree(item) {
    this.close();
    openOverlay({
      mod: "worktree",
      kind: "dialog",
      width: 440,
      height: 260,
      props: { space: this.space, branch: item.branch },
    });
  }

  refuse(message, cx) {
    this.message = message;
    cx.notify();
  }

  key(e, cx) {
    if (this.busy) return;
    const k = e.key;
    if (k === "escape") return this.close();
    if (k === "down" || k === "j") return this.move(1, cx);
    if (k === "up" || k === "k") return this.move(-1, cx);
    const item = this.items?.[this.selected];
    if (!item) return;
    this.message = null;
    if (k === "enter") {
      if (item.kind === "worktree") return this.goWorktree(item);
      return cx.spawn(async (cx) => this.switchBranch(item, cx));
    }
    if (k === "d") return cx.spawn(async (cx) => this.diff(item, cx));
    if (k === "n" && item.kind === "branch") return this.newWorktree(item);
  }

  row(item, i, t) {
    const on = i === this.selected;
    const tag = (text, color) =>
      div()
        .px(5)
        .rounded(4)
        .text_size(10.5)
        .text_color(color)
        .border_1()
        .border_color(color)
        .child(text);
    const left =
      item.kind === "worktree"
        ? h_flex()
            .gap(8)
            .min_w(0)
            .child(
              div()
                .text_color(item.current ? t.accent : t.foreground)
                .font_semibold()
                .child(item.branch),
            )
            .child(div().text_color(t.dim).truncate().child(base(item.path)))
        : h_flex()
            .gap(8)
            .min_w(0)
            .child(div().text_color(t.foreground).child(item.branch))
            .child(
              div()
                .text_color(t.dim)
                .truncate()
                .child(item.subject ?? ""),
            );
    const right = h_flex().gap(6).flex_none().items_center();
    if (item.kind === "worktree") {
      if (item.current) right.child(tag("here", t.accent));
      if (item.dirty) right.child(tag("changes", t.ansi3));
      if (item.agent) right.child(tag("agent", t.ansi1));
    } else {
      right.child(
        div()
          .text_size(11)
          .text_color(t.dim)
          .child(item.when ?? ""),
      );
    }
    return h_flex()
      .id(`br-${i}`)
      .justify_between()
      .gap(10)
      .px(10)
      .py(6)
      .rounded(6)
      .when(on, (el) => el.bg(t.selected))
      .hover((s) => s.bg(t.hover))
      .cursor_pointer()
      .on_click((_e, cx) => {
        this.selected = i;
        cx.notify();
      })
      .child(left)
      .child(right);
  }

  render(cx) {
    const t = theme();
    const items = this.items ?? [];
    const header = (text) =>
      div().px(10).pt(10).pb(4).text_size(11).text_color(t.dim).child(text);
    const children = [];
    items.forEach((item, i) => {
      if (i === 0 && item.kind === "worktree")
        children.push(header("WORKTREES"));
      if (item.kind === "branch" && (i === 0 || items[i - 1].kind !== "branch"))
        children.push(header("BRANCHES"));
      children.push(this.row(item, i, t));
    });
    const sel = items[this.selected];
    const hints =
      sel?.kind === "branch"
        ? "enter switch here · d diff vs current · n new worktree · esc"
        : "enter go to space · d diff in Hunk · esc";
    return v_flex()
      .id("branches")
      .size_full()
      .p(12)
      .gap(8)
      .text_size(13)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        cx.stop_propagation();
        this.key(e, cx);
      })
      .child(
        h_flex()
          .justify_between()
          .child(
            div()
              .font_semibold()
              .text_color(t.foreground)
              .child("Branches & worktrees"),
          )
          .child(
            div()
              .text_color(t.dim)
              .child(this.cwd ? base(this.current?.path ?? this.cwd) : ""),
          ),
      )
      .child(
        this.error
          ? div().text_color(t.ansi1).child(this.error)
          : this.items === null
            ? div().text_color(t.dim).child("Reading git…")
            : v_flex()
                .id("branches-list")
                .flex_1()
                .min_h(0)
                .overflow_y_scrollbar()
                .track_scroll(this.scroll)
                .children(children),
      )
      .when(Boolean(this.message), (el) =>
        el.child(div().text_color(t.ansi1).child(this.message)),
      )
      .child(
        div()
          .text_size(11.5)
          .text_color(this.busy ? t.ansi3 : t.dim)
          .child(this.busy ? "Switching…" : hints),
      );
  }
}
