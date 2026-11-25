
import { View, div, image, svg } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { agents, terminals, panes, focused, spaces, run, theme, shared, setShared, setAgentOrder, openOverlay, closeOverlay, props, window, layout, home, exec, checking, readTextFile, watchFolder } from "herdr";

const FUEL_LINE = 150000;
const SUB_LINGER_MS = 120000;
const fuelDir = () => `${home()}/.local/state/herdr-fuel`;
const fuelLine = () => props()?.line ?? FUEL_LINE;

function foldSubs(text) {
  const subs = new Map();
  for (const raw of text.split("\n")) {
    let e;
    try { e = JSON.parse(raw); } catch { continue; }
    if (typeof e?.id !== "string") continue;
    const s = subs.get(e.id) ?? { id: e.id, status: "working" };
    if (e.ev === "start") Object.assign(s, { type: e.type, status: "working", at: e.at });
    if (e.ev === "stop") Object.assign(s, { status: "done", ended: e.at });
    if (typeof e.desc === "string" && e.desc) s.desc = e.desc;
    if (typeof e.model === "string" && e.model) s.model = e.model;
    subs.set(e.id, s);
  }
  return [...subs.values()].filter((s) => s.at);
}

function modelLabel(id) {
  if (!id) return null;
  const m = id.replace(/\[[^\]]*\]$/, "").match(/^claude-([a-z]+)-(\d+)(?:-(\d{1,2}))?(?:-\d{8})?$/);
  if (!m) return id;
  return `${m[1][0].toUpperCase()}${m[1].slice(1)} ${m[2]}${m[3] ? `.${m[3]}` : ""}`;
}

function fuelZone(tokens, line) {
  if (typeof tokens !== "number") return null;
  if (tokens >= line) return "dumb";
  if (tokens >= line * 0.75) return "warm";
  return "sharp";
}

function fuelColor(t, zone) {
  if (zone === "sharp") return t.ansi3;
  if (zone === "warm") return t.ansi6;
  return t.foreground;
}

function tokensLabel(n) {
  if (n >= 1000000) return `${(n / 1000000).toFixed(1).replace(/\.0$/, "")}M`;
  if (n >= 1000) return `${Math.round(n / 1000)}k`;
  return String(n);
}

function fuelText(tokens, line, zone) {
  const gap = tokensLabel(Math.abs(line - tokens));
  if (zone === "dumb") return `${gap} into the dumb zone`;
  if (zone === "warm") return `${gap} left, wrap up`;
  return `${gap} before the dumb zone`;
}

function find(node, id) {
  if (node?.id === id) return node;
  for (const child of node?.children ?? []) {
    const found = find(child, id);
    if (found) return found;
  }
  return null;
}

const narrow = () => (find(layout(), "sidebar")?.size?.px ?? Infinity) < 160;

function toggleSidebar() {
  const width = find(layout(), "sidebar")?.size?.px;
  if (!narrow()) setShared("sidebar.width", width);
  setShared("sidebar.collapsed", !narrow());
  run("layout.resize", { id: "sidebar", px: narrow() ? (shared("sidebar.width") ?? 268) : window()?.platform === "macos" ? 76 : 44 });
}

const clean = (s) => (s ?? "").replace(/^[^\p{L}\p{N}\p{Ps}\p{Pi}]+/u, "").replace(/^\[ [!.] \] /, "").trim();

function leadPanes(all) {
  const leads = new Map();
  for (const p of all) {
    if (!p.terminal) continue;
    const current = leads.get(p.tab);
    if (!current || (!current.program?.agent && (p.program?.agent || (!current.focused && p.focused)))) leads.set(p.tab, p);
  }
  return leads;
}

function title(tab, l) {
  const prog = l?.program?.name;
  const live = clean(l?.title);
  const path = /^[~/]|^[A-Za-z]:\\/.test((l?.title ?? "").replace(/^[^\p{L}\p{N}~/]+/u, ""));
  const telling = live && live !== (l?.terminal ?? l?.address) && live !== prog && !path && !live.includes("@") && live.toLowerCase() !== l?.program?.key;
  return (tab.name === prog || !tab.named) && telling ? live : tab.name;
}
const RANK = { blocked: 4, done: 3, working: 2, idle: 1 };
const kidStatus = (k, listed) => {
  const s = listed.get(k.address) ?? k.status;
  return s === "error" ? "blocked" : s === "done" ? "idle" : s;
};
const SORTS = [
  { id: "priority", label: "Priority", icon: "clock" },
  { id: "grouped", label: "Grouped by space", icon: "layers" },
];
const sortOf = () => SORTS.find((s) => s.id === shared("agents.sort")) ?? SORTS[0];
const SECTION = { 4: "Needs you", 3: "Done", 2: "Working", 1: "Idle" };
const SECTION_STATUS = { 4: "blocked", 3: "done", 2: "working" };

function duration(ms) {
  const m = Math.floor(ms / 60000);
  if (m < 1) return "<1m";
  if (m < 60) return `${m}m`;
  const h = Math.floor(m / 60);
  return h < 24 ? `${h}h` : `${Math.floor(h / 24)}d`;
}

function icon(p, size, color) {
  if (!p?.icon) return null;
  return p.tint ? svg(p.icon).size(size).flex_none().text_color(color) : image(p.icon).w(size).h(size).flex_none();
}

export default class Agents extends View {
  init(_props, cx) {
    if (props()?.sortMenu) return;
    this.folded = new Set();
    try { this.changed = JSON.parse(localStorage.getItem("changed") ?? "{}"); } catch { this.changed = {}; }
    if (!this.changed || typeof this.changed !== "object" || Array.isArray(this.changed)) this.changed = {};
    try { this.pendingPins = JSON.parse(localStorage.getItem("pins") ?? "[]"); } catch { this.pendingPins = []; }
    if (!Array.isArray(this.pendingPins)) this.pendingPins = [];
    if (!this.pendingPins.length) localStorage.removeItem("pins");
    this.migratingPins = new Set();
    this.fuel = {};
    cx.timer.every(30000, (cx) => cx.notify());
    if (checking()) return;
    cx.spawn(async (cx) => {
      await exec("mkdir", ["-p", fuelDir()]).catch(() => null);
      await this.loadFuel();
      cx.notify();
      for (;;) {
        try {
          await watchFolder(fuelDir());
        } catch {
          return;
        }
        await this.loadFuel();
        cx.notify();
      }
    });
    cx.timer.every(15000, (cx) => {
      cx.spawn(async (cx) => {
        await this.loadFuel();
        cx.notify();
      });
    });
  }

  async loadFuel() {
    const next = {};
    const subs = {};
    for (const a of agents()) {
      if (a.address.includes("/")) continue;
      const log = await readTextFile(`${fuelDir()}/${a.address}.subagents.jsonl`).catch(() => null);
      if (log) subs[a.address] = foldSubs(log);
      const text = await readTextFile(`${fuelDir()}/${a.address}.json`).catch(() => null);
      if (!text) continue;
      try {
        const reading = JSON.parse(text);
        if (typeof reading?.tokens === "number") next[a.address] = reading;
      } catch {}
    }
    this.fuel = next;
    this.subs = subs;
  }

  virtualKids(addr) {
    const pane = this.panes?.get(addr);
    if (!pane) return [];
    const cutoff = Date.now() - SUB_LINGER_MS;
    return (this.subs?.[addr] ?? [])
      .filter((s) => s.status === "working" || (s.ended ?? 0) * 1000 > cutoff)
      .map((s) => ({ address: `${addr}~${s.id}`, name: s.desc ?? s.type ?? "Subagent", kind: [s.desc ? s.type : null, modelLabel(s.model)].filter(Boolean).join(" · ") || null, status: s.status, pane }));
  }

  publishSmoke(list) {
    const smoke = {};
    for (const a of list) {
      if (fuelZone(this.fuel[a.address]?.tokens, fuelLine()) === "dumb") smoke[a.space] = true;
    }
    const key = JSON.stringify(Object.keys(smoke).sort());
    if (key === this.smokeKey) return;
    this.smokeKey = key;
    setShared("beacon.smoke", smoke);
  }

  badge(t, zone) {
    const ring = zone === "dumb" ? t.muted : fuelColor(t, zone);
    const mark = zone === "dumb"
      ? svg("mods/beacon-agents/smoke.svg").size(8).text_color(t.foreground)
      : svg("mods/beacon-agents/flame.svg").size(zone === "warm" ? 6 : 8).text_color(fuelColor(t, zone));
    return h_flex()
      .absolute()
      .right(-4)
      .bottom(-4)
      .size(12)
      .rounded(6)
      .bg(t.background)
      .border(1)
      .border_color(ring)
      .items_center()
      .justify_center()
      .child(mark);
  }

  gauge(t, reading) {
    const line = fuelLine();
    const zone = fuelZone(reading.tokens, line);
    const color = fuelColor(t, zone);
    const scale = line * 2;
    const at = Math.min(reading.tokens / scale, 1) * 100;
    const beyond = reading.tokens > scale;
    const track = (fill, upTo) => h_flex().absolute().left(0).right(0).top(3).h(3).rounded(2).overflow_hidden()
      .child(div().h(3).w(`${upTo}%`).bg(fill));
    const marker = beyond
      ? svg("mods/workspace-chrome/chevron-right.svg").absolute().right(-7).top(-1.5).size(12).text_color(color)
      : div().absolute().left(`${at}%`).top(0).size(9).ml(-4.5).rounded(5).bg(color).border(1.5).border_color(t.background);
    return v_flex()
      .pl(40)
      .pr(1)
      .mt(-2).pb(3)
      .gap(0)
      .child(
        h_flex()
          .items_center()
          .gap(14)
          .child(
            div()
              .relative()
              .flex_1()
              .h(9)
              .child(track(t.foreground + "1f", 100))
              .child(track(t.ansi3 + "d9", at))
              .child(div().absolute().left("50%").top(0).w(2).h(9).ml(-1).rounded(1).bg(t.blocked))
              .child(marker),
          )
          .child(div().w(62).flex_none().truncate().text_right().text_size(10.5).text_color(t.muted).child(reading.model ?? "")),
      )
      .child(
        h_flex()
          .gap(4)
          .text_size(10.5).h(14).items_center()
          .child(div().flex_none().font_semibold().text_color(color).child(`${tokensLabel(reading.tokens)} used`))
          .child(div().min_w(0).truncate().text_color(t.muted).child(`· ${fuelText(reading.tokens, line, zone)}`)),
      );
  }

  track(list, now) {
    let dirty = false;
    for (const a of list) {
      const s = a.status === "done" ? "idle" : a.status;
      if (this.changed[a.address]?.s === s) continue;
      this.changed[a.address] = { s, at: now };
      dirty = true;
    }
    if (!dirty) return;
    const listed = new Set(list.map((a) => a.address));
    for (const [k, c] of Object.entries(this.changed)) if (!listed.has(k) && now - c.at > 30 * 86400000) delete this.changed[k];
    localStorage.setItem("changed", JSON.stringify(this.changed));
  }

  migratePins(all) {
    const listed = new Map(all.filter((t) => t.state === "live").map((t) => [t.address, t]));
    const pending = this.pendingPins.filter((address) => {
      const terminal = listed.get(address);
      if (!terminal) return true;
      if (terminal.labels?.["herdr.pinned"]) return false;
      if (!this.migratingPins.has(address)) {
        this.migratingPins.add(address);
        run("terminal.pin", { terminal: address, pinned: true });
      }
      return true;
    });
    if (pending.length === this.pendingPins.length) return;
    this.pendingPins = pending;
    if (pending.length) localStorage.setItem("pins", JSON.stringify(pending));
    else localStorage.removeItem("pins");
  }

  since(a) {
    return this.changed[a.address]?.at ?? 0;
  }

  rank(a) {
    return RANK[a.status] ?? 0;
  }

  sortMenu(t) {
    const on = sortOf().id;
    return v_flex()
      .w_full()
      .p(5)
      .bg(t.popover)
      .text_size(12.8)
      .child(div().px(8).pt(4).pb(6).text_size(11).text_color(t.dim).child("Sort agents"))
      .children(SORTS.map((s) => h_flex()
        .id(`sort-${s.id}`)
        .h(31)
        .px(8)
        .gap(9)
        .rounded(7)
        .items_center()
        .text_color(t.foreground)
        .hover((el) => el.bg(t.hover))
        .on_click(() => {
          setShared("agents.sort", s.id);
          closeOverlay(props().overlay);
        })
        .child(svg(`mods/workspace-chrome/${s.icon}.svg`).size(14).flex_none().text_color(t.muted))
        .child(div().flex_1().child(s.label))
        .when(s.id === on, (el) => el.child(svg("mods/workspace-chrome/check.svg").size(14).flex_none().text_color(t.foreground)))));
  }

  peek(addr) {
    run("terminal.peek", { terminal: addr });
  }

  tree(all, addr, lead = []) {
    const kids = [...all.filter((k) => k.parent === addr && k.state === "live"), ...this.virtualKids(addr)];
    return kids.flatMap((k, i) => {
      const last = i === kids.length - 1;
      return [
        { t: k, lines: [...lead, last ? "└" : "├"] },
        ...this.tree(all, k.address, [...lead, last ? "" : "│"]),
      ];
    });
  }

  subRow(t, s, shown, compact) {
    const k = s.t;
    const st = kidStatus(k, this.listed);
    const pic = icon(k.program, 14, t.foreground);
    const on = k.address === shown;
    return h_flex()
      .id(`sub-${k.address}`)
      .pl(39)
      .pr(8)
      .rounded(8)
      .h(26)
      .flex_none()
      .gap(0)
      .items_center()
      .text_size(12.5)
      .when(on, (el) => el.bg(t.raised))
      .hover((s) => s.bg(t.hover))
      .on_click(() => (k.pane ? run("pane.focus", { pane: k.pane }) : this.listed.has(k.address) ? run("pane.focus", { terminal: k.address }) : this.peek(k.address)))
      .children(s.lines.map(l => div().self_start().mt(-4).w(9).h(17).mr(4).flex_none()
        .when(Boolean(l), el => el.border_l(1.5).border_color(t.input))
        .when(l === "└" || l === "├", el => el.border_b(1.5).rounded_bl(6))))
      .when(Boolean(pic), el => el.child(div().mr(8).child(pic)))
      .child(h_flex().flex_1().min_w(0).gap(5).mr(8).items_center()
        .child(div().flex_1().min_w(0).truncate().text_color(on ? t.foreground : t.muted).child(k.name || k.template || k.program?.name || k.address))
        .when(Boolean(k.kind), (el) => el.child(div().flex_none().text_size(11).text_color(t.dim).child(k.kind))))
      .child(this.status(t, { status: st }, compact));
  }

  status(t, a, compact = false) {
    const color = t[a.status] ?? t.dim;
    if (a.status === "working") return svg("mods/workspace-chrome/spinner.svg").size(12).mx(1).flex_none().text_color(t.working).spin(900);
    if (a.status === "blocked" && compact) return div().size(14).flex_none().rounded(4).bg(t.blocked + "26").text_size(10.5).font_bold().text_color(t.blocked).flex().items_center().justify_center().child("!");
    if (a.status === "blocked") return div().flex_none().px(6).h(18).rounded(5).bg(t.blocked + "26").text_size(10.5).font_semibold().text_color(t.blocked).flex().items_center().child("Needs you");
    if (a.status === "idle") return div().size(7).mx(3).flex_none().rounded(4).border(1.5).border_color(t.idle ?? t.dim);
    return div().size(7).mx(3).flex_none().rounded(4).bg(a.status === "done" ? color : t.dim + "00");
  }

  render(cx) {
    const t = theme();
    if (props()?.sortMenu) return this.sortMenu(t);
    const collapsed = narrow();
    const sort = sortOf();
    let list = agents();
    this.listed = new Map(list.map((a) => [a.address, a.status]));
    this.panes = new Map(list.map((a) => [a.address, a.pane]));
    const all = terminals();
    this.migratePins(all);
    const shown = focused()?.agent;
    const now = Date.now();
    this.track(list, now);
    const nested = new Set(collapsed ? [] : list.flatMap((a) => this.tree(all, a.address).map((s) => s.t.address)));
    list = list.filter((a) => a.pinned || !nested.has(a.address));
    const tabCounts = new Map();
    for (const a of list) tabCounts.set(a.tab, (tabCounts.get(a.tab) ?? 0) + 1);
    const byPriority = sort.id === "priority";
    const urgency = new Map(list.map((a) => [a.address, Math.max(this.rank(a), ...this.tree(all, a.address).map((s) => RANK[kidStatus(s.t, this.listed)] ?? 0))]));
    const rankOf = (a) => urgency.get(a.address);
    if (byPriority) list = [...list].sort((x, y) => rankOf(y) - rankOf(x) || this.since(y) - this.since(x) || y.seq - x.seq);
    const pinned = list.filter((a) => a.pinned);
    const rest = list.filter((a) => !a.pinned);
    list = [...pinned, ...rest];
    setAgentOrder(list.map((a) => a.address));
    this.publishSmoke(list);

    const withKids = (a, i) => {
      const subs = collapsed ? [] : this.tree(all, a.address);
      const r = row(a, i, subs);
      if (this.folded.has(a.address)) return [r];
      return [r, ...subs.map((s) => this.subRow(t, s, shown, byPriority))];
    };

    const fold = (a, subs) => {
      const sts = subs.map((s) => kidStatus(s.t, this.listed));
      const top = sts.reduce((m, s) => ((RANK[s] ?? 0) > (RANK[m] ?? 0) ? s : m), null);
      const open = !this.folded.has(a.address);
      return div()
        .id(`fold-${a.address}`)
        .h(18)
        .px(3)
        .gap(3)
        .flex()
        .items_center()
        .rounded(4)
        .text_size(11)
        .text_color(open ? t.dim : (t[top] ?? t.dim))
        .cursor_pointer()
        .hover((s) => s.bg(t.hover).text_color(t.foreground))
        .on_click((_e, cx) => {
          cx.stop_propagation();
          open ? this.folded.add(a.address) : this.folded.delete(a.address);
          cx.notify();
        })
        .child(svg(`mods/workspace-chrome/chevron-${open ? "down" : "right"}.svg`).size(11).flex_none())
        .when(!open, (el) => el.child(String(subs.length)));
    };

    const leads = leadPanes(panes());
    const tabs = new Map(spaces().flatMap(s => s.tabs).map(tab => [tab.id, tab]));
    const labels = new Map(list.map(a => {
      const tab = tabs.get(a.tab);
      const many = tabCounts.get(a.tab) > 1;
      return [a.address, tab ? title(many ? { ...tab, named: false } : tab, many ? a : leads.get(a.tab)) : (a.title || a.program?.name || "")];
    }));
    const label = (a) => labels.get(a.address);
    const detail = new Map(list.map(a => [a.address, /^\d+$/.test(label(a)) ? null : label(a)]));
    const seen = {};
    for (const a of list) { const k = `${a.space_name}\u0000${detail.get(a.address) ?? ""}`; seen[k] = (seen[k] ?? 0) + 1; }
    for (const a of list) {
      if (detail.get(a.address) == null && seen[`${a.space_name}\u0000`] > 1) detail.set(a.address, label(a));
    }

    const row = (a, i, subs = []) => {
      const pic = icon(a.program, 16, t.foreground);
      const more = detail.get(a.address);
      const machine = a.address.includes("/") ? a.address.split("/")[0] : null;
      if (collapsed) {
        return h_flex()
          .id(`ag-${a.address}`)
          .h(26)
          .px(8)
          .flex_none()
          .items_center()
          .gap(4)
          .hover((s) => s.bg(t.hover))
          .on_click(() => run("pane.focus", { pane: a.pane }))
          .child(div().flex_1().min_w(0).whitespace_nowrap().text_right().text_color(t.muted).child(String(i + 1)))
          .child(h_flex().w(14).flex_none().items_center().justify_center().child(this.status(t, a, true)));
      }
      const when = this.since(a);
      const idle = a.status === "idle" && !a.focused;
      const isPinned = a.pinned;
      const pin = div()
        .id(`pin-${a.address}`)
        .flex_none()
        .p(2)
        .rounded(4)
        .text_color(t.dim)
        .cursor_pointer()
        .hover((s) => s.bg(t.hover).text_color(t.foreground))
        .tooltip(isPinned ? "Unpin" : "Pin to the top")
        .on_click((_e, cx) => {
          cx.stop_propagation();
          run("terminal.pin", { terminal: a.address, pinned: !isPinned });
          this.hovered = null;
          cx.notify();
        })
        .child(svg(`mods/workspace-chrome/${isPinned ? "pin-off" : "pin"}.svg`).size(13));
      const full = byPriority || isPinned;
      const name = full ? a.space_name || label(a) : more ?? (/^\d+$/.test(label(a)) ? a.program?.name || label(a) : label(a));
      const reading = this.fuel[a.address];
      const zone = fuelZone(reading?.tokens, fuelLine());
      const smoking = zone === "dumb";
      const head = h_flex()
        .h(22)
        .items_center()
        .gap(8)
        .child(div().id(`agent-fold-${a.address}`)
          .relative()
          .size(20)
          .flex_none()
          .rounded(6)
          .bg(t.foreground + "0f")
          .flex()
          .items_center()
          .justify_center()
          .when(subs.length > 0, el => el.on_click((_e, cx) => {
            cx.stop_propagation();
            this.folded.has(a.address) ? this.folded.delete(a.address) : this.folded.add(a.address);
            cx.notify();
          }))
          .when(Boolean(pic), el => el.child(div().when(smoking, d => d.opacity(0.75)).child(pic)))
          .when(Boolean(zone), el => el.child(this.badge(t, zone))))
        .child(h_flex().flex_1().min_w(0).gap(5).items_center()
          .child(div().min_w(0).max_w(full && more ? 120 : 200).truncate().text_color(idle ? t.muted : t.foreground).child(name))
          .when(full && Boolean(more), el => el.child(div().flex_1().min_w(0).truncate().text_color(t.muted).child(`\u00b7 ${more}`))))
        .when(Boolean(machine), el => el.child(div().text_size(10).text_color(t.dim).child(machine)))
        .when(subs.length > 0, el => el.child(fold(a, subs)))
        .when(this.hovered === a.address, el => el.child(pin))
        .when(this.hovered !== a.address && Boolean(when), el => el.child(div().flex_none().text_size(11).text_color(t.dim).child(duration(now - when))))
        .child(this.status(t, a, byPriority && !isPinned));
      return v_flex()
        .id(`ag-${a.address}`)
        .px(8)
        .flex_none()
        .rounded(10)
        .when(a.focused, (el) => el.bg(t.selected))
        .cursor_pointer()
        .hover((s) => s.bg(t.hover))
        .on_hover((on, cx) => {
          if (on) this.hovered = a.address;
          else if (this.hovered === a.address) this.hovered = null;
          cx.notify();
        })
        .on_click(() => run("pane.focus", { pane: a.pane }))
        .child(head)
        .when(Boolean(zone), el => el.child(this.gauge(t, reading)));
    };

    const groups = pinned.length ? [{ id: "pinned", label: "Pinned", status: null, members: pinned }] : [];
    for (const a of rest) {
      const r = rankOf(a) || RANK.idle;
      const id = byPriority ? `sec-${r}` : `sp-${a.space}`;
      if (groups.at(-1)?.id === id) groups.at(-1).members.push(a);
      else groups.push({ id, label: byPriority ? SECTION[r] : a.space_name || "", status: byPriority ? SECTION_STATUS[r] : null, members: [a] });
    }

    const header = (g) => {
      const open = !this.folded.has(g.id);
      const top = g.members.reduce((m, a) => ((RANK[a.status] ?? 0) > (RANK[m] ?? 0) ? a.status : m), null);
      return h_flex()
        .id(g.id)
        .px(8)
        .pt(6)
        .h(26)
        .flex_none()
        .gap(6)
        .items_center()
        .text_size(11)
        .font_medium()
        .cursor_pointer()
        .hover((s) => s.text_color(t.foreground))
        .on_click((_e, cx) => {
          open ? this.folded.add(g.id) : this.folded.delete(g.id);
          cx.notify();
        })
        .child(div().truncate().text_color(t[g.status] ?? t.dim).child(g.label))
        .when(byPriority || !open, (el) => el.child(div().text_color(t.dim).child(String(g.members.length))))
        .child(div().flex_1())
        .when(!open, (el) => el
          .when(!byPriority && top && top !== "idle", (el) => el.child(this.status(t, { status: top }, true)))
          .child(svg("mods/workspace-chrome/chevron-right.svg").size(11).flex_none().text_color(t.dim)));
    };
    const rows = collapsed
      ? list.flatMap(withKids)
      : groups.flatMap((g) => [header(g), ...(this.folded.has(g.id) ? [] : g.members.flatMap(withKids))]);

    return v_flex()
      .size_full()
      .min_h(0)
      .bg(t.chrome)
      .text_size(13)
      .when(!collapsed, (el) =>
        el.child(
          h_flex()
            .px(16)
            .h(26)
            .mt(6)
            .flex_none()
            .items_center()
            .justify_center()
            .gap(10)
            .child(div().flex_1().h(1).bg(t.ansi3 + "40"))
            .child(svg("mods/beacon-agents/flame.svg").size(13).text_color(t.ansi3))
            .child(div().flex_1().h(1).bg(t.ansi3 + "40")),
        ).child(
          h_flex()
            .px(16)
            .h(34)
            .pt(8)
            .flex_none()
            .items_center()
            .gap(8)
            .child(svg("mods/beacon-agents/diamond.svg").size(7).flex_none().text_color(t.ansi3))
            .child(div().text_size(10.5).font_bold().text_color(t.ansi3).child("AGENTS"))
            .child(div().flex_1().h(1).bg(t.ansi3 + "33"))
            .child(
              div()
                .id("ag-sort")
                .text_size(11)
                .text_color(t.dim)
                .hover((s) => s.text_color(t.foreground))
                .on_click(() => openOverlay({ mod: "beacon-agents", kind: "popup", width: 200, height: 140, props: { sortMenu: true } }))
                .tooltip(`Sort agents: ${sort.label}`)
                .child(svg(`mods/workspace-chrome/${sort.icon}.svg`).size(14)),
            ),
        ),
      )
      .when(list.length === 0 && !collapsed, (el) =>
        el.child(div().px(12).text_size(12).text_color(t.dim).child("No agent status yet. Agents show up here when they report their status. Older versions and agents inside tmux may not.")),
      )
      .child(v_flex().id("ag-list").flex_1().min_h(0).px(collapsed ? 0 : 8).overflow_y_scrollbar().mode("always").children(rows))
      .when(collapsed, (el) =>
        el.child(
          h_flex()
            .h(28)
            .items_center()
            .justify_center()
            .child(
              div()
                .id("collapse")
                .text_size(13)
                .text_color(t.dim)
                .hover((s) => s.text_color(t.foreground))
                .on_click(toggleSidebar)
                .child("»"),
            ),
        ),
      );
  }
}
