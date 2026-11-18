import { View, div } from "gpui-kit";
import { v_flex, h_flex, ScrollHandle } from "gpui-base";
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
  home,
} from "herdr";

const K8S_KINDS =
  "deployment.app|binding|componentstatuse|configmap|endpoint|event|limitrange|namespace|node|persistentvolumeclaim|persistentvolume|pod|podtemplate|replicationcontroller|resourcequota|secret|serviceaccount|service|mutatingwebhookconfiguration.admissionregistration.k8s.io|validatingwebhookconfiguration.admissionregistration.k8s.io|customresourcedefinition.apiextension.k8s.io|apiservice.apiregistration.k8s.io|controllerrevision.apps|daemonset.apps|deployment.apps|replicaset.apps|statefulset.apps|tokenreview.authentication.k8s.io|localsubjectaccessreview.authorization.k8s.io|selfsubjectaccessreviews.authorization.k8s.io|selfsubjectrulesreview.authorization.k8s.io|subjectaccessreview.authorization.k8s.io|horizontalpodautoscaler.autoscaling|cronjob.batch|job.batch|certificatesigningrequest.certificates.k8s.io|events.events.k8s.io|daemonset.extensions|deployment.extensions|ingress.extensions|networkpolicies.extensions|podsecuritypolicies.extensions|replicaset.extensions|networkpolicie.networking.k8s.io|poddisruptionbudget.policy|clusterrolebinding.rbac.authorization.k8s.io|clusterrole.rbac.authorization.k8s.io|rolebinding.rbac.authorization.k8s.io|role.rbac.authorization.k8s.io|storageclasse.storage.k8s.io";

const URL = {
  name: "url",
  priority: 10,
  re: /(?:https?:\/\/|git@|git:\/\/|ssh:\/\/|ftp:\/\/|file:\/\/)[^\s()"']+/gi,
  trimUrl: true,
};

const PATTERNS = [
  URL,
  {
    name: "git-status",
    priority: 15,
    re: /^\s*(?:modified|deleted|deleted by us|new file|renamed): +(?<match>\S+)/g,
    accept: hasLetter,
  },
  {
    name: "git-status-branch",
    priority: 15,
    re: /Your branch is up to date with '(?<match>.*)'\./g,
  },
  {
    name: "diff",
    priority: 15,
    re: /^(?:---|\+\+\+) [ab]\/(?<match>\S+)/g,
    accept: hasLetter,
  },
  {
    name: "kubernetes",
    priority: 18,
    re: new RegExp(
      `\\b(?:${K8S_KINDS})(?:[/_#$%&+=@-][A-Za-z0-9_#$%&+=/@-]*)?\\b`,
      "g",
    ),
  },
  {
    name: "path",
    priority: 20,
    re: /(?<![\w\\/.:@$-])(?:~|\.{1,2}|[\w\-@.]+)?(?:\/[\w\-@.]+)+\/?/g,
    accept: isPath,
  },
  {
    name: "uuid",
    priority: 30,
    re: /\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\b/g,
  },
  {
    name: "kubernetes-pod",
    priority: 35,
    re: /\b[a-z][a-z0-9-]*[a-z0-9]-[bcdfghjklmnpqrstvwxz2456789]{5,10}-[bcdfghjklmnpqrstvwxz2456789]{5}\b/g,
  },
  { name: "git-sha", priority: 40, re: /\b[0-9a-fA-F]{7,40}\b/g },
  { name: "hex", priority: 45, re: /\b0x[0-9a-fA-F]+\b/g },
  { name: "ipv4", priority: 50, re: /\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/g },
  { name: "number", priority: 60, re: /(?<![\d/.:-])[0-9]{4,}(?![\d/.:-])/g },
];

const FILE_KINDS = new Set(["path", "git-status", "diff"]);

function hasLetter(text) {
  return /[A-Za-z]/.test(text);
}

function isPath(text) {
  const t = text.replace(/\/$/, "");
  if (!hasLetter(t)) return false;
  const segs = t.split("/");
  if (segs.some((seg) => /^\.{3,}$/.test(seg) || seg.includes("…"))) return false;
  const head = segs[0];
  const rest = segs.slice(1).filter(Boolean);
  if (head === "") return rest.length >= 2;
  if (head === "~" || head === "." || head === "..") return rest.length >= 1;
  return segs.length >= 3 || /\.[A-Za-z0-9]+$/.test(segs[segs.length - 1]);
}

const ALPHABET = "asdfghjklqwertyuiopzxcvbnm";

function count(text, ch) {
  return text.split(ch).length - 1;
}

function trimUrlEnd(text) {
  const pairs = { ")": "(", "]": "[", "}": "{" };
  while (text.length > 0) {
    const last = text[text.length - 1];
    const sentence = ".,;:!?".includes(last);
    const unbalanced =
      pairs[last] && count(text, last) > count(text, pairs[last]);
    if (!sentence && !unbalanced) break;
    text = text.slice(0, -1);
  }
  return text;
}

function candidates(lines, patterns) {
  const out = [];
  lines.forEach((line, lineIndex) => {
    patterns.forEach((p, order) => {
      for (const m of line.matchAll(p.re)) {
        const group =
          m.groups && m.groups.match !== undefined ? m.groups.match : m[0];
        const start =
          m.index +
          (m.groups && m.groups.match !== undefined ? m[0].indexOf(group) : 0);
        const text = p.trimUrl ? trimUrlEnd(group) : group;
        if (text.length === 0) continue;
        if (p.accept && !p.accept(text)) continue;
        out.push({
          line: lineIndex,
          start,
          end: start + text.length,
          text,
          pattern: p.name,
          priority: p.priority,
          order,
        });
      }
    });
  });
  return out;
}

function resolve(list) {
  const sorted = [...list].sort(
    (a, b) =>
      a.priority - b.priority ||
      b.end - b.start - (a.end - a.start) ||
      a.line - b.line ||
      a.start - b.start ||
      a.end - b.end ||
      a.order - b.order,
  );
  const accepted = [];
  for (const c of sorted) {
    if (
      accepted.every(
        (s) => s.line !== c.line || c.start >= s.end || s.start >= c.end,
      )
    )
      accepted.push(c);
  }
  return accepted.sort(
    (a, b) => a.line - b.line || a.start - b.start || a.end - b.end,
  );
}

function hints(n) {
  if (n <= ALPHABET.length) return [...ALPHABET];
  return [...ALPHABET].flatMap((a) => [...ALPHABET].map((b) => a + b));
}

function findTokens(lines, mode) {
  let spans = resolve(candidates(lines, mode === "url" ? [URL] : PATTERNS));
  if (mode === "url")
    spans = spans.filter((s) => /^(https?|file):\/\//i.test(s.text));
  const seen = new Set();
  const order = [];
  for (const s of [...spans].reverse()) {
    if (seen.has(s.text)) continue;
    if (order.length >= ALPHABET.length * ALPHABET.length) break;
    seen.add(s.text);
    order.push({ text: s.text, pattern: s.pattern });
  }
  const labels = hints(order.length);
  return order.map((t, i) => ({ ...t, hint: labels[i] }));
}

export default class Pluck extends View {
  init(_p, cx) {
    const p = props() ?? {};
    this.mode = p.mode === "url" ? "url" : "pluck";
    this.typed = "";
    this.tokens = null;
    this.selected = 0;
    this.scroll = ScrollHandle.new();
    this.handle = cx.focus_handle();
    this.handle.focus();
    cx.timer.after(150, () => this.handle.focus());
    const terminal = focused()?.terminal;
    this.cwd = focused()?.cwd ?? null;
    if (!terminal || checking()) {
      this.tokens = [];
      return;
    }
    cx.spawn(async (cx) => {
      const r = await hc(["read", terminal, "--json"]);
      const lines = (JSON.parse(r.stdout).text ?? "").split("\n");
      this.tokens = findTokens(lines, this.mode);
      if (this.tokens.length === 0) {
        notify({
          title:
            this.mode === "url"
              ? "No URLs on screen"
              : "Nothing to pluck on screen",
          timeout: 2000,
        });
        this.dismiss();
        return;
      }
      cx.notify();
    });
  }

  dismiss() {
    closeOverlay(props().overlay);
  }

  shown() {
    return (this.tokens ?? []).filter((t) => t.hint.startsWith(this.typed));
  }

  canOpen(token) {
    return token.pattern === "url" || FILE_KINDS.has(token.pattern);
  }

  canReveal(token) {
    return FILE_KINDS.has(token.pattern);
  }

  async locate(text) {
    const exists = async (p) => (await exec("test", ["-e", p])).code === 0;
    if (text.startsWith("/")) return (await exists(text)) ? text : null;
    if (text === "~" || text.startsWith("~/")) {
      const p = home() + text.slice(1);
      return (await exists(p)) ? p : null;
    }
    if (!this.cwd) return null;
    const bases = [this.cwd];
    const git = await exec("git", ["rev-parse", "--show-toplevel"], { cwd: this.cwd });
    if (git.code === 0 && git.stdout.trim()) bases.push(git.stdout.trim());
    for (const base of bases) {
      const p = `${base}/${text}`;
      if (await exists(p)) return p;
    }
    return null;
  }

  choose(token, action, cx) {
    if (this.busy) return;
    if (action === "reveal" && !this.canReveal(token)) action = "open";
    if (action === "open" && !this.canOpen(token)) action = "copy";
    if (action === "copy") {
      exec("sh", ["-c", 'printf %s "$1" | pbcopy', "sh", token.text]);
      notify({ title: "Copied", body: token.text, timeout: 2000 });
      return this.dismiss();
    }
    if (token.pattern === "url") {
      exec("open", [token.text]);
      return this.dismiss();
    }
    this.busy = true;
    cx.spawn(async () => {
      const target = await this.locate(token.text);
      if (target) exec("open", action === "reveal" ? ["-R", target] : [target]);
      else notify({ title: "Could not find that path", body: token.text, level: "error" });
      this.dismiss();
    });
  }

  defaultAction() {
    return this.mode === "url" ? "open" : "copy";
  }

  actionFor(e) {
    if (e.modifiers?.alt) return "reveal";
    if (e.modifiers?.shift) return this.mode === "url" ? "copy" : "open";
    return this.defaultAction();
  }

  move(by, cx) {
    const list = this.shown();
    if (!list.length) return;
    this.selected = (this.selected + by + list.length) % list.length;
    this.scroll.scroll_to_item(this.selected);
    cx.notify();
  }

  key(e, cx) {
    if (e.key === "escape" || e.keystroke === "ctrl-c") return this.dismiss();
    if (e.key === "down" || e.keystroke === "ctrl-n") return this.move(1, cx);
    if (e.key === "up" || e.keystroke === "ctrl-p") return this.move(-1, cx);
    if (e.key === "enter") {
      const token = this.shown()[this.selected];
      if (token) this.choose(token, this.actionFor(e), cx);
      return;
    }
    if (e.key === "backspace") {
      this.typed = this.typed.slice(0, -1);
      this.selected = 0;
      return cx.notify();
    }
    const ch = e.key;
    if (e.modifiers?.control || e.modifiers?.platform) return;
    if (!ch || ch.length !== 1 || !/[a-z]/.test(ch)) return;
    const typed = this.typed + ch;
    const exact = (this.tokens ?? []).find((t) => t.hint === typed);
    if (exact) return this.choose(exact, this.actionFor(e), cx);
    if ((this.tokens ?? []).some((t) => t.hint.startsWith(typed))) {
      this.typed = typed;
      this.selected = 0;
    }
    cx.notify();
  }

  row(token, index, t, font) {
    const on = index === this.selected;
    const rest = token.hint.slice(this.typed.length);
    return h_flex()
      .id(`pluck-${token.hint}`)
      .px(10)
      .py(6)
      .gap(12)
      .items_center()
      .rounded(6)
      .cursor_pointer()
      .when(on, (el) => el.bg(t.selected))
      .hover((s) => s.bg(t.hover))
      .on_click((_e, cx) => this.choose(token, this.defaultAction(), cx))
      .child(
        h_flex()
          .flex_none()
          .w(30)
          .justify_center()
          .py(1)
          .rounded(4)
          .bg(t.ansi3)
          .font_family(font)
          .font_weight(800)
          .text_color(t.background)
          .when(this.typed.length > 0, (el) =>
            el.child(div().opacity(0.45).child(this.typed)),
          )
          .child(rest),
      )
      .child(
        div()
          .flex_1()
          .min_w(0)
          .truncate()
          .font_family(font)
          .text_color(on ? t.foreground : t.muted)
          .child(token.text),
      )
      .when(on && this.mode !== "url" && this.canOpen(token), (el) =>
        el.child(this.button(`open-${token.hint}`, "Open", "⇧", t, (cx) => this.choose(token, "open", cx))),
      )
      .when(on && this.canReveal(token), (el) =>
        el.child(this.button(`reveal-${token.hint}`, "Finder", "⌥", t, (cx) => this.choose(token, "reveal", cx))),
      )
      .child(
        div()
          .flex_none()
          .text_size(11)
          .text_color(t.dim)
          .child(token.pattern),
      );
  }

  button(id, label, hint, t, run) {
    return h_flex()
      .id(id)
      .flex_none()
      .px(8)
      .h(22)
      .gap(6)
      .items_center()
      .rounded(5)
      .border_1()
      .border_color(t.border)
      .bg(t.surface)
      .text_size(11.5)
      .cursor_pointer()
      .hover((s) => s.bg(t.hover))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click((_e, cx) => {
        cx.stop_propagation();
        run(cx);
      })
      .child(div().text_color(t.foreground).child(label))
      .child(div().text_size(10.5).text_color(t.dim).child(hint));
  }

  render(_cx) {
    const t = theme();
    const font = settings().font_family ?? "Menlo";
    const list = this.shown();
    const title = this.mode === "url" ? "Open URL" : "Pluck";
    return v_flex()
      .id("pluck-root")
      .size_full()
      .text_size(13)
      .track_focus(this.handle)
      .on_mouse_down("left", () => this.handle.focus())
      .on_key_down((e, cx) => {
        cx.stop_propagation();
        this.key(e, cx);
      })
      .child(
        h_flex()
          .flex_none()
          .px(14)
          .py(10)
          .gap(10)
          .items_center()
          .border_b_1()
          .border_color(t.border)
          .child(div().font_semibold().text_color(t.foreground).child(title))
          .child(
            div()
              .text_size(11)
              .text_color(t.dim)
              .child(this.tokens === null ? "Reading terminal…" : `${list.length}`),
          )
          .child(div().flex_1())
          .when(this.typed.length > 0, (el) =>
            el.child(
              div()
                .text_size(11.5)
                .text_color(t.ansi3)
                .child(`hint: ${this.typed}`),
            ),
          ),
      )
      .child(
        v_flex()
          .id("pluck-list")
          .flex_1()
          .min_h(0)
          .p(6)
          .overflow_y_scrollbar()
          .track_scroll(this.scroll)
          .children(list.map((token, i) => this.row(token, i, t, font))),
      )
      .child(
        div()
          .flex_none()
          .px(14)
          .py(8)
          .border_t_1()
          .border_color(t.border)
          .text_size(11.5)
          .text_color(t.dim)
          .child(
            this.mode === "url"
              ? "hint open · ⇧hint copy · ↑↓ ↵ · esc close"
              : "hint copy · ⇧hint open in default app · ⌥hint reveal in Finder · ↑↓ ↵ · esc close",
          ),
      );
  }
}
