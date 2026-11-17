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
    re: /(?:modified|deleted|deleted by us|new file): +(?<match>.+)/g,
  },
  {
    name: "git-status-branch",
    priority: 15,
    re: /Your branch is up to date with '(?<match>.*)'\./g,
  },
  { name: "diff", priority: 15, re: /(?:---|\+\+\+) [ab]\/(?<match>.*)/g },
  {
    name: "kubernetes",
    priority: 18,
    re: new RegExp(
      `\\b(?:${K8S_KINDS})(?:[/_#$%&+=@-][A-Za-z0-9_#$%&+=/@-]*)?\\b`,
      "g",
    ),
  },
  { name: "path", priority: 20, re: /(?:[.\w\-~$@]+)?(?:\/[.\w\-@]+)+\/?/g },
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
  { name: "number", priority: 60, re: /[0-9]{4,}/g },
];

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
  const order = [];
  const byText = new Map();
  for (const s of spans) {
    if (!byText.has(s.text)) {
      if (order.length >= ALPHABET.length * ALPHABET.length) continue;
      byText.set(s.text, order.length);
      order.push({ text: s.text, pattern: s.pattern, spans: [] });
    }
    order[byText.get(s.text)].spans.push({
      line: s.line,
      start: s.start,
      end: s.end,
    });
  }
  const labels = hints(order.length);
  return order.map((t, i) => ({ ...t, hint: labels[i] }));
}

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
