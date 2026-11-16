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

export function findTokens(lines, mode) {
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
