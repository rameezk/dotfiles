import { View, div, svg } from "gpui-kit";
import { v_flex, h_flex, ScrollHandle } from "gpui-base";
import { exec, run, theme, shared, setShared, openOverlay, closeOverlay, props, notify, checking } from "herdr";

const REFRESH_MS = 5 * 60 * 1000;
const STALE_DAYS = 90;
const SIDEBAR_LIMIT = 6;
const KEY = "prs.inbox";

const FRAGMENT = `fragment pr on PullRequest {
  id number title url isDraft updatedAt headRefName headRefOid baseRefName additions deletions changedFiles reviewDecision mergeStateStatus
  repository { nameWithOwner viewerPermission viewerDefaultMergeMethod mergeCommitAllowed squashMergeAllowed rebaseMergeAllowed }
  author { login __typename }
  latestReviews(first: 20) { nodes { author { login } state commit { oid } } }
  commits(last: 1) { nodes { commit { statusCheckRollup { state contexts(first: 30) { nodes { __typename ... on CheckRun { name status conclusion } ... on StatusContext { context state } } } } } } }
}`;

const SEARCHES = {
  owned: "is:pr is:open archived:false user:@me -author:@me",
  requested: "is:pr is:open archived:false review-requested:@me",
  reviewed: "is:pr is:open archived:false reviewed-by:@me -author:@me",
  involved: "is:pr is:open archived:false involves:@me -author:@me",
  mine: "is:pr is:open archived:false author:@me",
};

const QUERY = `${FRAGMENT}
query {
  viewer { login }
${Object.entries(SEARCHES).map(([k, q]) => `  ${k}: search(type: ISSUE, first: 50, query: "${q}") { nodes { ...pr } }`).join("\n")}
}`;

const GROUPS = [
  { id: "review", label: "To review" },
  { id: "ready", label: "Ready to merge" },
  { id: "mine", label: "Yours" },
  { id: "waiting", label: "Waiting on others" },
  { id: "stale", label: "Stale" },
];

const ACTIONABLE = new Set(["review", "ready"]);

const VERDICT = { APPROVED: "You approved", CHANGES_REQUESTED: "You asked for changes", COMMENTED: "You commented", DISMISSED: "Your review was dismissed" };
const DECISION = { APPROVED: "approved", CHANGES_REQUESTED: "changes requested", REVIEW_REQUIRED: "review required" };
const BLOCKED = {
  BEHIND: "branch is behind its base",
  BLOCKED: "blocked by branch rules",
  DIRTY: "has merge conflicts",
  UNSTABLE: "some checks are failing",
  HAS_HOOKS: "merge hooks pending",
  UNKNOWN: "mergeability still being checked",
  DRAFT: "still a draft",
};
const WRITERS = new Set(["ADMIN", "MAINTAIN", "WRITE"]);
const METHOD_FLAG = { MERGE: "--merge", SQUASH: "--squash", REBASE: "--rebase" };
const METHOD_LABEL = { MERGE: "Merge", SQUASH: "Squash and merge", REBASE: "Rebase and merge" };

function mergeMethod(repo) {
  const allowed = { MERGE: repo.mergeCommitAllowed, SQUASH: repo.squashMergeAllowed, REBASE: repo.rebaseMergeAllowed };
  if (allowed[repo.viewerDefaultMergeMethod]) return repo.viewerDefaultMergeMethod;
  return ["SQUASH", "MERGE", "REBASE"].find((m) => allowed[m]) ?? null;
}

function checksOf(pr) {
  const rollup = pr.commits?.nodes?.[0]?.commit?.statusCheckRollup;
  const state = rollup?.state;
  const ci = !state ? "none" : state === "SUCCESS" ? "pass" : state === "FAILURE" || state === "ERROR" ? "fail" : "running";
  const list = (rollup?.contexts?.nodes ?? []).map((c) => {
    if (c.__typename === "CheckRun") {
      const r = c.status !== "COMPLETED" ? "running" : ["SUCCESS", "NEUTRAL", "SKIPPED"].includes(c.conclusion) ? "pass" : "fail";
      return { name: c.name, state: r, label: c.status !== "COMPLETED" ? c.status.toLowerCase() : (c.conclusion ?? "").toLowerCase() };
    }
    const r = c.state === "SUCCESS" ? "pass" : c.state === "PENDING" || c.state === "EXPECTED" ? "running" : "fail";
    return { name: c.context, state: r, label: (c.state ?? "").toLowerCase() };
  });
  return { ci, list };
}

function classify(data) {
  const me = data.viewer.login;
  const seen = new Map();
  const from = {};
  for (const key of Object.keys(SEARCHES)) {
    from[key] = new Set();
    for (const pr of data[key]?.nodes ?? []) {
      if (!pr?.id) continue;
      from[key].add(pr.id);
      seen.set(pr.id, pr);
    }
  }
  const now = Date.now();
  const items = [];
  for (const pr of seen.values()) {
    const mine = pr.author?.login === me;
    const review = (pr.latestReviews?.nodes ?? []).find((r) => r.author?.login === me);
    const requested = from.requested.has(pr.id);
    const owned = from.owned.has(pr.id);
    const old = now - Date.parse(pr.updatedAt) > STALE_DAYS * 86400000;
    const method = mergeMethod(pr.repository);
    const canMerge = !pr.isDraft && pr.mergeStateStatus === "CLEAN" && WRITERS.has(pr.repository.viewerPermission) && method !== null;
    const approvedAtHead = review?.state === "APPROVED" && review.commit?.oid === pr.headRefOid;
    let group;
    let why;
    let rank = 9;
    if (mine) {
      group = "mine";
      why = pr.isDraft ? "Draft" : canMerge ? "Ready to merge" : DECISION[pr.reviewDecision] ? `Review: ${DECISION[pr.reviewDecision]}` : "Open";
    } else if (approvedAtHead) {
      group = canMerge ? "ready" : "waiting";
      why = canMerge ? "You approved · ready to merge" : `You approved · ${BLOCKED[pr.mergeStateStatus] ?? "not mergeable yet"}`;
    } else if (pr.isDraft) {
      group = "waiting";
      why = "Draft";
    } else if (review && review.commit?.oid && review.commit.oid !== pr.headRefOid) {
      group = "review";
      why = "New commits since your review";
      rank = 0;
    } else if (requested) {
      group = "review";
      why = review ? "Review requested again" : "Review requested";
      rank = 1;
    } else if (review) {
      group = "waiting";
      why = VERDICT[review.state] ?? "You reviewed";
    } else if (owned) {
      group = old ? "stale" : "review";
      why = pr.author?.__typename === "Bot" ? "Bot PR in your repo" : "In your repo";
      rank = 2;
    } else {
      group = old ? "stale" : "review";
      why = "You are involved";
      rank = 3;
    }
    const checks = checksOf(pr);
    items.push({
      id: pr.id,
      url: pr.url,
      repo: pr.repository.nameWithOwner,
      number: pr.number,
      title: pr.title,
      author: pr.author?.login ?? "ghost",
      bot: pr.author?.__typename === "Bot",
      branch: pr.headRefName,
      add: pr.additions,
      del: pr.deletions,
      files: pr.changedFiles,
      updatedAt: pr.updatedAt,
      decision: DECISION[pr.reviewDecision] ?? null,
      base: pr.baseRefName,
      head: pr.headRefOid,
      mine,
      canMerge,
      approvedAtHead,
      method,
      ci: checks.ci,
      checks: checks.list,
      group,
      why,
      rank,
    });
  }
  items.sort((a, b) => a.rank - b.rank || Date.parse(b.updatedAt) - Date.parse(a.updatedAt));
  return items;
}

async function fetchInbox() {
  const r = await exec("gh", ["api", "graphql", "-f", `query=${QUERY}`], { timeout: 60000 });
  if (r.code !== 0) throw new Error((r.stderr || r.stdout).trim().split("\n")[0] || "gh api graphql failed");
  const body = JSON.parse(r.stdout);
  if (body.errors?.length) throw new Error(body.errors[0].message);
  return classify(body.data);
}

function inbox() {
  const v = shared(KEY);
  return v && typeof v === "object" && Array.isArray(v.items) ? v : { items: [], at: null, error: null };
}

function age(iso) {
  const m = Math.floor((Date.now() - Date.parse(iso)) / 60000);
  if (m < 60) return `${Math.max(m, 1)}m`;
  const h = Math.floor(m / 60);
  if (h < 24) return `${h}h`;
  const d = Math.floor(h / 24);
  if (d < 365) return `${d}d`;
  return `${Math.floor(d / 365)}y`;
}

function short(repo) {
  return repo.split("/").pop();
}

function ciColor(t, ci) {
  return ci === "pass" ? t.done : ci === "fail" ? t.blocked : ci === "running" ? t.working : t.dim;
}

function tint(color, alpha) {
  return /^#[0-9a-f]{6}/i.test(color) ? `${color.slice(0, 7)}${alpha}` : color;
}

function whyColor(t, item) {
  return item.group === "ready" ? t.done : item.rank === 0 ? t.ansi3 : t.dim;
}

function ciDot(t, ci) {
  const c = ciColor(t, ci);
  const d = div().size(8).rounded(4).flex_none();
  return ci === "pass" || ci === "fail" ? d.bg(c) : d.border_1().border_color(c);
}

function reviewInHunk(item) {
  run("pane.split", {
    direction: "right",
    argv: ["sh", "-c", 'exec hunk gh pr "$1" --repo "$2"', "sh", String(item.number), item.repo],
  });
  run("pane.zoom", {});
}

function openUrl(url) {
  exec("open", [url]).catch(() => notify({ title: "Could not open the browser", body: url, level: "error" }));
}

function copyUrl(item) {
  exec("sh", ["-c", 'printf %s "$1" | pbcopy', "sh", item.url]).then(
    () => notify({ title: "Copied PR link", body: item.url, timeout: 2000 }),
    () => notify({ title: "Could not copy the link", body: item.url, level: "error" }),
  );
}

function mergeLabel(item) {
  const verb = METHOD_LABEL[item.method] ?? "Merge";
  return item.mine || item.approvedAtHead ? verb : `Approve and ${verb.toLowerCase()}`;
}

async function gh(args) {
  const r = await exec("gh", args, { timeout: 120000 });
  if (r.code !== 0) throw new Error((r.stderr || r.stdout).trim().split("\n").pop() || `gh ${args[0]} failed`);
}

async function mergePr(item, phase) {
  const target = [String(item.number), "--repo", item.repo];
  if (!item.mine && !item.approvedAtHead) {
    phase("approving");
    await gh(["pr", "review", ...target, "--approve"]);
  }
  phase("merging");
  await gh(["pr", "merge", ...target, METHOD_FLAG[item.method], "--match-head-commit", item.head]);
}

const ACTIONS = [
  { key: "enter", hint: "↵", label: "Review in Hunk", primary: true, closes: true, run: (item) => reviewInHunk(item) },
  { key: "m", label: (item) => mergeLabel(item), primary: true, confirm: true, when: (item) => item.canMerge },
  { key: "o", label: "Open PR", run: (item) => openUrl(item.url) },
  { key: "f", label: "Files", run: (item) => openUrl(`${item.url}/files`) },
  { key: "c", label: "Checks", run: (item) => openUrl(`${item.url}/checks`) },
  { key: "g", label: "Repo", run: (item) => openUrl(`https://github.com/${item.repo}`) },
  { key: "y", label: "Copy", run: (item) => copyUrl(item) },
];

function openInbox(select) {
  openOverlay({ mod: "beacon-prs", kind: "dialog", width: 980, height: 0.8, props: { select } });
}

export default class PullRequests extends View {
  init(_props, cx) {
    this.overlayMode = Boolean(props()?.overlay);
    this.busy = false;
    if (this.overlayMode) {
      this.handle = cx.focus_handle();
      this.handle.focus();
      cx.timer.after(150, () => this.handle.focus());
      cx.timer.after(600, () => this.handle.focus());
      this.scroll = ScrollHandle.new();
      this.group = "review";
      this.selectedId = props()?.select ?? null;
      const picked = inbox().items.find((i) => i.id === this.selectedId);
      if (picked) this.group = picked.group;
    }
    if (checking()) return;
    if (this.overlayMode) {
      const at = inbox().at;
      if (!at || Date.now() - Date.parse(at) > 60000) this.refresh(cx);
      return;
    }
    this.refresh(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.refresh(cx));
  }

  refresh(cx) {
    if (this.busy) return;
    this.busy = true;
    cx.notify();
    cx.spawn(async (cx) => {
      try {
        const items = await fetchInbox();
        setShared(KEY, { items, at: new Date().toISOString(), error: null });
      } catch (e) {
        setShared(KEY, { ...inbox(), error: String(e?.message ?? e) });
      }
      this.busy = false;
      cx.notify();
    });
  }

  render(cx) {
    return this.overlayMode ? this.renderInbox(cx) : this.renderSidebar(cx);
  }

  renderSidebar(_cx) {
    const t = theme();
    const data = inbox();
    const review = GROUPS.filter((g) => ACTIONABLE.has(g.id)).flatMap((g) => data.items.filter((i) => i.group === g.id));
    const counts = Object.fromEntries(GROUPS.map((g) => [g.id, data.items.filter((i) => i.group === g.id).length]));
    const rows = review.slice(0, SIDEBAR_LIMIT).map((item) =>
      v_flex()
        .id(`pr-${item.id}`)
        .px(8)
        .py(5)
        .gap(1)
        .rounded(6)
        .cursor_pointer()
        .hover((s) => s.bg(t.hover))
        .on_click(() => openInbox(item.id))
        .tooltip(`${item.repo}#${item.number} · ${item.why}`)
        .child(
          h_flex()
            .gap(6)
            .items_center()
            .child(ciDot(t, item.ci))
            .child(div().flex_1().min_w(0).truncate().text_color(t.foreground).child(item.title))
            .child(div().flex_none().text_size(11).text_color(t.dim).child(age(item.updatedAt))),
        )
        .child(
          h_flex()
            .pl(14)
            .gap(6)
            .text_size(11)
            .child(div().flex_none().text_color(t.muted).child(`${short(item.repo)}#${item.number}`))
            .child(div().flex_1().min_w(0).truncate().text_color(whyColor(t, item)).child(item.why)),
        ),
    );
    const rest = GROUPS.filter((g) => !ACTIONABLE.has(g.id) && counts[g.id] > 0).map((g) => `${g.label} ${counts[g.id]}`);
    const more = review.length - rows.length;
    const footer = [more > 0 ? `${more} more` : null, ...rest].filter(Boolean).join(" · ");

    return v_flex()
      .size_full()
      .min_h(0)
      .bg(t.chrome)
      .text_size(13)
      .child(
        h_flex()
          .px(16)
          .h(26)
          .mt(6)
          .flex_none()
          .items_center()
          .justify_center()
          .gap(10)
          .child(div().flex_1().h(1).bg(tint(t.ansi6, "40")))
          .child(svg("mods/beacon-prs/diamond.svg").size(9).text_color(t.ansi6))
          .child(div().flex_1().h(1).bg(tint(t.ansi6, "40"))),
      )
      .child(
        h_flex()
          .px(16)
          .h(34)
          .pt(8)
          .flex_none()
          .items_center()
          .gap(8)
          .child(svg("mods/beacon-prs/diamond.svg").size(7).flex_none().text_color(t.ansi6))
          .child(div().text_size(10.5).font_bold().text_color(t.ansi6).child("PULL REQUESTS"))
          .when(review.length > 0, (el) => el.child(div().text_size(10.5).font_bold().text_color(t.ansi6).child(String(review.length))))
          .child(div().flex_1().h(1).bg(tint(t.ansi6, "33")))
          .child(
            div()
              .id("prs-open")
              .text_size(11)
              .text_color(t.dim)
              .cursor_pointer()
              .hover((s) => s.text_color(t.foreground))
              .on_click(() => openInbox(null))
              .tooltip("Open the PR inbox")
              .child(svg("mods/workspace-chrome/chevron-right.svg").size(12)),
          ),
      )
      .when(Boolean(data.error), (el) => el.child(div().px(16).pb(4).text_size(11).text_color(t.ansi1).truncate().child(data.error)))
      .when(rows.length === 0 && !data.error, (el) =>
        el.child(div().px(16).py(4).text_size(12).text_color(t.dim).child(data.at ? "Nothing waiting on you." : "Checking GitHub…")),
      )
      .child(
        v_flex()
          .id("prs-list")
          .flex_1()
          .min_h(0)
          .px(8)
          .overflow_y_scrollbar()
          .children(rows)
          .when(Boolean(footer), (el) =>
            el.child(
              div()
                .id("prs-footer")
                .px(8)
                .py(6)
                .text_size(11)
                .text_color(t.dim)
                .cursor_pointer()
                .hover((s) => s.text_color(t.foreground))
                .on_click(() => openInbox(null))
                .child(footer),
            ),
          ),
      );
  }

  listed() {
    return inbox().items.filter((i) => i.group === this.group);
  }

  selected() {
    const list = this.listed();
    return list.find((i) => i.id === this.selectedId) ?? list[0] ?? null;
  }

  move(by, cx) {
    const list = this.listed();
    if (!list.length) return;
    const at = Math.max(list.indexOf(this.selected()), 0);
    const next = (at + by + list.length) % list.length;
    this.selectedId = list[next].id;
    this.scroll.scroll_to_item(next);
    cx.notify();
  }

  cycle(by, cx) {
    const at = GROUPS.findIndex((g) => g.id === this.group);
    this.group = GROUPS[(at + by + GROUPS.length) % GROUPS.length].id;
    this.selectedId = null;
    cx.notify();
  }

  close() {
    closeOverlay(props().overlay);
  }

  key(e, cx) {
    const k = e.key;
    if (this.job) return;
    if (this.confirming && this.selected()?.id !== this.confirming) this.confirming = null;
    if (this.confirming) {
      if (k === "enter") return this.merge(this.selected(), cx);
      if (k === "escape" || k === "n") {
        this.confirming = null;
        return cx.notify();
      }
      return;
    }
    if (k === "escape" || k === "q") return this.close();
    if (k === "tab") return this.cycle(e.modifiers?.shift ? -1 : 1, cx);
    if (k === "right" || k === "l") return this.cycle(1, cx);
    if (k === "left" || k === "h") return this.cycle(-1, cx);
    if (k === "down" || k === "j") return this.move(1, cx);
    if (k === "up" || k === "k") return this.move(-1, cx);
    if (k === "r") return this.refresh(cx);
    const tab = GROUPS[Number(k) - 1];
    if (tab) {
      this.group = tab.id;
      this.selectedId = null;
      return cx.notify();
    }
    const item = this.selected();
    const action = ACTIONS.find((a) => a.key === k);
    if (item && action) this.act(action, item, cx);
  }

  act(action, item, cx) {
    if (action.when && !action.when(item)) return;
    if (action.confirm) {
      this.confirming = item.id;
      this.mergeError = null;
      return cx.notify();
    }
    if (action.closes) this.close();
    action.run(item);
  }

  merge(item, cx) {
    this.job = { item, phase: "merging" };
    this.mergeError = null;
    cx.notify();
    cx.spawn(async (cx) => {
      try {
        await mergePr(item, (phase) => {
          this.job = { item, phase };
          cx.notify();
        });
        this.job = { item, phase: "done" };
        cx.notify();
        notify({ title: `Merged ${short(item.repo)}#${item.number}`, body: item.title, timeout: 4000 });
        cx.timer.after(1400, (cx) => {
          this.job = null;
          this.confirming = null;
          this.refresh(cx);
        });
      } catch (e) {
        this.mergeError = String(e?.message ?? e);
        this.job = null;
        cx.notify();
      }
    });
  }

  progress(job, t) {
    const { item, phase } = job;
    const done = phase === "done";
    const verb = (METHOD_LABEL[item.method] ?? "Merge").replace(" and merge", "-merging").replace(/^Merge$/, "Merging");
    const text = done
      ? `Merged ${short(item.repo)}#${item.number} into ${item.base}`
      : phase === "approving"
        ? `Approving ${short(item.repo)}#${item.number}…`
        : `${verb} ${short(item.repo)}#${item.number} into ${item.base}…`;
    return v_flex()
      .gap(12)
      .p(14)
      .rounded(8)
      .border_1()
      .border_color(t.done)
      .bg(tint(t.done, "14"))
      .child(
        h_flex()
          .gap(10)
          .items_center()
          .child(
            done
              ? div().size(16).flex_none().rounded(8).bg(t.done).text_color(t.background).text_size(11).font_bold().flex().items_center().justify_center().child("✓")
              : svg("mods/workspace-chrome/spinner.svg").size(16).flex_none().text_color(t.done).spin(800),
          )
          .child(div().flex_1().min_w(0).truncate().font_semibold().text_color(t.foreground).child(text)),
      );
  }

  actionsFor(sel, t) {
    if (this.job) return this.progress(this.job, t);
    if (this.confirming === sel.id) {
      const verb = mergeLabel(sel);
      return v_flex()
        .gap(10)
        .p(14)
        .rounded(8)
        .border_1()
        .border_color(t.done)
        .bg(tint(t.done, "14"))
        .child(div().font_semibold().text_color(t.foreground).child(`${verb} ${short(sel.repo)}#${sel.number} into ${sel.base}?`))
        .when(!sel.mine && !sel.approvedAtHead, (el) => el.child(div().text_size(12).text_color(t.muted).child("This approves it as you first.")))
        .child(div().text_size(12).text_color(t.muted).child("Only merges if nothing was pushed since this list was loaded."))
        .when(Boolean(this.mergeError), (el) => el.child(div().text_size(12).text_color(t.blocked).child(this.mergeError)))
        .child(
          h_flex()
            .gap(8)
            .child(this.button("merge-yes", verb, "↵", "merge", t, (_e, cx) => this.merge(sel, cx)))
            .child(
              this.button("merge-no", "Cancel", "n", null, t, (_e, cx) => {
                this.confirming = null;
                cx.notify();
              }),
            ),
        );
    }
    const shown = ACTIONS.filter((a) => !a.when || a.when(sel));
    const rows = [shown.filter((a) => a.primary), shown.filter((a) => !a.primary)];
    return v_flex()
      .gap(8)
      .children(
        rows.map((group, row) =>
          h_flex()
            .id(`acts-${row}`)
            .gap(8)
            .children(
              group.map((a) =>
                this.button(
                  `act-${a.key}`,
                  typeof a.label === "function" ? a.label(sel) : a.label,
                  a.hint ?? a.key,
                  a.confirm ? "merge" : a.primary ? "primary" : null,
                  t,
                  (_e, cx) => this.act(a, sel, cx),
                ),
              ),
            ),
        ),
      );
  }

  renderInbox(_cx) {
    const t = theme();
    const data = inbox();
    const list = this.listed();
    const sel = this.selected();

    const tabs = GROUPS.map((g) => {
      const on = g.id === this.group;
      const n = data.items.filter((i) => i.group === g.id).length;
      return h_flex()
        .id(`tab-${g.id}`)
        .px(10)
        .h(28)
        .gap(6)
        .items_center()
        .rounded(6)
        .cursor_pointer()
        .when(on, (el) => el.bg(t.selected))
        .hover((s) => s.bg(t.hover))
        .on_click((_e, cx) => {
          this.group = g.id;
          this.selectedId = null;
          cx.notify();
        })
        .child(div().text_color(on ? t.foreground : t.muted).child(g.label))
        .child(div().text_size(11).text_color(t.dim).child(String(n)));
    });

    const rows = list.map((item) => {
      const on = sel?.id === item.id;
      return v_flex()
        .id(`row-${item.id}`)
        .px(12)
        .py(8)
        .gap(2)
        .rounded(6)
        .cursor_pointer()
        .when(on, (el) => el.bg(t.selected))
        .hover((s) => s.bg(t.hover))
        .on_click((_e, cx) => {
          this.selectedId = item.id;
          cx.notify();
        })
        .child(
          h_flex()
            .gap(8)
            .items_center()
            .child(ciDot(t, item.ci))
            .child(div().flex_none().text_color(t.foreground).child(short(item.repo)))
            .child(div().flex_none().text_color(t.dim).child(`#${item.number}`))
            .child(div().flex_1())
            .child(div().flex_none().text_size(11).text_color(t.dim).child(age(item.updatedAt))),
        )
        .child(div().truncate().text_color(t.foreground).child(item.title))
        .child(div().truncate().text_size(11.5).text_color(whyColor(t, item)).child(item.why));
    });

    const chip = (text, color) => div().px(8).py(2).rounded(4).bg(t.surface).text_size(11.5).text_color(color ?? t.muted).child(text);

    const detail = sel
      ? v_flex()
          .flex_1()
          .min_w(0)
          .p(20)
          .gap(12)
          .child(div().text_size(11.5).text_color(t.dim).child(`${sel.repo}#${sel.number} · ${sel.author}${sel.bot ? " (bot)" : ""}`))
          .child(div().text_size(17).font_semibold().text_color(t.foreground).child(sel.title))
          .child(div().text_size(12).text_color(sel.group === "ready" ? t.done : sel.rank === 0 ? t.ansi3 : t.muted).child(sel.why))
          .child(
            h_flex()
              .gap(6)
              .flex_wrap()
              .child(chip(sel.branch, t.ansi5))
              .child(
                h_flex()
                  .px(8)
                  .py(2)
                  .gap(4)
                  .rounded(4)
                  .bg(t.surface)
                  .text_size(11.5)
                  .child(div().text_color(t.ansi2).child(`+${sel.add}`))
                  .child(div().text_color(t.ansi1).child(`-${sel.del}`))
                  .child(div().text_color(t.muted).child(`· ${sel.files} files`)),
              )
              .when(Boolean(sel.decision), (el) => el.child(chip(sel.decision))),
          )
          .child(div().pt(6).text_size(10.5).font_bold().text_color(t.dim).child("CHECKS"))
          .when(sel.checks.length === 0, (el) => el.child(div().text_size(12).text_color(t.dim).child("No checks on the latest commit")))
          .children(
            sel.checks.map((c, i) =>
              h_flex()
                .id(`check-${i}`)
                .gap(10)
                .items_center()
                .child(ciDot(t, c.state))
                .child(div().flex_1().min_w(0).truncate().text_color(t.foreground).child(c.name))
                .child(div().flex_none().text_size(11).text_color(t.dim).child(c.label)),
            ),
          )
          .child(div().flex_1())
          .child(this.actionsFor(sel, t))
      : v_flex().flex_1().p(20).text_color(t.dim).child(data.at ? "Nothing here." : "Checking GitHub…");

    const status = this.busy ? "Refreshing…" : data.error ? data.error : data.at ? `Checked ${age(data.at)} ago` : "";

    return v_flex()
      .id("prs-inbox")
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
          .px(14)
          .py(10)
          .gap(4)
          .items_center()
          .border_b_1()
          .border_color(t.border)
          .child(div().mr(10).font_semibold().text_color(t.foreground).child("Pull requests"))
          .children(tabs)
          .child(div().flex_1())
          .child(div().text_size(11).text_color(data.error && !this.busy ? t.ansi1 : t.dim).truncate().child(status)),
      )
      .child(
        h_flex()
          .flex_1()
          .min_h(0)
          .items_stretch()
          .child(
            v_flex()
              .id("prs-rows")
              .w(400)
              .flex_none()
              .p(6)
              .border_r_1()
              .border_color(t.border)
              .overflow_y_scrollbar()
              .track_scroll(this.scroll)
              .children(rows),
          )
          .child(detail),
      )
      .child(
        div()
          .px(14)
          .py(8)
          .border_t_1()
          .border_color(t.border)
          .text_size(11.5)
          .text_color(t.dim)
          .child("1-5 or tab group · ↑↓ / jk move · ↵ Hunk · m merge · o PR · f files · c checks · g repo · y copy · r refresh · esc close"),
      );
  }
}
