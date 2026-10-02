usage() {
  echo "Usage: gh-setup-repo [OWNER/NAME]"
  echo
  echo "Apply standard settings, branch ruleset and security features to an existing GitHub repo."
  echo "Defaults to the repo of the current directory."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

repo="${1:-$(gh repo view --json nameWithOwner --jq .nameWithOwner)}"
ruleset_name="protect-default-branch"
failures=0

step() {
  printf '==> %s\n' "$1"
}

warn() {
  printf '  ! %s\n' "$1" >&2
  failures=$((failures + 1))
}

step "Configuring $repo"

step "Repository settings"
gh api --silent --method PATCH "repos/$repo" \
  -F has_wiki=false \
  -F has_projects=false \
  -F allow_squash_merge=true \
  -F allow_merge_commit=false \
  -F allow_rebase_merge=false \
  -F allow_auto_merge=false \
  -F delete_branch_on_merge=true \
  -f squash_merge_commit_title=PR_TITLE \
  -f squash_merge_commit_message=BLANK

step "Vulnerability alerts"
gh api --silent --method PUT "repos/$repo/vulnerability-alerts" ||
  warn "could not enable vulnerability alerts"

step "Dependabot security updates"
gh api --silent --method PUT "repos/$repo/automated-security-fixes" ||
  warn "could not enable Dependabot security updates"

step "Secret scanning and push protection"
jq -n '{
  security_and_analysis: {
    secret_scanning: { status: "enabled" },
    secret_scanning_push_protection: { status: "enabled" }
  }
}' | gh api --silent --method PATCH "repos/$repo" --input - ||
  warn "could not enable secret scanning (private repos need GitHub Advanced Security)"

step "Default branch ruleset"
ruleset=$(jq -n --arg name "$ruleset_name" '{
  name: $name,
  target: "branch",
  enforcement: "active",
  conditions: {
    ref_name: { include: ["~DEFAULT_BRANCH"], exclude: [] }
  },
  bypass_actors: [
    { actor_id: 5, actor_type: "RepositoryRole", bypass_mode: "always" }
  ],
  rules: [
    { type: "deletion" },
    { type: "non_fast_forward" },
    { type: "required_linear_history" },
    {
      type: "pull_request",
      parameters: {
        required_approving_review_count: 0,
        dismiss_stale_reviews_on_push: false,
        require_code_owner_review: false,
        require_last_push_approval: false,
        required_review_thread_resolution: false,
        allowed_merge_methods: ["squash"]
      }
    }
  ]
}')

if existing_id=$(gh api "repos/$repo/rulesets" --jq ".[] | select(.name == \"$ruleset_name\") | .id"); then
  if [[ -n "$existing_id" ]]; then
    echo "$ruleset" | gh api --silent --method PUT "repos/$repo/rulesets/$existing_id" --input - ||
      warn "could not update ruleset"
  else
    echo "$ruleset" | gh api --silent --method POST "repos/$repo/rulesets" --input - ||
      warn "could not create ruleset (private repos on a free plan need GitHub Pro)"
  fi
else
  warn "could not list rulesets (private repos on a free plan need GitHub Pro)"
fi

if ((failures > 0)); then
  step "Done with $failures warning(s)"
  exit 1
fi

step "Done"
