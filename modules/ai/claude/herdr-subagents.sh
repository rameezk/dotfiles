#!/usr/bin/env bash

[ -n "$HC_TERMINAL_ID" ] || exit 0

dir="$HOME/.local/state/herdr-fuel"
log="$dir/$HC_TERMINAL_ID.subagents.jsonl"
mkdir -p "$dir"

input=$(cat)
event=$(jq -r '.hook_event_name // empty' <<<"$input")

case "$event" in
  SessionStart)
    if [ -z "$(jq -r '.agent_id // empty' <<<"$input")" ] && [ "$(jq -r '.source // empty' <<<"$input")" != "compact" ]; then
      : >"$log"
    fi
    ;;
  SubagentStart)
    jq -c '{id: .agent_id, type: .agent_type, ev: "start", at: (now | floor)}' <<<"$input" >>"$log"
    ;;
  SubagentStop)
    jq -c '{id: .agent_id, ev: "stop", at: (now | floor)}' <<<"$input" >>"$log"
    ;;
  PostToolUse)
    jq -c '.tool_response
      | (if type == "string" then (fromjson? // {}) else . end)
      | select(type == "object" and (.agentId | type) == "string")
      | {id: .agentId, desc: .description}' <<<"$input" >>"$log"
    ;;
esac

exit 0
