#!/usr/bin/env bash

input=$(cat)

model=$(echo "$input" | jq -r '.model.display_name')
effort=$(echo "$input" | jq -r '.effort.level // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
input_tokens=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
output_tokens=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')

format_tokens() {
  local tokens=$1
  if [ "$tokens" -ge 1000 ]; then
    printf "%.1fk" "$(echo "$tokens / 1000" | bc -l)"
  else
    printf "%d" "$tokens"
  fi
}

parts=("$model")

if [ -n "$effort" ]; then
  parts+=("Effort: $effort")
fi

total_tokens=$((input_tokens + output_tokens))
if [ "$total_tokens" -gt 0 ]; then
  parts+=("Tokens: $(format_tokens "$total_tokens")")
fi

if [ -n "$used" ]; then
  parts+=("Context: $(printf '%.0f' "$used")% used")
fi

output=""
for part in "${parts[@]}"; do
  if [ -z "$output" ]; then
    output="$part"
  else
    output="$output | $part"
  fi
done

printf "\033[2m%s\033[0m" "$output"

if [ -n "$HC_TERMINAL_ID" ]; then
  fuel_dir="$HOME/.local/state/herdr-fuel"
  fuel_file="$fuel_dir/$HC_TERMINAL_ID.json"
  mkdir -p "$fuel_dir"
  if echo "$input" | jq -c '{
      harness: "claude",
      model: .model.display_name,
      tokens: ((.context_window.current_usage // {}) | ((.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0))),
      window: .context_window.context_window_size,
      at: (now | floor)
    }' > "$fuel_file.tmp" 2>/dev/null; then
    mv "$fuel_file.tmp" "$fuel_file"
  else
    rm -f "$fuel_file.tmp"
  fi
fi
