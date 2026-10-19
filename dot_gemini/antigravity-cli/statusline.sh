#!/usr/bin/env bash
# ==============================================================================
# Antigravity CLI Statusline Script
# ==============================================================================

# Read JSON payload piped from stdin
input=$(cat)

# Terminal width fallback if the payload has no terminal_width field
fallback_width=${COLUMNS:-$(tput cols 2>/dev/null)}
[[ $fallback_width =~ ^[0-9]+$ ]] || fallback_width=80

# Format in a single jq pass: shortcuts hint on the left, stats right-aligned
printf '%s' "$input" | jq -r --argjson fw "$fallback_width" '
  def k: if . >= 1000 then "\((. / 100 | floor) / 10)k" else tostring end;
  def dur:
    if . >= 86400 then "\(. / 86400 | floor)d\((. % 86400) / 3600 | floor)h"
    elif . >= 3600 then "\(. / 3600 | floor)h\((. % 3600) / 60 | floor)m"
    else "\(. / 60 | floor)m" end;
  def pct: "\(. * 100 | floor)%";
  def bucket($name; $label):
    .quota[$name] as $q
    | if $q then "\($label) \($q.remaining_fraction | pct) (\($q.reset_in_seconds | dur))" else empty end;

  (.model.display_name // .model.name // "Gemini") as $model
  | (.context_window.current_usage // {}) as $u
  | (($u.input_tokens // 0) + ($u.cache_read_input_tokens // 0) + ($u.cache_creation_input_tokens // 0)) as $ctx
  | (if ($model | test("gemini"; "i")) then "gemini" else "3p" end) as $fam
  | ([bucket("\($fam)-5h"; "5h"), bucket("\($fam)-weekly"; "wk")] | join(" · ")) as $quota
  | "? for shortcuts" as $left
  | "🤖 \($model) | 📊 ctx \($ctx | k) · out \(($u.output_tokens // 0) | k) | ⚡ \(if $quota == "" then "N/A" else $quota end)" as $right
  | (.terminal_width // $fw | floor) as $w
  # The 3 emoji each take 2 columns but count as 1 char; keep 1 column of margin
  | ([$w - ($left | length) - ($right | length) - 3 - 1, 1] | max) as $pad
  | $left + (" " * $pad) + $right
'
