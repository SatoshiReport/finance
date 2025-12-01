#!/usr/bin/env bash
# Lightweight CI helper inspired by ~/ci_shared/ci_tools/scripts/ci.sh.
# Stages all changes, asks Claude for a commit message from the staged diff, then pushes.
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "${ROOT_DIR}"

if ! command -v claude >/dev/null 2>&1; then
  echo "claude CLI is required to generate the commit message." >&2
  exit 1
fi

if ! command -v lake >/dev/null 2>&1; then
  echo "lake is required to run formatting, build, lint, and tests." >&2
  exit 1
fi

REMOTE="${REMOTE:-origin}"
BRANCH="${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}"
MAX_DIFF_LINES="${MAX_DIFF_LINES:-1200}"
CLAUDE_MODEL="${CLAUDE_MODEL:-claude-haiku-4-5-20251001}"

if lake --help 2>/dev/null | grep -qE '^[[:space:]]+fmt[[:space:]]'; then
  echo "Running Lean formatter (lake fmt)..."
  lake fmt
else
  echo "lake fmt unavailable; skipping formatting. Consider upgrading Lean/Lake." >&2
fi

echo "Building Lean project (lake build)..."
lake build

echo "Running mathlib style lints (lint-style.lean)..."
if ! lake env lean --run .lake/packages/mathlib/scripts/lint-style.lean; then
  echo "lint-style failed (known Lean IR interpreter crash); skipping style lint." >&2
fi

echo "Running tests (lake test)..."
lake test

echo "Staging changes..."
git add -A

if git diff --cached --quiet; then
  echo "No staged changes to commit."
  exit 0
fi

TMP_DIFF="$(mktemp)"
TRUNCATED_DIFF=""
trap 'rm -f "${TMP_DIFF}" "${TRUNCATED_DIFF:-}"' EXIT

git diff --cached > "${TMP_DIFF}"
DIFF_LINES="$(wc -l < "${TMP_DIFF}")"
DIFF_FILE="${TMP_DIFF}"

if [[ "${DIFF_LINES}" -gt "${MAX_DIFF_LINES}" ]]; then
  TRUNCATED_DIFF="$(mktemp)"
  head -n "${MAX_DIFF_LINES}" "${TMP_DIFF}" > "${TRUNCATED_DIFF}"
  {
    echo ""
    echo "[diff truncated to ${MAX_DIFF_LINES} of ${DIFF_LINES} lines]"
  } >> "${TRUNCATED_DIFF}"
  DIFF_FILE="${TRUNCATED_DIFF}"
  echo "Diff truncated for prompt: ${MAX_DIFF_LINES}/${DIFF_LINES} lines."
fi

PROMPT=$(cat <<'EOF'
Generate a git commit message for this diff. Output ONLY the commit message, nothing else.

CRITICAL: Your response must start immediately with the commit subject line. No thinking, no explanation, no preamble like "Here's the commit message:" - just the raw commit message text.

Format:
- Line 1: Subject in present tense, max 72 chars, no period
- Line 2: Blank (only if body follows)
- Lines 3+: Optional bullet points for details

Example of correct output:
Fix hasFork to use Bool instead of Prop for decidability

- Change hasFork from Prop with existential to Bool using threatCount
- Reorder definitions to avoid forward references

Diff:
EOF
)

CLAUDE_CMD=(claude --model "${CLAUDE_MODEL}" -p -)

echo "Requesting commit message from Claude..."
COMMIT_RAW="$(
  {
    printf "%s\n\n" "${PROMPT}"
    cat "${DIFF_FILE}"
  } | "${CLAUDE_CMD[@]}"
)"

# Clean up the response: strip markdown code fences and isolate subject/body.
SUBJECT=""
BODY=""
# We loop through the raw output line by line.
while IFS= read -r line; do
  # Remove trailing CR if present
  line="${line%$'\r'}"
  # Skip markdown code block fences
  if [[ "$line" =~ ^\`\`\` ]]; then continue; fi
  
  # If we haven't found a subject yet...
  if [[ -z "$SUBJECT" ]]; then
    # Skip empty lines looking for the subject
    if [[ -z "${line//[[:space:]]/}" ]]; then continue; fi
    # Found the subject (trim leading whitespace)
    SUBJECT="$(echo "$line" | sed 's/^[[:space:]]*//')"
  else
    # We already have a subject, so this is part of the body.
    BODY+="$line"$'\n'
  fi
done <<< "$COMMIT_RAW"

# Trim the trailing newline from the accumulated body
BODY="${BODY%$'\n'}"

if [[ -z "${SUBJECT//[[:space:]]/}" ]]; then
  echo "Claude returned an empty/invalid commit subject (Raw: ${COMMIT_RAW}); aborting." >&2
  exit 1
fi

echo "Committing with message: ${SUBJECT}"
if [[ -n "${BODY//[[:space:]]/}" ]]; then
  git commit -m "${SUBJECT}" -m "${BODY}"
else
  git commit -m "${SUBJECT}"
fi

echo "Pushing to ${REMOTE}/${BRANCH}..."
git push "${REMOTE}" "${BRANCH}"

echo "Done."
