#!/usr/bin/env bash
set -euo pipefail

REPOSITORY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXTENSION_DEVELOPMENT=0
DRY_RUN=0

while (( $# > 0 )); do
  case "$1" in
    --extension-development)
      EXTENSION_DEVELOPMENT=1
      shift
      ;;
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    -h|--help)
      echo "Usage: scripts/code-development.sh [--extension-development] [--dry-run] [code arguments...]"
      echo "Launch VS Code with Raven configured to use repository build outputs."
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

if ! command -v code >/dev/null 2>&1; then
  echo "The VS Code 'code' command is not available on PATH." >&2
  exit 1
fi

# shellcheck source=./raven-env.sh
source "$REPOSITORY_ROOT/scripts/raven-env.sh"
export RAVEN_VSCODE_DEVELOPMENT="repository"

if (( $# == 0 )); then
  set -- "$REPOSITORY_ROOT"
fi

if [[ "$EXTENSION_DEVELOPMENT" == "1" ]]; then
  VSCODE_ENVIRONMENT_ROOT="$REPOSITORY_ROOT/artifacts/vscode-extension-development"
else
  VSCODE_ENVIRONMENT_ROOT="$REPOSITORY_ROOT/artifacts/vscode-development"
fi

CODE_ARGUMENTS=(
  --new-window
  --user-data-dir "$VSCODE_ENVIRONMENT_ROOT/user-data"
)
if [[ "$EXTENSION_DEVELOPMENT" == "1" ]]; then
  CODE_ARGUMENTS+=(--extensionDevelopmentPath="$REPOSITORY_ROOT/src/Raven.VSCode")
fi
CODE_ARGUMENTS+=("$@")

if [[ "$DRY_RUN" == "1" ]]; then
  printf 'Environment:\n'
  raven-env-info
  printf 'Command:\n  code'
  printf ' %q' "${CODE_ARGUMENTS[@]}"
  printf '\n'
  exit 0
fi

exec code "${CODE_ARGUMENTS[@]}"
