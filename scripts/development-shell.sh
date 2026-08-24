#!/usr/bin/env bash
set -euo pipefail

REPOSITORY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# shellcheck source=./raven-env.sh
source "$REPOSITORY_ROOT/scripts/raven-env.sh"

DEVELOPMENT_SHELL="${RAVEN_DEVELOPMENT_SHELL:-${SHELL:-/bin/bash}}"
echo
echo "Starting an isolated Raven repository shell. Exit it to return to the previous environment."

case "$(basename "$DEVELOPMENT_SHELL")" in
  bash)
    exec "$DEVELOPMENT_SHELL" --noprofile --norc -i
    ;;
  zsh)
    exec "$DEVELOPMENT_SHELL" -f
    ;;
  *)
    exec "$DEVELOPMENT_SHELL" -i
    ;;
esac
