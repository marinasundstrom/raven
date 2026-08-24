#!/usr/bin/env bash
set -euo pipefail

REPOSITORY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SKIP_BUILD=0

case "${1:-}" in
  --no-build)
    SKIP_BUILD=1
    shift
    ;;
  -h|--help)
    echo "Usage: scripts/code-extension-development.sh [--no-build] [code arguments...]"
    echo "Build and launch the Raven extension in an isolated VS Code Extension Development Host."
    exit 0
    ;;
esac

if [[ "$SKIP_BUILD" == "0" ]]; then
  "$REPOSITORY_ROOT/scripts/build-development-environment.sh"
fi

exec "$REPOSITORY_ROOT/scripts/code-development.sh" --extension-development "$@"
