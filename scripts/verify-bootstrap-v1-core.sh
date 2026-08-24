#!/usr/bin/env bash
# Verify the checked-in bootstrap-v1 Raven.Core seed before it is consumed.

set -euo pipefail

REPOSITORY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_ROOT="${1:-$REPOSITORY_ROOT/eng/bootstrap/v1}"

usage() {
  cat <<'EOF'
Usage: scripts/verify-bootstrap-v1-core.sh [bootstrap-root]

Verify the manifest and SHA-256 checksums for the checked-in bootstrap-v1 Core
assemblies. Future bootstrap-v2 builds should run this before referencing them.
EOF
}

if [[ "$BOOTSTRAP_ROOT" == "-h" || "$BOOTSTRAP_ROOT" == "--help" ]]; then
  usage
  exit 0
fi

if [[ ! -d "$BOOTSTRAP_ROOT" ]]; then
  echo "Bootstrap-v1 Core root does not exist: $BOOTSTRAP_ROOT" >&2
  echo "The seed is checked in only after bootstrap v1 has qualified." >&2
  exit 1
fi

for required_file in manifest.json SHA256SUMS net10.0/Raven.Core.dll net11.0/Raven.Core.dll; do
  if [[ ! -f "$BOOTSTRAP_ROOT/$required_file" ]]; then
    echo "Bootstrap-v1 Core seed is missing $required_file." >&2
    exit 1
  fi
done

if ! grep -Fq '"result": "passed"' "$BOOTSTRAP_ROOT/manifest.json"; then
  echo "Bootstrap-v1 manifest does not record a passing IL verification gate." >&2
  exit 1
fi

while IFS= read -r checksum_entry; do
  checked_path="${checksum_entry#*  }"
  case "$checked_path" in
    ""|/*|../*|*/../*|*/..)
      echo "Unsafe path in bootstrap SHA256SUMS: $checked_path" >&2
      exit 1
      ;;
  esac
done < "$BOOTSTRAP_ROOT/SHA256SUMS"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$BOOTSTRAP_ROOT" && sha256sum --check SHA256SUMS)
else
  (cd "$BOOTSTRAP_ROOT" && shasum -a 256 --check SHA256SUMS)
fi

echo "Bootstrap-v1 Core seed verified: $BOOTSTRAP_ROOT"
