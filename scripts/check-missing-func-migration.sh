#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  scripts/check-missing-func-migration.sh [--base <git-ref>]
  scripts/check-missing-func-migration.sh --files <path> [<path> ...]

Checks changed Raven sources/snippets for missing `func` migration violations.
EOF
}

base_ref=""
declare -a explicit_files=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --base)
      base_ref="${2:-}"
      if [[ -z "$base_ref" ]]; then
        echo "Missing value for --base"
        exit 2
      fi
      shift 2
      ;;
    --files)
      shift
      while [[ $# -gt 0 ]]; do
        explicit_files+=("$1")
        shift
      done
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1"
      usage
      exit 2
      ;;
  esac
done

declare -a candidate_files=()

if [[ ${#explicit_files[@]} -gt 0 ]]; then
  candidate_files=("${explicit_files[@]}")
elif [[ -n "$base_ref" ]]; then
  while IFS= read -r file; do
    [[ -n "$file" ]] && candidate_files+=("$file")
  done < <(cd "$ROOT_DIR" && git diff --name-only --diff-filter=ACMRTUXB "$base_ref"...HEAD)
else
  while IFS= read -r file; do
    [[ -n "$file" ]] && candidate_files+=("$file")
  done < <(cd "$ROOT_DIR" && git status --porcelain | awk '{print $2}')
fi

if [[ ${#candidate_files[@]} -eq 0 ]]; then
  echo "No candidate files."
  exit 0
fi

declare -a rav_files=()
declare -a cs_files=()

for file in "${candidate_files[@]}"; do
  case "$file" in
    *.rav)
      rav_files+=("$file")
      ;;
    test/Raven.CodeAnalysis.Tests/*.cs|test/Raven.CodeAnalysis.Tests/**/*.cs)
      cs_files+=("$file")
      ;;
  esac
done

declare -a violations=()

if [[ ${#rav_files[@]} -gt 0 ]]; then
  for file in "${rav_files[@]}"; do
    abs_file="$ROOT_DIR/$file"
    [[ -f "$abs_file" ]] || continue

    output="$(dotnet run --project "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" --property WarningLevel=0 -- "$abs_file" --no-emit 2>&1 || true)"
    if grep -q "RAV0909" <<<"$output"; then
      violations+=("$file: contains RAV0909 (missing func)")
    fi
  done
fi

if [[ ${#cs_files[@]} -gt 0 ]]; then
  for file in "${cs_files[@]}"; do
    case "$file" in
      test/Raven.CodeAnalysis.Tests/Syntax/ClassDeclarationParserTests.cs|\
      test/Raven.CodeAnalysis.Tests/Semantics/Diagnostics/MethodFuncKeywordDiagnosticsTests.cs)
        continue
        ;;
    esac

    abs_file="$ROOT_DIR/$file"
    [[ -f "$abs_file" ]] || continue

    # Migration guard for Raven snippets embedded in tests:
    # detect member-style `(...) ->` declarations that omit `func`.
    if rg -n --pcre2 '^\s*(?:(?:public|private|internal|protected)\s+)?(?:(?:static|override|virtual|abstract|sealed|unsafe|extern|async)\s+)*(?!func\b)(?!operator\b)(?!self\b)[A-Za-z_][A-Za-z0-9_]*(?:<[^>\n]+>)?\s*\([^)\n]*\)\s*->' "$abs_file" >/dev/null; then
      violations+=("$file: contains member declaration(s) without func")
    fi
  done
fi

if [[ ${#violations[@]} -gt 0 ]]; then
  echo "Missing-func migration guard failed:"
  for violation in "${violations[@]}"; do
    echo "  - $violation"
  done
  exit 1
fi

echo "Missing-func migration guard passed."
