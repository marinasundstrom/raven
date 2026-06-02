#!/usr/bin/env bash
# Build sample projects under samples/projects. Failing one won't stop the others.

set -Euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECTS_DIR="$ROOT_DIR/samples/projects"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"
OUTPUT_DIR="${OUTPUT_DIR:-$PROJECTS_DIR/output}"
BUILD_REPORT_TSV="${BUILD_REPORT_TSV:-$OUTPUT_DIR/build-report.tsv}"
BUILD_REPORT_MD="${BUILD_REPORT_MD:-$OUTPUT_DIR/build-report.md}"
INCLUDE_TEMPORARY=0
INCLUDE_CSPROJ=1

usage() {
  cat <<EOF
Usage: scripts/build-project-samples.sh [options] [filter...]

Options:
  -c, --configuration <c> Build configuration (default: ${BUILD_CONFIG})
      --include-temporary Include tmp-* project folders
      --rvn-only          Build only .rvnproj sample projects
  -h, --help              Show this help

Filters:
  filter                  Optional path/name/glob filter(s) for project selection.
                          Examples:
                            scripts/build-project-samples.sh aspnet-minimal-api
                            scripts/build-project-samples.sh 'macro-*'

Environment overrides:
  BUILD_CONFIG, OUTPUT_DIR, BUILD_REPORT_TSV, BUILD_REPORT_MD, DOTNET_BUILD_ARGS
EOF
}

FILTERS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -c|--configuration)
      [[ $# -lt 2 ]] && { echo "Missing value for $1"; exit 2; }
      BUILD_CONFIG="$2"
      shift 2
      ;;
    --include-temporary)
      INCLUDE_TEMPORARY=1
      shift
      ;;
    --rvn-only)
      INCLUDE_CSPROJ=0
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      FILTERS+=("$1")
      shift
      ;;
  esac
done

mkdir -p "$OUTPUT_DIR"

timestamp_ms() {
  perl -MTime::HiRes=time -e 'printf "%d\n", time() * 1000'
}

format_duration_ms() {
  perl -e 'printf "%.3f", $ARGV[0] / 1000' "$1"
}

escape_markdown_cell() {
  local value="$1"
  value="${value//|/\\|}"
  printf '%s' "$value"
}

is_temporary_project() {
  local path="$1"

  case "$path" in
    */tmp-*/*|*/tmp_*/*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

matches_filters() {
  local path="$1"

  if (( ${#FILTERS[@]} == 0 )); then
    return 0
  fi

  local filter
  for filter in "${FILTERS[@]}"; do
    if [[ "$path" == *"$filter"* || "$path" == $filter ]]; then
      return 0
    fi
  done

  return 1
}

collect_projects() {
  local find_args=(-name '*.rvnproj')

  if [[ "$INCLUDE_CSPROJ" == "1" ]]; then
    find_args=(-name '*.rvnproj' -o -name '*.csproj')
  fi

  while IFS= read -r project; do
    local relative="${project#"$ROOT_DIR/"}"

    if [[ "$INCLUDE_TEMPORARY" != "1" ]] && is_temporary_project "$relative"; then
      continue
    fi

    matches_filters "$relative" || continue
    printf '%s\n' "$relative"
  done < <(
    find "$PROJECTS_DIR" \
      \( -path '*/.raven' -o -path '*/bin' -o -path '*/obj' -o -path '*/output' \) -prune -o \
      -type f \( "${find_args[@]}" \) -print |
      sort
  )
}

PROJECTS=()
while IFS= read -r project; do
  PROJECTS+=("$project")
done < <(collect_projects)

if (( ${#PROJECTS[@]} == 0 )); then
  echo "No sample projects matched."
  exit 0
fi

rows=()
failures=()
pass_count=0
fail_count=0

dotnet_build_args=()
if [[ -n "${DOTNET_BUILD_ARGS:-}" ]]; then
  # shellcheck disable=SC2206
  dotnet_build_args=(${DOTNET_BUILD_ARGS})
fi

for project in "${PROJECTS[@]}"; do
  echo
  echo "Building $project"

  start_ms="$(timestamp_ms)"

  if (( ${#dotnet_build_args[@]} > 0 )); then
    if dotnet build "$ROOT_DIR/$project" --configuration "$BUILD_CONFIG" /property:WarningLevel=0 "${dotnet_build_args[@]}"; then
      build_exit_code=0
    else
      build_exit_code=$?
    fi
  else
    if dotnet build "$ROOT_DIR/$project" --configuration "$BUILD_CONFIG" /property:WarningLevel=0; then
      build_exit_code=0
    else
      build_exit_code=$?
    fi
  fi

  if (( build_exit_code == 0 )); then
    exit_code=0
    status="PASS"
    (( pass_count += 1 ))
  else
    exit_code=$build_exit_code
    status="FAIL"
    failures+=("$project")
    (( fail_count += 1 ))
  fi

  end_ms="$(timestamp_ms)"
  wall_ms=$(( end_ms - start_ms ))
  rows+=("$project"$'\t'"$status"$'\t'"$wall_ms"$'\t'"$exit_code")
done

{
  printf 'project\tstatus\twall_ms\twall_s\texit_code\n'
  for row in "${rows[@]}"; do
    IFS=$'\t' read -r project status wall_ms exit_code <<< "$row"
    printf '%s\t%s\t%s\t%s\t%s\n' \
      "$project" \
      "$status" \
      "$wall_ms" \
      "$(format_duration_ms "$wall_ms")" \
      "$exit_code"
  done
} > "$BUILD_REPORT_TSV"

{
  echo "| Project | Status | Wall time | Exit code |"
  echo "|---|---:|---:|---:|"
  for row in "${rows[@]}"; do
    IFS=$'\t' read -r project status wall_ms exit_code <<< "$row"
    printf '| %s | %s | %ss | %s |\n' \
      "$(escape_markdown_cell "$project")" \
      "$status" \
      "$(format_duration_ms "$wall_ms")" \
      "$exit_code"
  done
} > "$BUILD_REPORT_MD"

echo
echo "Project sample build report:"
echo "  $BUILD_REPORT_TSV"
echo "  $BUILD_REPORT_MD"
echo "Passed: $pass_count"
echo "Failed: $fail_count"

if (( fail_count > 0 )); then
  echo
  echo "Failed projects:"
  printf '  %s\n' "${failures[@]}"
  exit 1
fi
