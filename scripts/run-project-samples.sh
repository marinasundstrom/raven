#!/usr/bin/env bash
# Run executable sample projects after they have been built.

set -Euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECTS_DIR="$ROOT_DIR/samples/projects"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"
OUTPUT_DIR="${OUTPUT_DIR:-$PROJECTS_DIR/output}"
RUN_REPORT_TSV="${RUN_REPORT_TSV:-$OUTPUT_DIR/run-report.tsv}"
RUN_REPORT_MD="${RUN_REPORT_MD:-$OUTPUT_DIR/run-report.md}"
CLASSIFICATIONS_FILE="${CLASSIFICATIONS_FILE:-$PROJECTS_DIR/run-classifications.tsv}"
DEFAULT_TIMEOUT_SECONDS="${RUN_TIMEOUT_SECONDS:-30}"
SAMPLE_TOOLCHAIN="${RAVEN_SAMPLE_TOOLCHAIN:-repository}"
INCLUDE_TEMPORARY=0
INCLUDE_CSPROJ=1

usage() {
  cat <<EOF
Usage: scripts/run-project-samples.sh [options] [filter...]

Run every executable sample project with --no-build. Projects that cannot be
executed unattended must have a reviewed build-only entry in
samples/projects/run-classifications.tsv.

Options:
  -c, --configuration <c> Build configuration (default: ${BUILD_CONFIG})
      --include-temporary Include tmp-* project folders
      --rvn-only          Run only .rvnproj sample projects
      --installed-toolchain
                          Use the installed Raven SDK targets and compiler
  -h, --help              Show this help

Filters:
  filter                  Optional path/name/glob filter(s) for project selection.

Environment overrides:
  BUILD_CONFIG, OUTPUT_DIR, RUN_REPORT_TSV, RUN_REPORT_MD,
  CLASSIFICATIONS_FILE, RUN_TIMEOUT_SECONDS,
  RAVEN_SAMPLE_TOOLCHAIN (repository or installed)
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
    --installed-toolchain)
      SAMPLE_TOOLCHAIN="installed"
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

case "$DEFAULT_TIMEOUT_SECONDS" in
  ''|*[!0-9]*)
    echo "RUN_TIMEOUT_SECONDS must be a positive integer."
    exit 2
    ;;
esac

if (( DEFAULT_TIMEOUT_SECONDS <= 0 )); then
  echo "RUN_TIMEOUT_SECONDS must be a positive integer."
  exit 2
fi

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
  case "$1" in
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

load_classification() {
  local project="$1"
  CLASSIFICATION="run"
  TIMEOUT_SECONDS="$DEFAULT_TIMEOUT_SECONDS"
  EXPECTED_EXIT=0
  REASON=""

  [[ -f "$CLASSIFICATIONS_FILE" ]] || return 0

  while IFS=$'\t' read -r configured_project disposition timeout_seconds expected_exit reason ||
    [[ -n "${configured_project:-}" ]]; do
    [[ -z "${configured_project:-}" || "$configured_project" == \#* ]] && continue
    [[ "$configured_project" == "$project" ]] || continue

    CLASSIFICATION="$disposition"
    TIMEOUT_SECONDS="$timeout_seconds"
    EXPECTED_EXIT="$expected_exit"
    REASON="$reason"
    return 0
  done < "$CLASSIFICATIONS_FILE"
}

run_with_timeout() {
  local timeout_seconds="$1"
  shift

  perl -MPOSIX -e '
    my $timeout = shift @ARGV;
    my $pid = fork();
    die "fork failed: $!\n" unless defined $pid;

    if ($pid == 0) {
      POSIX::setpgid(0, 0);
      exec @ARGV;
      exit 127;
    }

    my $timed_out = 0;
    $SIG{ALRM} = sub {
      $timed_out = 1;
      kill "TERM", -$pid;
      select undef, undef, undef, 0.5;
      kill "KILL", -$pid;
    };

    alarm $timeout;
    waitpid($pid, 0);
    my $status = $?;
    alarm 0;

    if ($timed_out) {
      kill "KILL", -$pid;
      exit 124;
    }

    if ($status & 127) {
      exit 128 + ($status & 127);
    }

    exit($status >> 8);
  ' "$timeout_seconds" "$@"
}

repository_compiler=""
repository_targets=""

resolve_repository_core() {
  local project_path="$1"
  local target_framework
  local target_frameworks
  local core_framework
  local candidate

  target_framework="$(dotnet msbuild "$project_path" -getProperty:TargetFramework)"
  if [[ -z "$target_framework" ]]; then
    target_frameworks="$(dotnet msbuild "$project_path" -getProperty:TargetFrameworks)"
    target_framework="${target_frameworks%%;*}"
  fi

  core_framework="${target_framework%%-*}"
  candidate="$ROOT_DIR/src/Raven.Core/bin/$BUILD_CONFIG/$core_framework/Raven.Core.dll"
  if [[ -f "$candidate" ]]; then
    printf '%s\n' "$candidate"
  fi
}

case "$SAMPLE_TOOLCHAIN" in
  repository)
    repository_compiler="$ROOT_DIR/src/Raven.Compiler/bin/$BUILD_CONFIG/net11.0/rvnc.dll"
    repository_targets="$ROOT_DIR/build/Raven.Language.targets"
    if [[ ! -f "$repository_compiler" ]]; then
      echo "Repository compiler host not found: $repository_compiler"
      echo "Build src/Raven.Compiler for net11.0 first."
      exit 1
    fi
    ;;
  installed)
    ;;
  *)
    echo "Unknown RAVEN_SAMPLE_TOOLCHAIN '$SAMPLE_TOOLCHAIN' (expected repository or installed)."
    exit 2
    ;;
esac

PROJECTS=()
while IFS= read -r project; do
  PROJECTS+=("$project")
done < <(collect_projects)

if (( ${#PROJECTS[@]} == 0 )); then
  echo "No sample projects matched."
  exit 0
fi

echo "Raven sample toolchain: $SAMPLE_TOOLCHAIN"

rows=()
failures=()
pass_count=0
fail_count=0
build_only_count=0
non_executable_count=0

for project in "${PROJECTS[@]}"; do
  project_path="$ROOT_DIR/$project"
  msbuild_args=(-property:Configuration="$BUILD_CONFIG")
  if [[ "$SAMPLE_TOOLCHAIN" == "repository" ]]; then
    repository_core="$(resolve_repository_core "$project_path")"
    msbuild_args+=("/property:RavenCompilerHost=$repository_compiler")
    if [[ -n "$repository_core" ]]; then
      msbuild_args+=("/property:RavenCoreReferencePath=$repository_core")
    fi
    if [[ "$project" == *.rvnproj ]]; then
      msbuild_args+=("/property:LanguageTargets=$repository_targets")
    fi
  fi

  if ! output_type="$(dotnet msbuild "$project_path" -getProperty:OutputType "${msbuild_args[@]}")"; then
    rows+=("$project"$'\t'"FAIL"$'\t'"evaluate"$'\t'"0"$'\t'"1"$'\t'"Could not evaluate OutputType")
    failures+=("$project (project evaluation failed)")
    (( fail_count += 1 ))
    continue
  fi

  if [[ "$output_type" != "Exe" && "$output_type" != "WinExe" ]]; then
    (( non_executable_count += 1 ))
    continue
  fi

  load_classification "$project"
  case "$CLASSIFICATION" in
    build-only)
      if [[ -z "$REASON" ]]; then
        rows+=("$project"$'\t'"FAIL"$'\t'"build-only"$'\t'"0"$'\t'"1"$'\t'"Missing build-only reason")
        failures+=("$project (missing build-only reason)")
        (( fail_count += 1 ))
      else
        echo "Build-only: $project — $REASON"
        rows+=("$project"$'\t'"BUILD_ONLY"$'\t'"build-only"$'\t'"0"$'\t'"0"$'\t'"$REASON")
        (( build_only_count += 1 ))
      fi
      continue
      ;;
    run)
      ;;
    *)
      rows+=("$project"$'\t'"FAIL"$'\t'"$CLASSIFICATION"$'\t'"0"$'\t'"1"$'\t'"Unknown disposition")
      failures+=("$project (unknown disposition '$CLASSIFICATION')")
      (( fail_count += 1 ))
      continue
      ;;
  esac

  case "$TIMEOUT_SECONDS" in
    ''|*[!0-9]*)
      rows+=("$project"$'\t'"FAIL"$'\t'"run"$'\t'"0"$'\t'"1"$'\t'"Invalid timeout")
      failures+=("$project (invalid timeout '$TIMEOUT_SECONDS')")
      (( fail_count += 1 ))
      continue
      ;;
  esac

  echo
  echo "Running $project (timeout ${TIMEOUT_SECONDS}s)"
  run_command=(dotnet run --no-build --project "$project_path" --configuration "$BUILD_CONFIG")
  if [[ "$SAMPLE_TOOLCHAIN" == "repository" ]]; then
    run_command+=("/property:RavenCompilerHost=$repository_compiler")
    if [[ -n "$repository_core" ]]; then
      run_command+=("/property:RavenCoreReferencePath=$repository_core")
    fi
    if [[ "$project" == *.rvnproj ]]; then
      run_command+=("/property:LanguageTargets=$repository_targets")
    fi
  fi

  start_ms="$(timestamp_ms)"
  if run_with_timeout "$TIMEOUT_SECONDS" "${run_command[@]}"; then
    actual_exit=0
  else
    actual_exit=$?
  fi
  end_ms="$(timestamp_ms)"
  wall_ms=$(( end_ms - start_ms ))

  if (( actual_exit == EXPECTED_EXIT )); then
    status="PASS"
    (( pass_count += 1 ))
  else
    status="FAIL"
    if (( actual_exit == 124 )); then
      reason="Timed out after ${TIMEOUT_SECONDS}s"
    else
      reason="Expected exit $EXPECTED_EXIT, got $actual_exit"
    fi
    failures+=("$project ($reason)")
    (( fail_count += 1 ))
  fi

  rows+=("$project"$'\t'"$status"$'\t'"run"$'\t'"$wall_ms"$'\t'"$actual_exit"$'\t'"${reason:-}")
  unset reason
done

{
  printf 'project\tstatus\tdisposition\twall_ms\twall_s\texit_code\treason\n'
  for row in "${rows[@]}"; do
    IFS=$'\t' read -r project status disposition wall_ms exit_code reason <<< "$row"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
      "$project" "$status" "$disposition" "$wall_ms" \
      "$(format_duration_ms "$wall_ms")" "$exit_code" "$reason"
  done
} > "$RUN_REPORT_TSV"

{
  echo "| Project | Status | Disposition | Wall time | Exit code | Reason |"
  echo "|---|---:|---:|---:|---:|---|"
  for row in "${rows[@]}"; do
    IFS=$'\t' read -r project status disposition wall_ms exit_code reason <<< "$row"
    printf '| %s | %s | %s | %ss | %s | %s |\n' \
      "$(escape_markdown_cell "$project")" \
      "$status" \
      "$disposition" \
      "$(format_duration_ms "$wall_ms")" \
      "$exit_code" \
      "$(escape_markdown_cell "$reason")"
  done
} > "$RUN_REPORT_MD"

echo
echo "Project sample run report:"
echo "  $RUN_REPORT_TSV"
echo "  $RUN_REPORT_MD"
echo "Passed: $pass_count"
echo "Build-only classifications: $build_only_count"
echo "Non-executable projects: $non_executable_count"
echo "Failed: $fail_count"

if (( fail_count > 0 )); then
  echo
  echo "Failed projects:"
  printf '  %s\n' "${failures[@]}"
  exit 1
fi
