#!/usr/bin/env bash
# IL-verify and run a reviewed set of compiler-shaped samples in Release mode.

set -Euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST="${SAMPLE_IL_MANIFEST:-$ROOT_DIR/samples/ilverify-manifest.tsv}"
OUTPUT_DIR="${SAMPLE_IL_OUTPUT_DIR:-$ROOT_DIR/artifacts/validation/sample-il}"
COMPILER="$ROOT_DIR/src/Raven.Compiler/bin/Release/net11.0/rvnc.dll"
LANGUAGE_TARGETS="$ROOT_DIR/build/Raven.Language.targets"
REPORT_TSV="$OUTPUT_DIR/report.tsv"
REPORT_MD="$OUTPUT_DIR/report.md"

if [[ ! -f "$MANIFEST" ]]; then
  echo "Sample IL manifest not found: $MANIFEST" >&2
  exit 1
fi

mkdir -p "$OUTPUT_DIR/logs" "$OUTPUT_DIR/standalone"

dotnet tool restore --tool-manifest "$ROOT_DIR/.config/dotnet-tools.json"

echo "Building the Release compiler host"
dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" \
  --configuration Release \
  --framework net11.0 \
  /property:UseRavenCoreReference=false \
  /property:ProduceReferenceAssembly=true \
  /property:WarningLevel=0

for target_framework in net10.0 net11.0; do
  echo
  echo "Building and verifying Raven.Core for $target_framework"
  dotnet build "$ROOT_DIR/src/Raven.Core/Raven.Core.rvnproj" \
    --configuration Release \
    --framework "$target_framework" \
    --no-incremental \
    "/property:RavenCompilerHost=$COMPILER" \
    /property:RavenBuildArgs=--ilverify \
    /property:WarningLevel=0
done

echo
echo "Building and verifying Raven.Macros for net10.0"
dotnet build "$ROOT_DIR/src/Raven.Macros/Raven.Macros.rvnproj" \
  --configuration Release \
  --framework net10.0 \
  --no-incremental \
  "/property:RavenCompilerHost=$COMPILER" \
  /property:RavenBuildArgs=--ilverify \
  /property:WarningLevel=0

rows=()
failures=()
pass_count=0
fail_count=0

while IFS=$'\t' read -r kind target_framework relative_path assembly_name coverage || [[ -n "${kind:-}" ]]; do
  [[ -z "${kind:-}" || "$kind" == \#* ]] && continue

  input_path="$ROOT_DIR/$relative_path"
  slug="${assembly_name//[^A-Za-z0-9_.-]/-}-$target_framework"
  log_path="$OUTPUT_DIR/logs/$slug.log"
  assembly_path=""
  verify_status="FAIL"
  runtime_status="NOT_RUN"

  echo
  echo "Verifying $relative_path ($target_framework)"

  case "$kind" in
    standalone)
      assembly_directory="$OUTPUT_DIR/standalone/$slug"
      mkdir -p "$assembly_directory"
      assembly_path="$assembly_directory/$assembly_name.dll"
      raven_core="$ROOT_DIR/src/Raven.Core/bin/Release/$target_framework/Raven.Core.dll"

      if dotnet "$COMPILER" "$input_path" \
        --configuration Release \
        --framework "$target_framework" \
        --raven-core "$raven_core" \
        --ilverify \
        --output "$assembly_path" 2>&1 | tee "$log_path"; then
        verify_status="PASS"
        cp "$raven_core" "$assembly_directory/Raven.Core.dll"
      fi
      ;;
    project)
      project_directory="$(dirname "$input_path")"
      assembly_path="$project_directory/bin/Release/$target_framework/$assembly_name.dll"

      if dotnet build "$input_path" \
        --configuration Release \
        --no-incremental \
        /property:WarningLevel=0 \
        "/property:RavenCompilerHost=$COMPILER" \
        "/property:LanguageTargets=$LANGUAGE_TARGETS" \
        /property:RavenBuildArgs=--ilverify 2>&1 | tee "$log_path"; then
        verify_status="PASS"
      fi
      ;;
    *)
      echo "Unknown sample IL manifest kind '$kind' for '$relative_path'." | tee "$log_path"
      ;;
  esac

  if [[ "$verify_status" == "PASS" && -f "$assembly_path" ]]; then
    if dotnet "$assembly_path" >>"$log_path" 2>&1; then
      runtime_status="PASS"
    else
      runtime_status="FAIL"
    fi
  elif [[ "$verify_status" == "PASS" ]]; then
    verify_status="FAIL"
    echo "Expected verified assembly was not produced: $assembly_path" >>"$log_path"
  fi

  if [[ "$verify_status" == "PASS" && "$runtime_status" == "PASS" ]]; then
    status="PASS"
    ((pass_count += 1))
  else
    status="FAIL"
    failures+=("$relative_path")
    ((fail_count += 1))
  fi

  rows+=("$kind"$'\t'"$target_framework"$'\t'"$relative_path"$'\t'"$assembly_path"$'\t'"$coverage"$'\t'"$verify_status"$'\t'"$runtime_status"$'\t'"$status")
done < "$MANIFEST"

{
  printf 'kind\ttarget_framework\tsource\tassembly\tcoverage\tilverify\truntime\tstatus\n'
  printf '%s\n' "${rows[@]}"
} > "$REPORT_TSV"

{
  echo "# Representative sample IL report"
  echo
  echo "| Kind | Target | Source | Coverage | ILVerify | Runtime | Status |"
  echo "|---|---|---|---|---:|---:|---:|"
  for row in "${rows[@]}"; do
    IFS=$'\t' read -r kind target_framework relative_path assembly_path coverage verify_status runtime_status status <<< "$row"
    printf '| %s | %s | `%s` | %s | %s | %s | %s |\n' \
      "$kind" "$target_framework" "$relative_path" "$coverage" "$verify_status" "$runtime_status" "$status"
  done
} > "$REPORT_MD"

echo
echo "Representative sample IL report:"
echo "  $REPORT_TSV"
echo "  $REPORT_MD"
echo "Passed: $pass_count"
echo "Failed: $fail_count"

if (( fail_count > 0 )); then
  echo
  echo "Failed samples:"
  printf '  %s\n' "${failures[@]}"
  exit 1
fi

echo "Representative sample IL gate passed."
