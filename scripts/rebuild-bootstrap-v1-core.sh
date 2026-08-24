#!/usr/bin/env bash
# Reproduce a reviewable bootstrap-v1 Raven.Core candidate from a clean commit.
# This script never modifies the checked-in eng/bootstrap/v1 seed.

set -euo pipefail

REPOSITORY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTPUT_ROOT="$REPOSITORY_ROOT/artifacts/bootstrap"
COMPILER_VERSION=""
CORE_VERSION=""
BOOTSTRAP_TAG="unassigned"
SKIP_TESTS=0

usage() {
  cat <<'EOF'
Usage: scripts/rebuild-bootstrap-v1-core.sh <compiler-version> [options]

Build the full C# compiler without a Raven.Core dependency, then use that
compiler to build Raven.Core for net10.0 and net11.0. The result is written to
an ignored candidate directory and never overwrites eng/bootstrap/v1.

Options:
  --core-version <version>  Raven.Core package/file version (default: compiler version).
  --bootstrap-tag <tag>  Require the annotated bootstrap tag to resolve to HEAD.
  --output-root <path>   Candidate parent directory (default: artifacts/bootstrap).
  --skip-tests           Skip Raven.Core.Tests for an exploratory candidate.
  -h, --help             Show this help.
EOF
}

if [[ $# -eq 0 ]]; then
  usage >&2
  exit 2
fi

if [[ "$1" == "-h" || "$1" == "--help" ]]; then
  usage
  exit 0
fi

COMPILER_VERSION="$1"
CORE_VERSION="$COMPILER_VERSION"
shift

while [[ $# -gt 0 ]]; do
  case "$1" in
    --core-version)
      if [[ $# -lt 2 ]]; then
        echo "--core-version requires a value." >&2
        exit 2
      fi
      CORE_VERSION="$2"
      shift
      ;;
    --bootstrap-tag)
      if [[ $# -lt 2 ]]; then
        echo "--bootstrap-tag requires a value." >&2
        exit 2
      fi
      BOOTSTRAP_TAG="$2"
      shift
      ;;
    --output-root)
      if [[ $# -lt 2 ]]; then
        echo "--output-root requires a value." >&2
        exit 2
      fi
      OUTPUT_ROOT="$2"
      shift
      ;;
    --skip-tests)
      SKIP_TESTS=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

if [[ "$COMPILER_VERSION" == v* || ! "$COMPILER_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$ ]]; then
  echo "Compiler version must be SemVer without a leading v: $COMPILER_VERSION" >&2
  exit 2
fi

if [[ "$CORE_VERSION" == v* || ! "$CORE_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$ ]]; then
  echo "Raven.Core version must be SemVer without a leading v: $CORE_VERSION" >&2
  exit 2
fi

if [[ "$BOOTSTRAP_TAG" != "unassigned" && ! "$BOOTSTRAP_TAG" =~ ^bootstrap-v1([.][0-9]+)?$ ]]; then
  echo "Bootstrap tag must be bootstrap-v1 or a revision such as bootstrap-v1.1." >&2
  exit 2
fi

WORKTREE_STATE="$(git -C "$REPOSITORY_ROOT" status --porcelain --untracked-files=all)"
if [[ -n "$WORKTREE_STATE" ]]; then
  echo "Bootstrap-v1 Core reproduction requires a clean worktree:" >&2
  printf '%s\n' "$WORKTREE_STATE" >&2
  exit 1
fi

SOURCE_COMMIT="$(git -C "$REPOSITORY_ROOT" rev-parse HEAD)"
SHORT_COMMIT="$(git -C "$REPOSITORY_ROOT" rev-parse --short=12 HEAD)"

if [[ "$BOOTSTRAP_TAG" != "unassigned" ]]; then
  if ! TAG_COMMIT="$(git -C "$REPOSITORY_ROOT" rev-parse "refs/tags/$BOOTSTRAP_TAG^{commit}" 2>/dev/null)"; then
    echo "Bootstrap tag does not exist: $BOOTSTRAP_TAG" >&2
    exit 1
  fi

  if [[ "$TAG_COMMIT" != "$SOURCE_COMMIT" ]]; then
    echo "$BOOTSTRAP_TAG points to $TAG_COMMIT, but HEAD is $SOURCE_COMMIT." >&2
    exit 1
  fi
fi

DOTNET_SDK_VERSION="$(dotnet --version)"
DOTNET_SDK_MAJOR="${DOTNET_SDK_VERSION%%.*}"
if [[ ! "$DOTNET_SDK_MAJOR" =~ ^[0-9]+$ ]] || (( DOTNET_SDK_MAJOR < 11 )); then
  echo "Bootstrap-v1 Core reproduction requires .NET SDK 11 or newer; found $DOTNET_SDK_VERSION." >&2
  exit 1
fi

dotnet tool restore --tool-manifest "$REPOSITORY_ROOT/.config/dotnet-tools.json"
ILVERIFY_VERSION="$(dotnet tool run ilverify -- --version | tail -n 1)"

case "$OUTPUT_ROOT" in
  ""|/|"$REPOSITORY_ROOT"|"$HOME")
    echo "Refusing unsafe bootstrap output root: $OUTPUT_ROOT" >&2
    exit 2
    ;;
esac

mkdir -p "$OUTPUT_ROOT"
OUTPUT_ROOT="$(cd "$OUTPUT_ROOT" && pwd)"
CANDIDATE_NAME="bootstrap-v1-core-$CORE_VERSION-$SHORT_COMMIT"
CANDIDATE_DIR="$OUTPUT_ROOT/$CANDIDATE_NAME"

if [[ -e "$CANDIDATE_DIR" ]]; then
  echo "Candidate already exists; refusing to overwrite it: $CANDIDATE_DIR" >&2
  exit 1
fi

STAGING_DIR="$(mktemp -d "$OUTPUT_ROOT/.bootstrap-v1-core.XXXXXX")"
cleanup() {
  case "$STAGING_DIR" in
    "$OUTPUT_ROOT"/.bootstrap-v1-core.*)
      rm -rf "$STAGING_DIR"
      ;;
    *)
      echo "Refusing to remove unexpected staging directory: $STAGING_DIR" >&2
      ;;
  esac
}
trap cleanup EXIT

sha256_file() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

COMPILER_PROPERTIES=(
  "/property:WarningLevel=0"
  "/property:Version=$COMPILER_VERSION"
  "/property:InformationalVersion=$COMPILER_VERSION"
  "/property:IncludeSourceRevisionInInformationalVersion=false"
)
CORE_PROPERTIES=(
  "/property:WarningLevel=0"
  "/property:Version=$CORE_VERSION"
  "/property:InformationalVersion=$CORE_VERSION"
  "/property:IncludeSourceRevisionInInformationalVersion=false"
)
COMPILER_FRAMEWORK="net11.0"
COMPILER_HOST="$REPOSITORY_ROOT/src/Raven.Compiler/bin/Release/$COMPILER_FRAMEWORK/rvnc.dll"

echo "==> Generating compiler sources"
"$REPOSITORY_ROOT/scripts/generate-compiler-sources.sh"

echo "==> Building the bootstrap-v1 C# compiler"
dotnet build "$REPOSITORY_ROOT/src/Raven.Compiler/Raven.Compiler.csproj" \
  --configuration Release \
  --framework "$COMPILER_FRAMEWORK" \
  /property:UseRavenCoreReference=false \
  "${COMPILER_PROPERTIES[@]}"

for target_framework in net10.0 net11.0; do
  echo "==> Building Raven.Core for $target_framework with the v1 compiler host"
  dotnet build "$REPOSITORY_ROOT/src/Raven.Core/Raven.Core.rvnproj" \
    --configuration Release \
    --framework "$target_framework" \
    /property:RavenCompilerHost="$COMPILER_HOST" \
    /property:RavenBuildArgs=--ilverify \
    "${CORE_PROPERTIES[@]}"

  mkdir -p "$STAGING_DIR/$target_framework"
  cp "$REPOSITORY_ROOT/src/Raven.Core/bin/Release/$target_framework/Raven.Core.dll" \
    "$STAGING_DIR/$target_framework/Raven.Core.dll"

  if [[ -f "$REPOSITORY_ROOT/src/Raven.Core/bin/Release/$target_framework/Raven.Core.xml" ]]; then
    cp "$REPOSITORY_ROOT/src/Raven.Core/bin/Release/$target_framework/Raven.Core.xml" \
      "$STAGING_DIR/$target_framework/Raven.Core.xml"
  fi
done

if [[ "$SKIP_TESTS" == "0" ]]; then
  echo "==> Running Raven.Core behavior tests"
  dotnet test "$REPOSITORY_ROOT/test/Raven.Core.Tests/Raven.Core.Tests.csproj" \
    /property:RavenCompilerHost="$COMPILER_HOST" \
    /property:WarningLevel=0
fi

COMPILER_SHA256="$(sha256_file "$COMPILER_HOST")"
CORE_NET10_SHA256="$(sha256_file "$STAGING_DIR/net10.0/Raven.Core.dll")"
CORE_NET11_SHA256="$(sha256_file "$STAGING_DIR/net11.0/Raven.Core.dll")"
CORE_ASSEMBLY_VERSION="$(sed -n 's/.*<AssemblyVersion>\([^<]*\)<\/AssemblyVersion>.*/\1/p' "$REPOSITORY_ROOT/eng/Package.props")"

if [[ -z "$CORE_ASSEMBLY_VERSION" ]]; then
  echo "Could not determine Raven.Core CLR assembly version from eng/Package.props." >&2
  exit 1
fi

cat > "$STAGING_DIR/manifest.json" <<EOF
{
  "schemaVersion": 1,
  "bootstrapVersion": "v1",
  "bootstrapTag": "$BOOTSTRAP_TAG",
  "compilerVersion": "$COMPILER_VERSION",
  "corePackageVersion": "$CORE_VERSION",
  "coreAssemblyVersion": "$CORE_ASSEMBLY_VERSION",
  "sourceCommit": "$SOURCE_COMMIT",
  "dotnetSdkVersion": "$DOTNET_SDK_VERSION",
  "compilerFramework": "$COMPILER_FRAMEWORK",
  "compilerHostSha256": "$COMPILER_SHA256",
  "ilVerification": {
    "tool": "dotnet-ilverify",
    "version": "$ILVERIFY_VERSION",
    "result": "passed"
  },
  "core": [
    {
      "targetFramework": "net10.0",
      "path": "net10.0/Raven.Core.dll",
      "sha256": "$CORE_NET10_SHA256"
    },
    {
      "targetFramework": "net11.0",
      "path": "net11.0/Raven.Core.dll",
      "sha256": "$CORE_NET11_SHA256"
    }
  ]
}
EOF

(
  cd "$STAGING_DIR"
  printf '%s  %s\n' "$CORE_NET10_SHA256" "net10.0/Raven.Core.dll" > SHA256SUMS
  printf '%s  %s\n' "$CORE_NET11_SHA256" "net11.0/Raven.Core.dll" >> SHA256SUMS
)

mv "$STAGING_DIR" "$CANDIDATE_DIR"
trap - EXIT

echo
echo "Bootstrap-v1 Core candidate created:"
echo "  $CANDIDATE_DIR"
echo
echo "This candidate has not modified eng/bootstrap/v1. Review its manifest,"
echo "binary contracts, tests, and hashes before any explicit seed update."
