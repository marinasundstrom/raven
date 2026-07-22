#!/usr/bin/env sh
set -eu

VERSION="${1:-}"
INSTALL_ROOT="${RAVEN_INSTALL_ROOT:-$HOME/.raven}"
REPOSITORY="${RAVEN_GITHUB_REPOSITORY:-marinasundstrom/raven}"

if [ -z "$VERSION" ]; then
  echo "Usage: install-raven.sh <version>" >&2
  exit 1
fi

VERSION="${VERSION#v}"

case "$(uname -s)" in
  Darwin) OS="osx" ;;
  Linux) OS="linux" ;;
  *) echo "Unsupported operating system: $(uname -s)" >&2; exit 1 ;;
esac

case "$(uname -m)" in
  x86_64|amd64) ARCH="x64" ;;
  arm64|aarch64) ARCH="arm64" ;;
  *) echo "Unsupported architecture: $(uname -m)" >&2; exit 1 ;;
esac

RID="$OS-$ARCH"
ASSET="raven-sdk-$VERSION-$RID.tar.gz"
BASE_URL="${RAVEN_RELEASE_BASE_URL:-https://github.com/$REPOSITORY/releases/download/v$VERSION}"
TEMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TEMP_DIR"' EXIT INT TERM

curl --fail --location --silent --show-error "$BASE_URL/$ASSET" --output "$TEMP_DIR/$ASSET"
curl --fail --location --silent --show-error "$BASE_URL/SHA256SUMS" --output "$TEMP_DIR/SHA256SUMS"

EXPECTED="$(awk -v asset="$ASSET" '$2 == asset || $2 == "*" asset { print $1 }' "$TEMP_DIR/SHA256SUMS")"
if [ -z "$EXPECTED" ]; then
  echo "No checksum was published for $ASSET." >&2
  exit 1
fi

ACTUAL="$(shasum -a 256 "$TEMP_DIR/$ASSET" | awk '{ print $1 }')"
if [ "$ACTUAL" != "$EXPECTED" ]; then
  echo "Checksum verification failed for $ASSET." >&2
  exit 1
fi

tar -xzf "$TEMP_DIR/$ASSET" -C "$TEMP_DIR"
SOURCE_DIR="$TEMP_DIR/raven-sdk-$VERSION-$RID"
DESTINATION="$INSTALL_ROOT/sdk/$VERSION"

if [ ! -d "$SOURCE_DIR" ]; then
  echo "The archive does not contain the expected SDK directory." >&2
  exit 1
fi

mkdir -p "$INSTALL_ROOT/sdk" "$INSTALL_ROOT/bin"
rm -rf "$DESTINATION"
mv "$SOURCE_DIR" "$DESTINATION"

write_launcher() {
  command_name="$1"
  assembly_path="$2"
  launcher_path="$INSTALL_ROOT/bin/$command_name"
  printf '#!/usr/bin/env sh\nexec dotnet "%s" "$@"\n' "$assembly_path" > "$launcher_path"
  chmod +x "$launcher_path"
}

write_launcher "rvn" "$DESTINATION/tools/rvn/rvn.dll"
write_launcher "rvnc" "$DESTINATION/tools/rvnc/rvnc.dll"
write_launcher "raven-language-server" "$DESTINATION/tools/language-server/Raven.LanguageServer.dll"

echo "Installed Raven $VERSION to $DESTINATION"
echo "Add $INSTALL_ROOT/bin to PATH."
