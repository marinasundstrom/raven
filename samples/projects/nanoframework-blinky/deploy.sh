#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
NANOFF_COMMAND="${NANOFF_COMMAND:-nanoff}"
BOARD="pico2"
SERIAL_PORT=""
USE_UF2=0
EXECUTE=0

usage() {
  cat <<'EOF'
Usage: ./deploy.sh [options]

Options:
  --board pico|pico-w|pico2|pico2-w
  --serial-port <port>                 Deploy through the nanoCLR wire protocol.
  --uf2                                Deploy through a Pico in BOOTSEL mode.
  --execute                            Perform deployment; otherwise print the command.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --board)
      [[ $# -ge 2 ]] || { echo "Missing value for --board." >&2; exit 2; }
      BOARD="$2"
      shift 2
      ;;
    --serial-port)
      [[ $# -ge 2 ]] || { echo "Missing value for --serial-port." >&2; exit 2; }
      SERIAL_PORT="$2"
      shift 2
      ;;
    --uf2)
      USE_UF2=1
      shift
      ;;
    --execute)
      EXECUTE=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option '$1'." >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$BOARD" in
  pico) TARGET="PICO_RP2040" ;;
  pico-w) TARGET="PICO_RP2040_W" ;;
  pico2) TARGET="PICO2_RP2350" ;;
  pico2-w) TARGET="PICO2_RP2350_W" ;;
  *)
    echo "Unsupported board profile '$BOARD'." >&2
    exit 2
    ;;
esac

if [[ "$USE_UF2" == "1" && -n "$SERIAL_PORT" ]]; then
  echo "Choose either --uf2 or --serial-port, not both." >&2
  exit 2
fi
if [[ "$USE_UF2" != "1" && -z "$SERIAL_PORT" ]]; then
  echo "Specify --uf2 or --serial-port <port>." >&2
  exit 2
fi

DEPLOYMENT_IMAGE="$OUTPUT_DIR/$BOARD/NanoFrameworkBlinky.bin"
if [[ ! -f "$DEPLOYMENT_IMAGE" ]]; then
  echo "Deployment image '$DEPLOYMENT_IMAGE' was not found; run build.sh for '$BOARD' first." >&2
  exit 1
fi

if [[ "$USE_UF2" == "1" ]]; then
  COMMAND=("$NANOFF_COMMAND" --platform rpi_pico --target "$TARGET" --deploy --image "$DEPLOYMENT_IMAGE" --uf2deploy)
else
  COMMAND=("$NANOFF_COMMAND" --nanodevice --deploy --serialport "$SERIAL_PORT" --image "$DEPLOYMENT_IMAGE")
fi

printf 'Deployment command:'
printf ' %q' "${COMMAND[@]}"
printf '\n'

if [[ "$EXECUTE" != "1" ]]; then
  echo "Dry run only. Add --execute to deploy."
  exit 0
fi

if ! command -v "$NANOFF_COMMAND" >/dev/null 2>&1; then
  echo "Required command '$NANOFF_COMMAND' was not found." >&2
  exit 1
fi

"${COMMAND[@]}"
