#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
NANOFF_COMMAND="${NANOFF_COMMAND:-nanoff}"
BOARD="pico-w"
SERIAL_PORT=""
LED_PIN=""
USE_UF2=0
EXECUTE=0

usage() {
  cat <<'EOF'
Usage: ./deploy.sh [options]

Options:
  --board pico-w|pico2-w
  --led-pin <gpio>                    External LED GPIO (default: 15).
  --serial-port <port>                Deploy through the nanoCLR wire protocol.
  --uf2                                Deploy through a Pico in BOOTSEL mode.
  --execute                            Perform deployment; otherwise print the command.

The script prompts for Wi-Fi credentials and compiles them into the image
before deployment. Set RAVEN_WIFI_SSID and RAVEN_WIFI_PASSWORD for a
non-interactive invocation.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --board)
      [[ $# -ge 2 ]] || { echo "Missing value for --board." >&2; exit 2; }
      BOARD="$2"
      shift 2
      ;;
    --led-pin)
      [[ $# -ge 2 ]] || { echo "Missing value for --led-pin." >&2; exit 2; }
      LED_PIN="$2"
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
  pico-w|pico2-w) ;;
  *)
    echo "Unsupported board profile '$BOARD'. This sample requires a wireless Pico." >&2
    exit 2
    ;;
esac

if [[ -n "$LED_PIN" && ! "$LED_PIN" =~ ^[0-9]+$ ]]; then
  echo "--led-pin must be a non-negative GPIO number." >&2
  exit 2
fi
if [[ "$USE_UF2" == "1" && -n "$SERIAL_PORT" ]]; then
  echo "Choose either --uf2 or --serial-port, not both." >&2
  exit 2
fi
if [[ "$USE_UF2" != "1" && -z "$SERIAL_PORT" ]]; then
  echo "Specify --uf2 or --serial-port <port>." >&2
  exit 2
fi

WIFI_SSID="${RAVEN_WIFI_SSID:-}"
WIFI_PASSWORD="${RAVEN_WIFI_PASSWORD:-}"

if [[ -z "$WIFI_SSID" ]]; then
  [[ -t 0 ]] || { echo "Set RAVEN_WIFI_SSID for a non-interactive deployment." >&2; exit 2; }
  read -r -p "Wi-Fi SSID: " WIFI_SSID
fi
if [[ -z "$WIFI_PASSWORD" ]]; then
  [[ -t 0 ]] || { echo "Set RAVEN_WIFI_PASSWORD for a non-interactive deployment." >&2; exit 2; }
  read -r -s -p "Wi-Fi password: " WIFI_PASSWORD
  printf '\n'
fi
if [[ -z "$WIFI_SSID" || -z "$WIFI_PASSWORD" ]]; then
  echo "Wi-Fi SSID and password must not be empty." >&2
  exit 2
fi

BUILD_ARGUMENTS=(--board "$BOARD")
if [[ -n "$LED_PIN" ]]; then
  BUILD_ARGUMENTS+=(--led-pin "$LED_PIN")
fi

RAVEN_WIFI_SSID="$WIFI_SSID" RAVEN_WIFI_PASSWORD="$WIFI_PASSWORD" \
  "$SAMPLE_DIR/build.sh" "${BUILD_ARGUMENTS[@]}"

DEPLOYMENT_IMAGE="$OUTPUT_DIR/$BOARD/NanoFrameworkWifiHttp.bin"
if [[ ! -f "$DEPLOYMENT_IMAGE" ]]; then
  echo "Deployment image '$DEPLOYMENT_IMAGE' was not produced." >&2
  exit 1
fi

if [[ "$USE_UF2" == "1" ]]; then
  COMMAND=("$NANOFF_COMMAND" --platform rpi_pico --deploy --image "$DEPLOYMENT_IMAGE" --uf2deploy)
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
