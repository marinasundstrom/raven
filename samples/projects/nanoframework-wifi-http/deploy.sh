#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
NANOFF_COMMAND="${NANOFF_COMMAND:-nanoff}"
BAUD_RATE="${NANOFF_BAUD:-1500000}"
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
  --baud <rate>                       Wire-protocol baud rate (default: 1500000).
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
    --baud)
      [[ $# -ge 2 ]] || { echo "Missing value for --baud." >&2; exit 2; }
      BAUD_RATE="$2"
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
if [[ ! "$BAUD_RATE" =~ ^[1-9][0-9]*$ ]]; then
  echo "--baud must be a positive integer." >&2
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

if [[ "$EXECUTE" == "1" ]] && ! command -v "$NANOFF_COMMAND" >/dev/null 2>&1; then
  echo "Required command '$NANOFF_COMMAND' was not found." >&2
  exit 1
fi

if [[ "$EXECUTE" == "1" && "$USE_UF2" != "1" ]]; then
  echo "Checking nanoCLR firmware on $SERIAL_PORT..."
  if ! DEVICE_DETAILS="$($NANOFF_COMMAND --nanodevice \
      --serialport "$SERIAL_PORT" \
      --baud "$BAUD_RATE" \
      --devicedetails 2>&1)"; then
    printf '%s\n' "$DEVICE_DETAILS" >&2
    echo "Could not inspect the nanoFramework device before deployment." >&2
    exit 1
  fi

  DEVICE_TARGET="$(printf '%s\n' "$DEVICE_DETAILS" | sed -n 's/^[[:space:]]*Target:[[:space:]]*//p' | head -n 1)"
  case "$BOARD" in
    pico-w) EXPECTED_TARGET="PICO_RP2040_W" ;;
    pico2-w) EXPECTED_TARGET="PICO2_RP2350_W" ;;
  esac

  if [[ "$DEVICE_TARGET" == "RP_PICO_W_RP2040" ]]; then
    echo "Unsupported legacy Pico W firmware target '$DEVICE_TARGET'." >&2
    echo "This firmware exposes System.Device.Wifi but returns no Wi-Fi adapters." >&2
    echo "Flash current PICO_RP2040_W preview firmware in BOOTSEL mode, then retry." >&2
    exit 1
  fi
  if [[ "$DEVICE_TARGET" != "$EXPECTED_TARGET" ]]; then
    echo "Board profile '$BOARD' requires firmware target '$EXPECTED_TARGET', but the device reports '${DEVICE_TARGET:-unknown}'." >&2
    exit 1
  fi

  echo "Firmware target: $DEVICE_TARGET"
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
  COMMAND=("$NANOFF_COMMAND" --nanodevice --deploy --serialport "$SERIAL_PORT" --baud "$BAUD_RATE" --image "$DEPLOYMENT_IMAGE")
fi

printf 'Deployment command:'
printf ' %q' "${COMMAND[@]}"
printf '\n'

if [[ "$EXECUTE" != "1" ]]; then
  echo "Dry run only. Add --execute to deploy."
  exit 0
fi

"${COMMAND[@]}"
