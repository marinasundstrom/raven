#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERIAL_PORT="${1:-}"
BAUD_RATE="${NANOFF_BAUD:-1500000}"
NANOFF_COMMAND="${NANOFF_COMMAND:-nanoff}"

if [[ -z "$SERIAL_PORT" ]]; then
  echo "Usage: ./deploy.sh /dev/tty.usbmodemXXXX" >&2
  exit 2
fi

"$SAMPLE_DIR/build.sh"

"$NANOFF_COMMAND" \
  --nanodevice \
  --deploy \
  --serialport "$SERIAL_PORT" \
  --baud "$BAUD_RATE" \
  --image "$SAMPLE_DIR/artifacts/NanoFrameworkDht22Display.bin"
