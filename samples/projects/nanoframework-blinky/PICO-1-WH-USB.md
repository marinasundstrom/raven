# Pico 1 WH over USB: build and deploy

This is the exact workflow validated on macOS with a Raspberry Pi Pico 1 WH,
an external LED on GP15, `PICO_RP2040_W` nanoCLR firmware, and `nanoff`
2.5.162. The board is connected directly over its USB debugger interface; no
UART or network transport is involved.

The older `RP_PICO_W_RP2040` target is sufficient for GPIO-only Blinky but is
not a valid firmware baseline for Wi-Fi samples because it can expose zero
wireless adapters. Use the current target so follow-up samples exercise the
same supported firmware.

Run all commands from the Raven repository root.

## Build for GP15

```bash
dotnet build \
  samples/projects/nanoframework-blinky/NanoFrameworkBlinky.rvnproj \
  --configuration Debug \
  --property:RavenLedPin=15 \
  --property:WarningLevel=0
```

The deployable application image is written to:

```text
samples/projects/nanoframework-blinky/bin/Debug/netnano1.0/NanoFrameworkBlinky.bin
```

Keep the `.pe`, `.pdbx`, and `.pdb` files in the same directory. They are used
by the nanoFramework debugger and must come from the same build.

## Find the USB port

Connect the Pico normally, without holding BOOTSEL, then run:

```bash
ls /dev/tty.usbmodem*
```

The validated device appeared as:

```text
/dev/tty.usbmodem11201
```

The suffix can change after reconnecting. Use the path reported on the current
machine. `nanoff --listdevices` may say `No devices found` on macOS even while
the explicit USB port works.

## Deploy over USB

Close any VS Code debug session or serial monitor using the port. Substitute
the current port in this command:

```bash
nanoff --nanodevice \
  --serialport /dev/tty.usbmodem11201 \
  --baud 1500000 \
  --deploy \
  --image samples/projects/nanoframework-blinky/bin/Debug/netnano1.0/NanoFrameworkBlinky.bin
```

A successful deployment reports:

```text
Getting details from nano device...OK
Deploying managed application...OK
Rebooting...OK
```

After reboot, the external LED on GP15 should alternate every 500 milliseconds.
Rebuild and repeat this deployment command after changing the Raven program.
The nanoCLR firmware does not need to be reflashed for each application build.

If a debugger has just detached and `nanoff` reports that it cannot connect to
the specified device even though `/dev/tty.usbmodem...` still exists, unplug
and reconnect the Pico normally, check the current port suffix again, and rerun
the deployment command. The macOS device node can outlive a non-responsive USB
debug session.

## Debugger status for this setup

Raven's emitted symbols were validated end to end on this device. A breakpoint
on `Program.rvn` line 13 resolved to nanoCLR IL offset 23, was accepted by the
device, stopped execution, and produced a stack frame at `Program.rvn:13`.

The released .NET nanoFramework VS Code extension 1.0.247 currently initiates
debug connections at 921600 baud and does not expose a debug baud setting in
`launch.json`. This Pico did not complete the debugger handshake at that rate;
the same bridge connected and debugged successfully at 1500000 baud. Therefore
the normal build and direct deployment commands above are validated, while F5
debugging on this exact USB setup depends on the extension allowing the working
baud rate to be selected. This is an adapter transport limitation, not a Raven
source-symbol mapping failure.
