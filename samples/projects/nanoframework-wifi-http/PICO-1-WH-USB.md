# Pico 1 WH over USB: Wi-Fi HTTP build and deploy

This is the exact workflow validated on macOS with a Raspberry Pi Pico 1 WH,
an external LED on GP15, and `nanoff` 2.5.162. The board is connected directly
through its USB debugger interface; no UART or network deployment transport is
involved.

The validated board reported the `RP_PICO_W_RP2040` target, nanoCLR `2.0.0.29`,
and the native Wi-Fi and networking assemblies required by this sample.
The corresponding checksums are `System.Device.Wifi` `0x7AE2272F` and
`System.Net` `0x0D0C3837`; the sample packages are pinned to that exact native
contract snapshot.

Run the commands below from `samples/projects/nanoframework-wifi-http`.

## Connect the hardware

Connect an external LED and suitable resistor to GP15 and ground. Connect the
Pico normally, without holding BOOTSEL. BOOTSEL mode is only needed when
installing or replacing nanoCLR firmware.

## Find and verify the USB port

List the current Pico USB serial device:

```bash
ls /dev/tty.usbmodem*
```

The validated device appeared as:

```text
/dev/tty.usbmodem11201
```

The suffix can change after reconnecting. Close any VS Code debug session or
serial monitor using the port, then verify the nanoCLR handshake explicitly at
1,500,000 baud:

```bash
nanoff --nanodevice \
  --serialport /dev/tty.usbmodem11201 \
  --baud 1500000 \
  --devicedetails
```

`nanoff` may first print `Can't check the version` and continue. The important
result is that `Getting details from nano device` succeeds and the command
prints the firmware, assemblies, and native assemblies.

If it instead reports `Couldn't connect to specified nano device`, unplug and
reconnect the Pico normally, list the port again, and repeat the command. A
macOS device node can remain present after a debugger or serial session has
left the USB endpoint unresponsive.

## Build and deploy with Wi-Fi credentials

The deployment wrapper asks for the SSID and password, hides the password,
rebuilds the application and networking bridge, and deploys the combined
compact image:

```bash
./deploy.sh \
  --board pico-w \
  --serial-port /dev/tty.usbmodem11201 \
  --baud 1500000 \
  --execute
```

The script defaults to 1,500,000 baud, so `--baud 1500000` may be omitted. It
is shown above to make the hardware-specific requirement explicit.

A successful deployment reports:

```text
Getting details from nano device...OK
Deploying managed application...OK
Rebooting...OK
```

After reboot, GP15 first blinks three times to prove that `Main` started before
any networking code runs. It then stays high while the operation is running,
stays high permanently when the request succeeds, or repeats a failure code:

- 1 blink: Wi-Fi, DHCP, or network-interface failure
- 2 blinks: the server returned a non-success HTTP status
- 3 blinks: HTTP, DNS, or TLS raised an exception

The application also writes the network-helper status, HTTP status, and safe
exception messages through `Debug.WriteLine`; it never writes the SSID or
password. To inspect this output without a managed debugger, close anything
using the device port, open a serial terminal at nanoFramework's diagnostic
rate, and then reset the board:

```bash
screen /dev/tty.usbmodem11201 921600
```

Use `Ctrl-A`, then `K`, to exit `screen`. Re-run the 1,500,000-baud
`--devicedetails` command before deploying again because only one process can
own the serial endpoint at a time.

> [!WARNING]
> The sample deliberately disables TLS certificate verification. HTTPS traffic
> is encrypted, but the Pico does not authenticate the server and is vulnerable
> to man-in-the-middle attacks. Use this only for the public demonstration
> request. A real application must restore verification and provision the root
> CA for its endpoint before sending credentials, tokens, or sensitive data.

## Direct `nanoff` fallback

If the image is already built with the correct credentials, deploy it directly:

```bash
nanoff --nanodevice \
  --serialport /dev/tty.usbmodem11201 \
  --baud 1500000 \
  --deploy \
  --image artifacts/pico-w/NanoFrameworkWifiHttp.bin
```

Do not use an older image built with placeholder or different credentials.
The credentials are compile-time constants embedded in the application image.

## Firmware boundary

Application deployment assumes compatible nanoCLR firmware is already running.
If `--devicedetails` does not report a Pico W target with the required
`System.Device.Wifi`, `System.Net`, GPIO, and core native assemblies, install a
matching nanoFramework 2.0 preview firmware before deploying the application.
Firmware installation through BOOTSEL is separate from normal application
deployment and is not required for every rebuild.

See [Getting started with .NET nanoFramework](../../../docs/compiler/nanoframework.md)
for firmware installation and BOOTSEL guidance.
