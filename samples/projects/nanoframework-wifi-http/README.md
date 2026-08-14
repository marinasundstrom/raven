# .NET nanoFramework Wi-Fi HTTP LED

This sample extends the Pico-family Blinky project with Wi-Fi and HTTP. It
connects a wireless Pico to a DHCP network, waits for a valid device clock for
TLS certificate validation, sends a request to
`https://example.com`, and turns an external LED on when the response succeeds.
The external LED defaults to GP15 because the onboard LED on Pico W boards is
attached to the CYW43 wireless controller rather than an ordinary GPIO.

The Wi-Fi credentials are required Raven `extern const` values. The project
contains nonfunctional placeholders so it can be evaluated without secrets;
`deploy.sh` always replaces them with prompted or environment-provided values
before creating the deployment image.

## Build

Install a .NET SDK, Python 3, and Mono, then build either wireless board profile:

```bash
./build.sh
./build.sh --board pico2-w
./build.sh --board pico-w --led-pin 14
```

Running `build.sh` without credential variables uses the placeholders and is
useful only for checking compilation and nanoFramework packaging. To build a
usable image without deploying it, provide credentials through the environment:

```bash
RAVEN_WIFI_SSID='network-name' \
RAVEN_WIFI_PASSWORD='network-password' \
./build.sh --board pico-w
```

Outputs are written under `artifacts/<board>/` as a managed `.dll`, a compact
`NFMRK2` `.pe`, and a deployable `.bin` containing the application's complete
managed reference closure.

## Deploy

Choose a wire-protocol serial port or Pico BOOTSEL/UF2 deployment. The script
asks for the SSID and password, hides password input, rebuilds the image with
those values, and remains a dry run unless `--execute` is supplied:

```bash
./deploy.sh --board pico-w --serial-port /dev/ttyACM0
./deploy.sh --board pico-w --serial-port /dev/ttyACM0 --execute
./deploy.sh --board pico2-w --uf2 --execute
```

For automation, set `RAVEN_WIFI_SSID` and `RAVEN_WIFI_PASSWORD` instead of
answering prompts. Credentials are not printed by the scripts or included in
the deployment command, but they are compiled into the generated application
and written to local MSBuild intermediates under `obj/`. Both `obj/` and
`artifacts/` are ignored by Git. Remove those directories when the embedded
credentials should no longer remain on disk, and treat a deployed device as
containing the credentials.

`NetworkHttpBridge.cs` is deliberately narrow: Raven's current Reflection.Emit
backend cannot safely materialize `WifiNetworkHelper`, whose device-only static
state is not loadable in the compiler's host runtime. The bridge is compiled
against the same nanoFramework references, converted to a compact `.pe`, and
included immediately before the Raven application in the deployment image.
The Raven source continues to own GPIO setup, credentials, and success control
flow. Repository-wide direct-compiler sample coverage excludes this one project
because its bridge is produced by `build.sh`.

The board must run a matching nanoFramework 2.0 preview nanoCLR image with the
Wi-Fi, networking, TLS, HTTP, and GPIO native contracts used by the pinned
managed packages. The current HTTP package places its assembly directly in its
`lib` directory, so the project includes an explicit reference until that
package advertises its `netnano1.0` asset normally. See
[Getting started with .NET nanoFramework](../../../docs/compiler/nanoframework.md)
for firmware and tooling guidance.
