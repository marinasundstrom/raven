# .NET nanoFramework Wi-Fi HTTP LED

This sample extends the Pico-family Blinky project with Wi-Fi and HTTP. It
connects a wireless Pico to a DHCP network, sends an HTTPS request to
`https://example.com`, and turns an external LED on when the response succeeds.
The external LED defaults to GP15 because the onboard LED on Pico W boards is
attached to the CYW43 wireless controller rather than an ordinary GPIO.

> [!WARNING]
> This sample deliberately sets `SslVerification.NoVerification` so it can
> demonstrate HTTPS transport without provisioning a root CA. The connection
> is encrypted, but the server's identity is not authenticated, making it
> vulnerable to man-in-the-middle attacks. Never use this setting for
> production traffic, credentials, tokens, or other sensitive data.

The LED also makes each stage observable without a debugger. It first blinks
three times to prove that `Main` started and GP15 works, stays off while the
network and request operation is running, turns on permanently only after a
successful response, or repeats one of these failure patterns:

- 1 blink: Wi-Fi association, DHCP, or network-interface failure
- 2 blinks: the server returned a non-success HTTP status
- 3 blinks: the HTTP, DNS, or TLS operation threw an exception

`ConnectAndGet` returns an exhaustive `NetworkRequestResult` union rather than
an integer status code. Every failure case carries the information valid for
that outcome: a DHCP/IP-address failure description, a setup exception
message, the HTTP status, or the request exception message. `Main` matches
those cases to the physical LED signal and debugger output.

`ScanAndConnectDhcp` scans for the supplied SSID and associates using the
credentials embedded by the build. When it returns `false`, the debugger
message includes the numeric value of `WifiNetworkHelper.Status`. The official
[nanoFramework status values](https://docs.nanoframework.net/api/nanoFramework.Networking.NetworkHelperStatus.html)
distinguish no configured interface (`3`), an address timeout (`4`), a
scan/association error (`6`), and an exception (`7`, with details in
`HelperException`). Raven reads the enum's underlying integer without runtime
reflection, which keeps this diagnostic available on nanoCLR.

Both the network phase and HTTP request have timeouts, so invalid credentials
no longer leave the sample apparently stuck forever. The successful path also
keeps `Main` alive; otherwise disposing the GPIO scope immediately after
writing `High` could make a successful request look like a failure.

The Wi-Fi credentials are required Raven `extern const` values. The project
contains nonfunctional placeholders so it can be evaluated without secrets;
`deploy.sh` always replaces them with prompted or environment-provided values
before creating the deployment image.

## Wi-Fi requirements

Pico W and Pico 2 W have single-band 2.4 GHz Wi-Fi. The SSID supplied to this
sample must therefore be available on 2.4 GHz; a 5 GHz-only SSID cannot be
used. A dual-band SSID is suitable when the access point also advertises it on
2.4 GHz.

This was verified on the Pico WH hardware setup: the application repeatedly
returned `NetworkUnavailable` on the original network, and debugger output
reported `WifiNetworkHelper` status `4` (`TokenExpiredWaitingIPAddress`) after
60 seconds. The same deployed application connected immediately when tested
against a 2.4 GHz network. Status `4` means that no valid IP address arrived
before the token expired; it can also indicate invalid credentials or a DHCP
problem, so verify those when the selected SSID already supports 2.4 GHz.

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

### Use the compiler from this checkout

An ordinary `dotnet build` resolves the `Raven.Sdk` version selected by the
repository's `global.json`. To test compiler changes that have not been
published in that SDK, add `--repo-compiler`:

```bash
RAVEN_WIFI_SSID='network-name' \
RAVEN_WIFI_PASSWORD='network-password' \
./build.sh --board pico-w --repo-compiler
```

This first rebuilds `src/Raven.Compiler` for `net10.0`, then passes the resulting
checkout-local `rvnc.dll` to the SDK through `RavenCompilerHost`. The build logs
print `Raven compiler host:` followed by the exact path used. The SDK still
provides the MSBuild targets; compilation and emission run in the freshly built
repo compiler.

`build.sh` uses the checkout-local compiler by default when that DLL already
exists, but `--repo-compiler` is the reproducible choice after changing compiler
source because it cannot accidentally reuse a stale build. Set
`RAVEN_COMPILER_DLL=/absolute/path/to/rvnc.dll` only when testing another
specific compiler build.

Outputs are written under `artifacts/<board>/` as a managed `.dll`, a compact
`NFMRK2` `.pe`, and a deployable `.bin` containing the application's complete
managed reference closure. `artifacts/<board>/debug/` contains the individual
compact assemblies used by the VS Code nanoFramework debugger.

## VS Code launch and attach

Open this sample directory itself as the VS Code workspace and install the
recommended official nanoFramework extension. The checked-in launch profiles
provide **Raven nanoFramework Wi-Fi: Launch Pico W** and **Raven nanoFramework
Wi-Fi: Attach Pico W**.

Launching prompts for the external LED GPIO, 2.4 GHz SSID, and password, then
runs `build.sh`, deploys the staged compact assemblies, and starts the managed
debugger. Password input is masked and the value is supplied to the build
through its environment rather than a command-line argument. Leave `device`
empty to let the extension select the connected nanoFramework device, or put a
serial port such as `/dev/tty.usbmodem11201` in `.vscode/launch.json`.

Attach connects to an application that is already deployed and uses the most
recent files under `artifacts/pico-w/debug/` for symbols; run the build task or
the launch profile first when those files do not exist or the source changed.
The credentials remain embedded in the generated files as described below.

## Deploy

Choose a wire-protocol serial port or Pico BOOTSEL/UF2 deployment. The script
asks for the SSID and password, hides password input, rebuilds the image with
those values, and remains a dry run unless `--execute` is supplied:

```bash
./deploy.sh --board pico-w --serial-port /dev/ttyACM0
./deploy.sh --board pico-w --serial-port /dev/ttyACM0 --execute
./deploy.sh --board pico-w --serial-port /dev/ttyACM0 --repo-compiler --execute
./deploy.sh --board pico2-w --uf2 --execute
```

Use the `--repo-compiler` form when validating changes in this Raven checkout.
It rebuilds the compiler before compiling credentials into the deployment
image, so the installed SDK's bundled compiler cannot be selected accidentally.

Serial deployment defaults to 1,500,000 baud, the rate validated with Pico WH
on macOS. Use `--baud <rate>` or `NANOFF_BAUD` to override it.

Before an executed wire-protocol deployment asks for credentials or builds the
image, the script checks the device firmware target. Pico W requires the
current `PICO_RP2040_W` target and Pico 2 W requires `PICO2_RP2350_W`. The
legacy `RP_PICO_W_RP2040` build is rejected: although it advertises the
`System.Device.Wifi` native assembly, `WifiAdapter.FindAllAdapters()` returns
an empty array and the Wi-Fi helper fails before association.

For automation, set `RAVEN_WIFI_SSID` and `RAVEN_WIFI_PASSWORD` instead of
answering prompts. Credentials are not printed by the scripts or included in
the deployment command, but they are compiled into the generated application
and written to local MSBuild intermediates under `obj/`. Both `obj/` and
`artifacts/` are ignored by Git. Remove those directories when the embedded
credentials should no longer remain on disk, and treat a deployed device as
containing the credentials.

The Raven source calls `WifiNetworkHelper` and `HttpClient` directly. Raven's
emitter preserves device-only method symbols as target metadata references, so
the nanoFramework assemblies do not need to be loadable in the desktop
compiler runtime. The build produces one application assembly and the
deployment image contains only that application plus its nanoFramework
reference closure.

The board must run a matching nanoFramework 2.0 preview nanoCLR image with the
Wi-Fi, networking, TLS, HTTP, and GPIO native contracts used by the pinned
managed packages. The managed package snapshot requires native checksums
`0x7AE2272F` for `System.Device.Wifi` and `0x0D0C3837` for `System.Net`. A
native-checksum mismatch causes nanoCLR to reject the affected managed
assembly before `Main` starts.
The current HTTP package places its assembly directly in its
`lib` directory, so the project includes an explicit reference until that
package advertises its `netnano1.0` asset normally. See
[Getting started with .NET nanoFramework](../../../docs/compiler/nanoframework.md)
for firmware and tooling guidance.

For the exact Pico 1 WH, external GP15 LED, and macOS USB procedure used during
hardware validation, see [Pico 1 WH over USB: build and deploy](PICO-1-WH-USB.md).

## HTTPS certificate verification

`HttpClient` normally validates the server certificate. This sample overrides
that secure default with `SslVerification.NoVerification` only to keep the
hardware walkthrough self-contained. A real application must remove that
assignment and provision the appropriate root CA in the device certificate
store or set `HttpClient.HttpsAuthentCert` for its controlled endpoint.
