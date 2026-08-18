# Internet of Things (IoT) Native AOT sample

Application running on a Raspberry Pi with a Linux distribution.

Using System.Device.Gpio for accessing GPIO ports on the device.

## Publish self-contained app

Run this command:

```sh
dotnet publish --runtime linux-arm64 --self-contained
```

### Run the app

Set execution permissions on executable:

```sh
chmod +x IotNativeAot
```

Execute the app:

```sh
./IotNativeAot
```

## Publish AOT build

Easiest way is to run `./publish-aot.sh` script.

You need to choose the target platform and architecture. The script will default to the platform of the current machine.

To publish for Raspberry Pi (Linux ARM64):

```sh
./publish-aot.sh linux-arm64
```