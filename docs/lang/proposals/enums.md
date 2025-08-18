# Proposal: Enums

> ℹ️ This proposal has been partly implemented

```csharp
enum DeviceType {
    Harddrive
    Monitor,
    CPU
}
```

```csharp
let x = DeviceType.Harddrive;
let x : DeviceType = .Harddrive;
```

```csharp
let type = GetDeviceType(.CPU);

fun Foo(x : DeviceType) {
    // Omitted
}
```

## Member import

```csharp
import DeviceType.*;

let x = Monitor;
```