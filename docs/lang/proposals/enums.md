# Proposal: Enums

> ⚠️ 🧩 This proposal has been partly implemented

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

## Member references

```csharp
let x = DeviceType.Monitor;
let y : DeviceType = .Monitor;
```

Enum members can be imported into unqualified value scope by type wildcard
imports or by individual member imports:

```csharp
import DeviceType.*;
import DeviceType.Monitor;

let x = Monitor;
```

Individual enum-member imports follow the same precedence rules as other
specific imports.

## Value space

Raven enums follow C#/CLR value rules. The runtime value space is open: an enum
value may hold any value representable by its underlying integral type, even
when that value is not named by a declared member.

Raven may later allow enums to opt into closed/exhaustive semantics as an
explicit language feature, but ordinary enums remain CLR-open for interop.
