# Error macro

This sample uses the standard `#[Error]` macro from `Raven.Macros` to derive
Raven's `System.IError` contract for a union:

```raven
#[Error]
union ParseError {
    case InvalidValue(value: string)
    case MissingValue
}
```

The macro keeps the union as an ordinary Raven/.NET type, adds `IError`, and
supplies default `Message` and `Cause` properties. An authored property with
either name takes precedence over the default.

Run the sample:

```bash
dotnet run --project ErrorMacro.rvnproj --property WarningLevel=0
```

Expected output:

```text
ParseError.InvalidValue("age")
```
