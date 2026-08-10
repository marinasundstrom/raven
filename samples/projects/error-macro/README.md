# Error macro

This sample uses the standard `#[Error]` macro from `Raven.Macros` to derive
Raven's `System.IError` contract for a union:

```raven
#[Error]
union ParseError {
    #[ErrorMessage("Invalid value: $value")]
    case InvalidValue(value: string)

    #[ErrorMessage("A required value is missing")]
    case MissingValue
}
```

The macro keeps the union as an ordinary Raven/.NET type, adds `IError`, and
supplies default `Message` and `Cause` properties. `ErrorMessage` customizes a
case using ordinary Raven string interpolation, so payload names such as
`value` are available without a separate formatting language. An authored
property named `Message` or `Cause` takes precedence over the generated member.

Conceptually, the two macros expand the declaration to:

```raven
union ParseError: IError {
    case InvalidValue(value: string)
    case MissingValue

    val Message: string => self match {
        InvalidValue(let value) => "Invalid value: $value"
        MissingValue => "A required value is missing"
        _ => self.ToString()
    }

    val Cause: IError? => null
}
```

The catch-all preserves the normal case-aware message for any case that does
not carry `ErrorMessage`.

The executable sample returns `ParseError` through `Result<int, ParseError>`.
At the calling boundary it uses `WithContext`, which maps the error channel to
`ContextError<ParseError>`:

```raven
let result = parseAge("age").WithContext("Reading the configured age")

if result is Error(let error) {
    Console.WriteLine(error.Message)
    Console.WriteLine(error.Cause.Message)
}
```

The contextual message describes the current operation. `Cause` remains the
explicit, typed `ParseError`; no exception is thrown and the original error is
not erased.

Run the sample:

```bash
dotnet run --project ErrorMacro.rvnproj --property WarningLevel=0
```

Expected output:

```text
Reading the configured age
Invalid value: age
```
