## Raven programming language

This section covers Raven as it exists today: a .NET language with an expression-oriented style, explicit mutability, pattern matching, `Option`/`Result`-based flow, records and primary constructors, extensions/traits, and direct .NET interop.

### Start here

* [Introduction](../introduction.md)
* [Language philosophy](philosophy.md)
* [Language specification](spec/language-specification.md)
* [Type system](spec/type-system.md)
* [Style guide](style-guide.md)
* [Proposals](proposals)

### Current shape in one screen

```raven
import System.Console.*

func Main() -> () {
    val result = ParsePort("8080") match {
        Ok(val port) => "Listening on $port"
        Error(val err) => "Invalid port: $err"
    }

    WriteLine(result)
}

func ParsePort(text: string) -> Result<int, string> {
    return try int.Parse(text) match {
        Ok(val value) => Ok(value)
        Error(_) => Error("not a number")
    }
}
```

Use the introduction for a guided overview and the specification for precise language rules.
