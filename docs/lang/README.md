# Raven language guides

These guides explain how to write clear Raven programs and how to choose among
the language's application-level constructs. They are written for people
learning and using Raven, rather than as a normative definition of the
language.

The [language specification](spec/language-specification.md) is maintained as a
separate reference for exact syntax and semantic rules. You do not need to read
the specification in order to learn Raven or build an application.

## Start here

* [Introduction](../introduction.md)
* [Raven for absolute beginners](../raven-for-absolute-beginners.md)
* [Raven for C# developers](../raven-for-csharp-developers.md)
* [Authoring Raven macros](../macro-authoring.md)
* [Language philosophy](philosophy.md)
* [Domain modeling](domain-modeling.md)
* [Style guide](style-guide.md)

## Build something

Workload guides teach the language in an application context:

- [Build applications with Raven](../workloads/index.md)
- [Build a Web API with ASP.NET Core](../workloads/web-api.md)

## Current shape in one screen

```raven
import System.Console.*

func Main() -> () {
    let result = match ParsePort("8080") {
        Ok(let port) => "Listening on $port"
        Error(let err) => "Invalid port: $err"
    }

    WriteLine(result)
}

func ParsePort(text: string) -> Result<int, string> {
    return int.Parse(text) match {
        Ok(let value) => Ok(value)
        Error(_) => Error("not a number")
    }
}
```

Use the introduction for a guided overview and the application guides for
complete project shapes. Open the specification when you need to resolve an
exact language question.

Language proposals and compiler development notes are maintained separately in
the source repository. They are useful to contributors, but do not define the
user-facing language documentation.
