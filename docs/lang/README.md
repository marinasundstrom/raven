# Raven language guides

These guides explain how to write clear Raven programs and how to choose among
the language's application-level constructs. They are written for people
learning and using Raven: examples lead, essential behavior is explained in
context, and compiler-internal detail stays out of the learning path.

## Start here

* [Introduction](../introduction.md)
* [Language features](features/index.md)
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

Use the introduction for a guided overview, the feature guides for focused
concepts, and the application guides for complete project shapes.

Language proposals and compiler development notes are maintained separately in
the source repository. They are useful to contributors, but do not define the
user-facing language documentation.
