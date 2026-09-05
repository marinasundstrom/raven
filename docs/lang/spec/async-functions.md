# Async functions

Async functions let a program wait for asynchronous work without blocking the
current thread. They are commonly used for operations such as network requests,
file access, database calls, and timers.

Declare an async function with the `async` modifier and use `await` to wait for
an asynchronous operation:

```raven
async func DownloadPage() {
    use client = HttpClient()

    let response = await client.GetAsync("https://example.com")
    let content = await response.Content.ReadAsStringAsync()

    Console.WriteLine(content)
}
```

While an awaited operation is incomplete, execution of the function may be
suspended. It resumes when the operation completes.

The `async` modifier can be used with top-level functions, methods, and local
functions.

## Returning values

An async function that doesn't return a value should declare
`System.Threading.Tasks.Task` as its return type:

```raven
async func Delay() -> Task {
    await Task.Delay(1000)
}
```

Use `Task<T>` when the function produces a value:

```raven
async func GetMessage() -> Task<string> {
    await Task.Delay(100)

    return "Hello"
}
```

As with other Raven functions, the final expression of a block can provide its
result:

```raven
async func GetMessage() -> Task<string> {
    await Task.Delay(100)

    "Hello"
}
```

Async functions can also use expression bodies:

```raven
async func GetMessage() -> Task<string> =>
    await GetMessageFromServer()
```

Raven does not infer `Task<T>` from the body of an async function. If an async
function returns a value, its task return type must be declared explicitly.

```raven
async func GetMessage() -> Task<string> {
    "Hello"
}
```

For `Task<T>`, returned values must be convertible to `T`.

A `Task` function does not return a value. Falling off the end of the function
is equivalent to `return`:

```raven
async func Save() -> Task {
    await SaveChanges()
}
```

An existing task is not returned directly from an `async Task` function.
Instead, await it:

```raven
async func Save() -> Task {
    await SaveChanges()
}
```

## Await expressions

Use `await` to suspend an async function until an asynchronous operation
completes:

```raven
async func LoadUser(id: int) -> Task<User> {
    let response = await client.GetAsync($"/users/{id}")
    let user = await response.Content.ReadFromJsonAsync<User>()

    user
}
```

The type of an `await` expression is the result produced by the awaited
operation. For example, awaiting `Task<string>` produces a `string`:

```raven
let text: string = await GetTextAsync()
```

Awaiting a `Task` that does not produce a value results in `unit`:

```raven
await Task.Delay(1000)
```

`await` can only be used in an async context.

## Top-level await

Top-level code can also use `await`:

```raven
import System.Net.Http.*

use client = HttpClient()

let response = await client.GetAsync("https://example.com")
let content = await response.Content.ReadAsStringAsync()

Console.WriteLine(content)
```

No explicit async entry-point function is required. Raven creates the necessary
program entry point and waits for the top-level asynchronous code to complete.

## Awaiting .NET types

Raven supports the standard .NET awaitable pattern, so `await` is not limited
to `Task` and `Task<T>`.

An awaitable value must provide a `GetAwaiter()` method. Its awaiter must provide:

* an `IsCompleted: bool` property
* a parameterless `GetResult()` method

Conceptually, an await expression:

```raven
let result = await operation
```

obtains the operation's awaiter. If the operation has already completed,
`GetResult()` can be evaluated immediately. Otherwise, the async function is
suspended and resumed when the operation completes.

The result type of `await` is the return type of `GetResult()`. If `GetResult()`
does not return a value, the expression has type `unit`.

This allows Raven to interoperate with .NET APIs that implement the awaitable
pattern without requiring their types to derive from or convert to `Task`.

## Async declarations

Explicit return types on async functions must use one of the supported task
shapes:

```raven
Task
Task<T>
ValueTask
ValueTask<T>
```

Other return types are rejected.

Async declarations may use either block bodies or expression bodies. Within an
`async Task<T>` function, each explicit return expression must be convertible to
`T`. Within an `async Task` function, `return` cannot carry an expression.

The same rules apply to async methods, top-level functions, and local
functions.

Property and indexer accessors may also be async. Async getters must expose a
task-shaped return type, while async setters may await asynchronous work before
storing a value.

## Unawaited calls

An ignored call returning `Task`, `Task<T>`, `ValueTask`, or `ValueTask<T>`
reports the default analyzer warning `RAV9038`, in both async and ordinary
functions. Await the operation to observe its completion and exceptions:

```raven
await Save()
```

Returning or storing the task handles its result without requiring an immediate
`await`. Use `_ = Save()` to mark an intentional discard. This warning is
independent of the returned-value handling mode and the disposable-value
analyzer. Configure `dotnet_diagnostic.RAV9038.severity` to change its severity.

## Exceptions

Exceptions raised during asynchronous execution are propagated through the
usual .NET async mechanism. Awaiting a failed operation rethrows its exception
at the `await` expression.

Exceptions that escape before asynchronous execution is suspended propagate
directly to the caller.

## Detailed rules and diagnostics

* `await` outside an async context reports `RAV2700`.
* Awaiting a value that does not follow the awaitable pattern reports
  `RAV2701`. An awaiter without `IsCompleted: bool` reports `RAV2702`, and one
  without a parameterless `GetResult()` reports `RAV2703`.
* An unsupported async return type reports `RAV2704`.
* Returning a value from an async function whose return type is `Task` or
  `ValueTask` reports `RAV2705`.
* An async body with no `await` reports warning `RAV2706` because it runs
  synchronously.
