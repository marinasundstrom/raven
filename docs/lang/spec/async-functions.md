# Async functions

The `async` modifier may appear on top-level functions, methods, and local
functions. An async declaration opts the body into asynchronous control flow so
`await` expressions can suspend and resume execution. When no return type is
annotated, async declarations default to `System.Threading.Tasks.Task`
(`unit`-returning async body). The compiler does not infer `Task<T>` from
omitted return annotations.

Async functions with an explicit return type must annotate one of the supported
task shapes: `System.Threading.Tasks.Task` or `System.Threading.Tasks.Task<T>`.
Annotating any other type produces a diagnostic, and the compiler continues
analysis as though the return type were `Task`. This rule applies uniformly to
methods, top-level functions, and local functions declared inside other
bodies. Property and indexer accessors may also carry `async`; getters must
expose a task-shaped return type to remain valid, while setters may await
asynchronous work before storing values.

Diagnostic analyzers may still suggest adding an explicit return type annotation
based on observed body shape; such suggestions are advisory and do not change
the language binding rule above.

Async declarations support both block bodies and expression bodies. Every
`return` inside an async declaration completes the task produced by the method.
For `async Task<T>` block bodies, a trailing expression statement is treated as
an implicit return value and must convert to `T`.
For `async Task` members each `return` statement must omit the expression;
falling off the end of the body is equivalent to `return;`. For `async Task<T>`
members, return expressions must convert to `T`.

Returning an existing task instance such as `Task.CompletedTask` is not
permitted inside an `async Task` body. Authors must `await` the task to observe
its completion instead of returning it directly. Attempting to return an
expression from an `async Task` member produces a diagnostic that mirrors the
behavior of C# (error RAV2705). Exceptions that escape before the first `await`
propagate directly to the caller. Once asynchronous execution begins, `await`
unwrapped exceptions rethrow when the task is awaited, matching .NET's
observable behaviour.

## Await expressions

The `await` keyword introduces a unary expression with the grammar `await`
*expression*. Await expressions participate in the same precedence as other
prefix unary operators. Because `await` is reserved, the identifier form must be
escaped as `@await` when used outside this construct.

`await` may only appear inside an async function or lambda. File-scope
statements synthesize a synchronous `Program.Main` plus an async
`Program.MainAsync`, so top-level awaits run inside the async method while the
bridge awaits `MainAsync(args)` before returning to the host. The awaited
expression must expose an instance method `GetAwaiter()` whose return type
provides an accessible `IsCompleted: bool` property and a parameterless
`GetResult()` method. If the awaiter's `GetResult` produces no value, the await
expression's type is `unit`; otherwise it matches the `GetResult` return type.

Failing any of these requirements produces a compile-time diagnostic identifying
the missing member. The compiler also reports an error when `await` appears
outside an async context.

Evaluation first computes the operand value and calls `GetAwaiter()` to obtain
the awaiter. If `val IsCompleted` is `true`, `GetResult()` is invoked immediately
and the await expression yields its value. Otherwise execution is suspended and
later resumed when the awaiter signals completion; resumption continues after
the `await` with the result of `GetResult()`.
