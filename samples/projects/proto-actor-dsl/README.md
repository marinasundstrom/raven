# A Raven actor declaration for Proto.Actor

This proof of concept explores a Swift-like actor declaration in Raven, lowered
by a compiler macro to an ordinary [Proto.Actor](https://proto.actor/) class.
It is a language-design sketch, not a replacement actor runtime.

The interesting question is small: can Raven make the actor boundary visible
in source while preserving Proto.Actor's explicit messages, `PID`s, mailboxes,
and supervision model?

## Why actors?

Concurrent programs become difficult when several tasks can read and mutate the
same state. Locks can protect that state, but every caller must use them
correctly and lock ordering becomes another source of bugs.

An actor gives one owner to a piece of mutable state:

- Other code holds the actor's identity (`PID`), not a reference to its state.
- Callers send immutable messages to the actor's mailbox.
- The actor processes those messages sequentially, although many actors can run
  concurrently.
- A request is still a message; the response is sent back asynchronously.
- Actors form a hierarchy, so failures can be handled through supervision rather
  than being allowed to corrupt shared state.

The model does not make every race or distributed-system problem disappear.
Message ordering, mailbox growth, retries, idempotency, failure policy, and
serialization remain design decisions. What it does provide is a strong state
ownership boundary and a uniform way to communicate across threads or nodes.

Proto.Actor is a good target for this experiment because its core .NET contract
is deliberately small: implement `IActor.ReceiveAsync(IContext)`, construct
`Props`, spawn a `PID`, and exchange messages. Its documentation also recommends
immutable messages, which lines up naturally with Raven records and unions.

## The proposed Raven surface

The message protocol is an ordinary closed Raven union:

```raven
union CounterCommand {
    case Add(amount: int)
    case Subtract(amount: int)
    case Reset
    case Get
}

record CounterSnapshot(Value: int)
```

The actor itself is one declaration:

```raven
actor! CounterActor(command: CounterCommand, count: int = 0) {
    match command {
        .Add(let amount) => count = count + amount
        .Subtract(let amount) => count = count - amount
        .Reset => count = 0
        .Get => context.Respond(CounterSnapshot(count))
    }
}
```

The `!` is Raven's marker for a macro invocation. `actor! Name(...) { ... }` is
a declaration-form macro, so it occupies the same source position as a class.
It does not add `actor` to Raven's global grammar.

The declaration has three semantic parts:

1. `CounterActor` is the generated actor type and Proto.Actor producer.
2. The first parameter, `command: CounterCommand`, declares the mailbox protocol
   and names the current message inside the body.
3. Remaining defaulted parameters are actor-owned state. Here `count` becomes a
   private mutable property initialized to zero.

The body is ordinary Raven. Because `CounterCommand` is a closed union, `match`
must interpret the complete protocol. Adding another command therefore points
at actor behavior that has not yet been defined.

## What the macro unfolds to

The generated shape is equivalent to this Raven code (slightly formatted for
readability):

```raven
class CounterActor : Proto.IActor {
    private var count: int = 0

    static func Props() -> Proto.Props =>
        Proto.Props.FromProducer(func () => CounterActor())

    func ReceiveAsync(context: Proto.IContext) -> System.Threading.Tasks.Task {
        if let command: CounterCommand = context.Message {
            match command {
                .Add(let amount) => count = count + amount
                .Subtract(let amount) => count = count - amount
                .Reset => count = 0
                .Get => context.Respond(CounterSnapshot(count))
            }
        }

        return System.Threading.Tasks.Task.CompletedTask
    }
}
```

There is no second runtime or actor abstraction. The application spawns the
generated producer and uses the normal Proto.Actor root context:

```raven
let system = ActorSystem()
let counter = system.Root.SpawnNamed(CounterActor.Props(), "counter")

let add: CounterCommand = .Add(7)
let get: CounterCommand = .Get
system.Root.Send(counter, add)
let snapshot = system.Root
    .RequestAsync<CounterSnapshot>(counter, get)
    .GetAwaiter()
    .GetResult()
```

## What the DSL improves

Without changing Proto.Actor's runtime semantics, the declaration makes several
facts local and explicit:

- **Isolation is visible at the declaration.** Readers do not have to infer the
  actor boundary from an `IActor` base contract and a conventional method name.
- **Protocol and behavior stay connected.** The inbox type is declared once and
  interpreted with an exhaustive match.
- **State ownership is visible.** Defaulted parameters after the protocol are
  private state belonging to each actor instance.
- **Framework plumbing is generated.** `IActor`, `ReceiveAsync`, the safe message
  type test, `Task.CompletedTask`, and `Props.FromProducer` are implementation
  details.
- **The body remains normal Raven.** The macro reports it as an ordinary block
  fragment and maps it back into the generated handler for diagnostics,
  completion, and debugging.
- **Interop remains direct.** The generated type, `Props`, `PID`, `IContext`, and
  messages are normal .NET and Proto.Actor shapes.

This is deliberately closer to Swift's declaration-level actor ergonomics than
to a fluent wrapper API. A wrapper can shorten spawning and sending, but it
cannot make actor isolation part of the declaration's meaning.

## POC boundaries and design questions

The prototype intentionally proves one narrow path. Before treating this as a
real actor feature, the following need design work:

- The handler is synchronous and returns `Task.CompletedTask`. An async body
  needs a sound expansion and clear rules about actor reentrancy.
- Proto.Actor lifecycle messages are currently ignored by the generated message
  type test. A real DSL needs explicit lifecycle clauses without weakening the
  domain protocol.
- Invalid declaration shapes currently fall back to ordinary compiler errors.
  The macro should report dedicated diagnostics for a missing protocol or state
  without defaults.
- The first-parameter/remaining-state convention is concise but still a POC.
  Named roles such as `receives` and `state` may communicate intent better.
- Supervision, child spawning, behavior changes, timers, persistence,
  cancellation, serialization, and remote/cluster contracts are not modeled.
- A production design should decide whether `Props()` belongs on the generated
  type or whether producer configuration should remain entirely at the call
  site.
- The generated class is a backend detail today. The same declaration could be
  lowered by another backend, but portable behavior should not be claimed until
  lifecycle and reentrancy semantics are specified.

These are useful review questions for an actor-runtime expert: the goal is not
to hide Proto.Actor, but to find the smallest Raven surface that makes correct
actor code easier to read and author.

## Project layout

- `macros/ActorMacro.rvn` implements the declaration macro and source mapping.
- `macros/ProtoActorDslMacros.rvnproj` builds the Raven compiler plugin.
- `app/src/CounterProtocol.rvn` defines immutable commands and the response.
- `app/src/CounterActor.rvn` contains the single actor declaration.
- `app/src/Program.rvn` exercises normal Proto.Actor spawn, send, and request
  APIs.
- `app/ProtoActorDslSample.rvnproj` references Proto.Actor 1.8.0 and the macro
  plugin.

Run the POC from the repository root:

```bash
dotnet run --project samples/projects/proto-actor-dsl/app/ProtoActorDslSample.rvnproj \
  --property WarningLevel=0
```

Expected output:

```text
Counter value: 5
After reset: 0
```

The runtime package is the stable
[`Proto.Actor` 1.8.0](https://www.nuget.org/packages/Proto.Actor/1.8.0) release.
The basic runtime shapes used here follow Proto.Actor's official
[getting-started guide](https://proto.actor/protoactor/getting-started/) and
[message guidance](https://proto.actor/protoactor/messages/).
