# MyServiceBus + RabbitMQ

This Raven console application exercises a third-party .NET messaging package
through Raven's ordinary interop surface. It configures MyServiceBus with
RabbitMQ, registers Raven consumers through dependency injection, publishes an
order command with a header, publishes a follow-up event from a consumer, and
performs a request/response exchange. It demonstrates both dedicated
`IConsumer<T>` classes and a Raven namespace-level function using the consumer
method request/response model available in MyServiceBus 0.1.0-preview.10.

Because this is a compiler-repository showcase, its project references
`Raven.Compiler` and explicitly uses that repository build instead of the
compiler bundled in the installed `Raven.Sdk` package.

The sample intentionally covers:

- restoring and referencing a NuGet package from an `.rvnproj`;
- generic extension methods and nested configuration lambdas;
- Raven classes implementing generic .NET interfaces;
- an attributed namespace-level request consumer that binds its message,
  consume context, and cancellation token, then returns `Task<TResponse>`;
- automatic assembly scanning that discovers the function together with the
  interface consumers;
- dependency-injection construction and service lookup;
- Raven records serialized as message contracts;
- asynchronous publish, consume, and request/response operations; and
- indexer assignment at a .NET API boundary for message headers.

## Start RabbitMQ

Docker must be installed and running. From this folder:

```bash
./rabbitmq.sh start
```

The helper creates only the `raven-myservicebus-rabbitmq` container and its
named data volume. It exposes AMQP on port `5672` and the management UI at
<http://localhost:15672> using `guest` / `guest`.

On first start, the helper also normalizes ownership inside its dedicated data
volume. This avoids host-dependent Docker volume ownership preventing Erlang
from reading RabbitMQ's cookie file.

Other commands are:

```bash
./rabbitmq.sh status
./rabbitmq.sh logs
./rabbitmq.sh stop
```

Stopping preserves the broker data volume.

## Build and run

From the repository root:

```bash
dotnet build samples/projects/myservicebus-rabbitmq/MyServiceBusRabbitMq.rvnproj --property WarningLevel=0
dotnet run --project samples/projects/myservicebus-rabbitmq/MyServiceBusRabbitMq.rvnproj --property WarningLevel=0
```

The app publishes one order, waits for the event consumer and request consumer,
then remains connected until Enter is pressed. RabbitMQ queues and exchanges can
be inspected in the management UI while the app is running.

Run `./rabbitmq.sh stop` when finished.
