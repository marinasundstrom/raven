# Raven language features

This section explains the Raven features you will use in everyday programs.
Each guide starts with the problem a feature solves, shows its usual form, and
points out the choice the feature communicates in your code.

You do not need to learn every feature before building something. Start with
values and functions, then follow the topic that matches the code you want to
write.

## Start with the essentials

- [Values, functions, and control flow](values-and-functions.md) introduces
  `let`, `var`, functions, expressions, and loops.
- [Model data with records, unions, and classes](data-modeling.md) explains how
  to choose a type from the meaning of the data.
- [Make decisions with patterns](patterns.md) shows how `match` interprets
  closed states and extracts their data.
- [Handle absence and failure](option-and-result.md) introduces `Option`,
  `Result`, and `?` propagation.

## Work with the .NET platform

- [Use .NET libraries](dotnet-interop.md) covers imports, framework types,
  packages, and Raven/.NET boundaries.
- [Write asynchronous code](async.md) covers `async`, `await`, tasks, async
  streams, and cancellation.
- [Transform code with macros](macros.md) introduces explicit compile-time
  transformations and when to choose them.

These guides intentionally describe the useful center of each feature rather
than every compiler rule or edge case. Follow a topic into the [language
reference](../spec/index.md) when you need its complete syntax and behavior.
Complete applications are available in the [workload
guides](../../workloads/index.md).
