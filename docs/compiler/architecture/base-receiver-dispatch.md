# Explicit base receiver dispatch

An explicit `base` receiver selects the base implementation for methods, property
accessors and indexer accessors. Ordinary reference receivers retain virtual
dispatch. This is shared language behavior for .NET and neoCLR.

`BoundReceiverFacts.IsBaseReceiver` recognizes a `BoundBaseExpression` through
identity conversions and required-value wrappers. It does not treat other
conversions or `self` as base access. The IL emitter and portable linear lowering
use this classification when selecting direct instance calls.

Read/modify/write binding captures receivers and indexes once. If the receiver
is explicit base access, its temporary remains wrapped in `BoundBaseExpression`.
Replacing it with a plain local would incorrectly restore virtual dispatch for
compound assignments and increments. Captured base receiver evaluation and
dispatch selection are separate concerns.

The compiler port exposed this defect through the overriding `BoundPattern.Type`
getter: virtual dispatch of `base.Type` recursively invoked the override. Reduced
runtime tests avoid crashing the host and cover base/ordinary property and indexer
reads, writes, compound assignments and prefix/postfix increments. Shared bound
receiver tests cover transparent wrappers and non-base receivers.

The classification helper is translated into Raven alongside the bound-expression
slice. Carry the binder capture and emitter changes into those components when
they are ported. Current runtime validation targets modern .NET. Portable lowering
has the matching correction; this is not evidence of native neoCLR execution.
No grammar or language-service-specific change is required.
