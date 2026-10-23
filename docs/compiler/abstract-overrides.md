# Abstract override dispatch

An abstract override keeps the inherited virtual slot. It makes a derived
implementation mandatory without creating a separate dispatch contract. For example,
a closed record base may declare `abstract override func ToString() -> string`;
derived records supply the display string seen through the base, System.Object,
and string interpolation. Ordinary abstract declarations introduce a new slot.

This follows CLR virtual dispatch and applies to classes as well as records.
The CLI emitter uses Abstract and Virtual without NewSlot for overrides. No runtime
contract configuration or neoCLR-specific mapping is involved. The focused
RecordInheritanceExecutionTests cover abstract and concrete base overrides with
payload and parameterless record cases, including Object dispatch and interpolation.
The native backend has separate record-body lowering and requires its own validation.

Empty records also expose a synthesized zero-parameter Deconstruct body through
the common body factory, in both original and lowered views. This includes closed
record bases and parameterless derived cases. Backends must consume that body
rather than attempt to bind the record declaration as an executable statement.
