# Common Intermediate Language

CIL, for short, is the intermediate bytecode that C# is compiled into, a format that is platform-independent and portable. It is Just-in-Time (JIT) compiled by the Common Language Runtime (CLR).

## Accessing instance members

There is a difference in how you access members of reference types and value types.

Reference types are always treated as references, so they are passed as addresses by default. Loading a local variable with `ldloc` will load the address since that is what is stored in the local slot.

But with value types, the actual value is stored in the locals, so you need to explicitly load its address using the `ldloca` op code when for instance calling an instance method.