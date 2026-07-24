# Lock statements

A lock statement executes a body while holding the monitor associated with a
reference value:

```raven
lock gate {
    updateSharedState()
}
```

The lock expression must have a reference type. It is evaluated exactly once
before the monitor is acquired. The monitor is released when control leaves the
body, including when the body returns or throws an exception.

The statement is equivalent in effect to evaluating the expression into a
hidden local, calling `System.Threading.Monitor.Enter`, executing the body in a
`try`, and calling `System.Threading.Monitor.Exit` from `finally`.
