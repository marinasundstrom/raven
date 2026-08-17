# Jumps and labels

Use `break` and `continue` to control loops. Labels can identify an enclosing
loop or provide a target for `goto`.

## `break` and `continue`

`break` exits the innermost `loop`, `while`, or `for`. `continue` skips the
remainder of the current iteration and starts the next one.

```raven
for value in values {
    if shouldStop(value) {
        break
    }

    if shouldSkip(value) {
        continue
    }

    consume(value)
}
```

Both transfers are valid as statements and as abrupt expressions. Expression
forms are useful in an inline `if` or `match` branch and do not contribute a
type because execution does not continue through that path.

```raven
outer: loop {
    for value in values {
        match classify(value) {
            .Stop => break outer
            .Skip => continue
            .Use => consume(value)
        }
    }
}
```

### Loop-transfer rules

* `break` and `continue` must occur inside a loop; otherwise Raven reports
  `RAV2600` and `RAV2601`, respectively.
* Statement-form transfers cannot be placed directly in expression contexts;
  doing so reports `RAV1902` for `break` or `RAV1903` for `continue`. Use their
  expression forms in those positions.
* A labeled transfer must name an enclosing labeled loop. Targeting an ordinary
  statement label reports `RAV2606`.

## Labels

A label is an identifier followed by `:` before a statement. Multiple labels
may refer to the same statement, and a label followed immediately by a newline
is still a valid target.

```raven
start:
performWork()
```

Label names belong to their containing function, lambda, or accessor. A name
may be declared only once in that body; duplicates report `RAV2500`. Escaped
identifiers use their logical name for lookup, so `@loop:` and `goto @loop`
refer to the same label. A label is statement syntax; placing one directly in
an expression context reports `RAV1905`.

## `goto`

`goto name` transfers execution to a label in the same function-like body. The
target may appear before or after the jump.

```raven
func retryingWork() {
start:
    let succeeded = tryOnce()
    if not succeeded {
        goto start
    }
}
```

A jump cannot cross into another function, lambda, or accessor. Any scopes it
leaves are unwound before execution continues at the target. An unknown target
reports `RAV2501`; a missing or invalid label name reports `RAV2502`. `goto` is
a statement and using it directly in an expression context reports `RAV1904`.
