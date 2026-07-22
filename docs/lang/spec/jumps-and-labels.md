# Jumps and labels

`break` exits the innermost enclosing loop statement immediately. Execution
resumes at the statement following that loop. `break label` exits the enclosing
loop statement marked with `label`.

```raven
var i = 0
while true {
    if i == 10 {
        break
    }

    i += 1
}
```

A `break` statement must appear within a `loop`, `while`, or `for` *statement*.
Placing `break` in any expression context, including the bodies of `if`, `while`,
or `for` expressions, produces diagnostic `RAV1902`. Using `break` outside a
loop reports diagnostic `RAV2600`. A labeled `break` must name an enclosing
labeled loop. Labels on ordinary statements remain `goto` targets and produce
diagnostic `RAV2606` when used as a `break` target.

## `continue` statements

`continue` skips the remainder of the current loop iteration and jumps to the
loop's re-check point. `continue label` skips to the next iteration of the
enclosing loop statement marked with `label`.

```raven
for value in values {
    if value.isOdd {
        continue
    }

    print(value)
}
```

`continue` follows the same placement rules as `break`: it may only appear inside
`loop`, `while`, or `for` statements. Using it from an expression context results
in diagnostic `RAV1903`, and placing it outside a loop reports diagnostic
`RAV2601`. A labeled `continue` must name an enclosing labeled loop. Labels on
ordinary statements remain `goto` targets and produce diagnostic `RAV2606` when
used as a `continue` target.

```raven
outer: loop {
    inner: loop {
        if shouldStop {
            break outer
        }

        continue inner
    }
}
```

## Labeled statements

A **labeled statement** prefixes another statement with an identifier followed by
a colon:

```raven
start:
print("running")
```

Labels introduce symbolic targets that `goto` statements can reference. A label
applies to the next statement in the source. When a newline immediately follows
the colon, the compiler synthesizes an empty statement so the label remains a
valid target even without an explicit body. Multiple labels may precede the same
statement.

Label names are scoped to the containing function body. Declaring the same label
more than once in the same body is an error (`RAV2500`). Labels participate in
semantic lookup just like other declarations, so tools can navigate to them.

Like all statements, a label may only appear where the grammar permits
statements. Attempting to nest a labeled statement inside an expression body (for
example, inside the branches of an `if` expression) reports diagnostic `RAV1905`.

## `goto` statements

The `goto` statement jumps to a labeled statement within the same function-like
body:

```raven
start:
print("loop")
goto start
```

Evaluation of a `goto` statement ends the current statement immediately and
transfers control to the target label. Targets may appear before or after the
`goto`; backward jumps form loops while forward jumps skip ahead in the block.
Jumping to a nonexistent label produces diagnostic `RAV2501`.

Gotos cannot escape the body they are declared in. A `goto` inside a function,
lambda, or accessor may only refer to labels declared in that same body. The
compiler ensures any scopes exited by the jump are correctly unwound before
branching.

`goto` statements follow the same placement rules as other control-flow
statements: they are only legal in statement contexts. Embedding a `goto` inside
an expression context (such as the body of an `if` expression) produces
diagnostic `RAV1904`.
