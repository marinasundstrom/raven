# Pattern forms and contexts

Patterns compose from the following primitives.

## Pattern contexts

The same pattern syntax appears in several places, but not every place accepts
every pattern form.

**General matching contexts**

These accept Raven’s full pattern vocabulary:

* `expr is pattern`
* `match expr { pattern => ... }`
* `match expr { ... }` statement form
* `if let pattern = expr`
* `while let pattern = expr`
* `let pattern = expr else { ... }`
* `for let pattern in values` and `for pattern in values`

These contexts may use comparison/range/property/member/nominal-deconstruction
patterns, boolean pattern combinators, and optional whole-pattern designations
where the specific construct allows them.

`expr is pattern` is the expression-oriented member of this group. It produces a
`bool`, has no outer binding keyword, and does not accept a trailing
whole-pattern designation. Captures inside an `is` pattern must be written at
the exact extraction point with `val` / `var` / `let`; a bare identifier is a
value pattern against an existing symbol. For example, `person is { Name: name }`
and `person is { Name: == name }` both compare `Name` to the current value of
`name`, while `person is { Name: val name }` declares a new local.

Dedicated pattern statements such as `if let pattern = expr`,
`while let pattern = expr`, and `let pattern = expr else { ... }` are
binding-oriented. Their leading binding keyword
supplies the binding mode for otherwise bare designations inside the pattern and
for an optional trailing whole-pattern designation. In these contexts a bare
member or element designation captures; comparing to an existing value must use
an explicit comparison pattern such as `== name`. In practice, `==` is the
marker for "compare with this existing variable or expression" where a bare
identifier would otherwise capture. Built-in literal patterns such as `"Bob"`,
`42`, `true`, `false`, and `null` keep their literal-matching meaning and do not
need `==`.

A `let ... else` declaration requires its `else` statement to have no reachable
endpoint. The branch must leave the current control-flow region with `return`,
`throw`, `break`, or `continue`. On a successful match, its pattern locals are
declared in the surrounding block rather than a nested branch:

```raven
let Ok(value) = result else {
    return
}

WriteLine(value)
```

```raven
if let Person { Name: name } = person {      // captures `Name` into `name`
    WriteLine(name)
}

if let Person { Name: "Bob" } = person {     // compares with literal "Bob"
    WriteLine("Bob")
}

if let Person { Name: == name } = person {   // compares with existing `name`
    WriteLine("same name")
}

if let Person { Name: == name, Age: age when > 20 } = person {
    WriteLine(age)
}
```

A capture may include a `when` guard. The capture introduces the local, then the
guard constrains that captured sub-value. In binding statements, the outer
binding keyword supplies the capture mode for a bare guarded designation such as
`age when > 20`; in `is` expressions the capture keyword remains explicit, as in
`Age: val age when > 20`.

**Deconstruction contexts**

These accept the deconstruction subset:

* `val ( ... ) = expr`
* `( ... ) = expr`
* `val [ ... ] = expr`
* `[ ... ] = expr`
* deconstruction parameter patterns in supported function/lambda positions

Deconstruction contexts are extraction-oriented. They support positional and
sequence decomposition, nested positional/sequence composition, discards,
typed designations, mutability/binding shorthands, and explicit value checks
that are part of those deconstruction forms. They do **not** use the
full general matching surface as assignment targets. In particular, property
patterns, nominal/member/case-pattern heads, comparison-only top-level
patterns, and other pure match-only forms are not assignment/declaration heads.
