# Match forms

Raven supports `match` in expression and statement positions. The normal
expression form is keyword-first so it reads the same way as statement-form
`match`, which matches the shape most programmers expect:

* **Expression form**: `match scrutinee { ... }`
* **Statement form**: `match scrutinee { ... }`
* **Postfix expression form**: `scrutinee match { ... }`

The postfix expression form remains valid for composition cases where the
scrutinee is already a prefix expression, such as
`try Convert.ToInt32(text) match { ... }`.

In statement context, `match scrutinee { ... }` is parsed as a
`MatchStatementSyntax` before expression statements are considered. The
keyword-first match expression form is used only when the parser is already
expecting an expression, such as after `=`, `return`, an argument separator, or
another expression-only position.

The parser represents these as distinct syntax nodes: keyword-first expression
form produces `MatchExpressionSyntax`, postfix expression form produces
`PostfixMatchExpressionSyntax`, and statement form produces
`MatchStatementSyntax`.

Both forms use the same pattern binder and diagnostics, including exhaustiveness
checking and unreachable-arm detection.

In expression form, the `match` result is the selected arm expression value. In
statement form, the selected arm expression is evaluated and its resulting value
is discarded.
When statement-form `match` is the final statement in a value-returning body,
its selected arm value contributes an implicit tail return.

Likewise, statement-form `if` with an `else` branch contributes an implicit tail
return when it is the final statement in a value-returning body.

In statement form, arm block expressions are interpreted in statement context.
That means explicit `return`/`throw` statements inside those arm blocks are
valid. `return` exits the enclosing function/method and `throw` raises an
exception. In expression form, direct arm expressions may use `return` and
`throw` expressions, but statement `return`/`throw` inside block-expression arms
remains disallowed and reports `RAV1900`/`RAV1907`.

Arm bodies accept any expression, including block expressions:

```raven
let label = match value {
    0 => { "zero" }
    _ => {
        let text = "other"
        text
    }
}

match value {
    0 => { Console.WriteLine("zero") }
    _ => {
        Console.WriteLine("other")
        ()
    }
}
```

```raven
let label = try Convert.ToInt32(text) match {
    Ok(let value) => "Parsed ${value}"
    Error(_) => "Invalid"
}
```

An optional outer binding keyword may appear before a match-arm pattern:

```raven
match values {
    let [first, second, ...rest] => first + second + rest.Length
    _ => 0
}

match value {
    let Some((x, y)) => x + y
    _ => 0
}

match value {
    let Some((x, y)) pair => pair.Value
    _ => 0
}
```

For match arms, the outer binding keyword uses the same shorthand rule as
deconstruction assignment and `for` pattern targets: it supplies the binding
mode for otherwise bare captures inside the arm pattern. The same ambient
binding mode also applies to an optional trailing whole-pattern designation.
Mixing an outer binding keyword with inline pattern binding keywords in the same
arm is an error.

Statement-form `match` with block arms may use explicit `return`:

```raven
class Evaluator {
    func Eval(scrutinee: bool) -> bool {
        match scrutinee {
            true => {
                return true
            }
            false => {
                return false
            }
        }
    }
}
```
