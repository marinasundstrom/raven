# Macro Token Stream (`.rvnproj`)

This sample shows a token-tree macro that replaces Raven's standard macro
token stream with a custom lexer.

The application invokes:

```raven
let answer = customToken!(Value: 42) {
    ⟨answer⟩
}
```

The Raven-authored macro provider implements
`ITokenTreeExpressionMacro<CustomTokenParameters>` together with
`IMacroTokenStreamProvider`. Raven binds the named `Value` argument into the
strongly typed parameter object while preserving the brace body as unrestricted
raw content. The custom stream emits a `SyntaxToken` with an
application-defined `RawKind`, while `SyntaxKind.None` makes clear that the
token is not part of Raven's normal lexer. The macro consumes that token and
lowers the DSL directly to the ordinary Raven expression supplied by `Value`.

This keeps custom lexing local to the macro invocation. It does not add a token
kind to Raven or change how normal Raven source is lexed.

Files:

- `app/MacroTokenStream.rvnproj`: Raven application using the macro
- `app/src/Main.rvn`: token-tree invocation and executable entry point
- `macros/TokenStreamMacros.rvnproj`: Raven compiler-plugin project
- `macros/CustomTokenMacro.rvn`: custom token stream and macro expansion

Run the sample:

```bash
dotnet run --project app/MacroTokenStream.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
```
