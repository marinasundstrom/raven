# Macro Token Stream (`.rvnproj`)

This sample shows a token-tree macro that replaces Raven's standard macro
token stream with a custom lexer.

The application invokes:

```raven
let answer = #customToken {
    ⟨answer⟩
}
```

The C# macro provider implements both `ITokenTreeExpressionMacro` and
`IMacroTokenStreamProvider`. Its custom stream emits a `SyntaxToken` with an
application-defined `RawKind`, while `SyntaxKind.None` makes clear that the
token is not part of Raven's normal lexer. The macro consumes that token and
lowers the DSL directly to the ordinary Raven expression `42`.

This keeps custom lexing local to the macro invocation. It does not add a token
kind to Raven or change how normal Raven source is lexed.

Files:

- `app/MacroTokenStream.rvnproj`: Raven application using the macro
- `app/src/main.rvn`: token-tree invocation
- `macros/TokenStreamMacros.csproj`: C# compiler-plugin project
- `macros/CustomTokenMacro.cs`: custom token stream and macro expansion

Run the sample:

```bash
dotnet run --project app/MacroTokenStream.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
```
