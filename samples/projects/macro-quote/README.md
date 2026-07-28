# Quote Macro (`.rvnproj`)

This sample shows `quote!` in its primary macro-authoring role: constructing the
Raven syntax returned by another macro without manually assembling a syntax
tree.

The Raven-authored `#twice` macro receives an ordinary Raven expression and
returns:

```raven
quote! {
    #(context.Arguments[0].Expression) + #(context.Arguments[0].Expression)
}
```

The `#(...)` holes splice the caller's expression syntax into the quoted
expression. The quote is evaluated while the macro provider is compiled and
the resulting syntax becomes the expansion used by the application.

Files:

- `app/MacroQuote.rvnproj`: Raven application using `#twice`
- `app/src/Main.rvn`: macro invocation and executable entry point
- `macros/QuoteMacros.rvnproj`: Raven compiler-plugin project
- `macros/TwiceMacro.rvn`: quote-based macro implementation

Run the sample:

```bash
dotnet run --project app/MacroQuote.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
```
