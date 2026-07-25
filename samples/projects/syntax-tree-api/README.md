# Raven Syntax Tree API sample

This Raven console application parses Raven source text with
`Raven.CodeAnalysis`, walks the resulting syntax tree, and recognizes class
declarations with Raven's typed `if let` syntax.

Build Raven first so that `src/Raven.CodeAnalysis/bin/Debug/net10.0` exists,
then run:

```bash
dotnet run --project samples/projects/syntax-tree-api/SyntaxTreeApiSample.rvnproj
```

The program prints the root syntax kind and the name `Greeter` discovered in
the tree. It also prints any parse diagnostics without compiling or changing
the input source.
