# Raven Syntax Quoter

This sample uses `RavenQuoter` from Raven code to turn a Raven source snippet
into the `SyntaxFactory` calls that recreate its syntax tree.

The quoter emits Raven code by default, including Raven `import` declarations
and collection expressions. Pass
`RavenQuoterOptions { OutputLanguage = RavenQuoterOutputLanguage.CSharp }`
when C# output is needed.

## Run

From the repository root:

```bash
dotnet run --project samples/projects/syntax-quoter/SyntaxQuoterSample.rvnproj \
  --property WarningLevel=0
```

The same default Raven output is available from:

```bash
rvn dev quote path/to/source.rvn
```

Once Raven supports macros that can quote syntax directly, this API is a
natural foundation for an invocable quoter macro. This sample intentionally
uses the runtime API until that macro model exists.
