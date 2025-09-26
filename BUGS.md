# BUGS

## Overview
- `dotnet test test/Raven.CodeAnalysis.Tests` fails during the build step, so no unit tests run. The compiler cannot resolve syntax-tree types such as `SyntaxKind`, `TypeArgumentListSyntax`, `TypeAnnotationClauseSyntax`, and other generated node classes that live under `Raven.CodeAnalysis.Syntax`, causing hundreds of `CS0246` errors emitted from the parser sources.【0e6d90†L67-L135】【0e6d90†L136-L215】

## Assessment
- All reported errors originate from the parser layer (`Syntax/InternalSyntax/Parser`). Each failure is the same pattern: the file tries to use a symbol defined in the generated syntax node set (`SyntaxKind`, `MemberDeclarationSyntax`, `TypeAnnotationClauseSyntax`, etc.), but the compiler believes no such type exists. Because every missing symbol belongs to the generated syntax node tree, this points to a systemic issue rather than dozens of independent typos.
- The project defines `global using Raven.CodeAnalysis.Syntax;` and ships the generated node files under `Syntax/generated/`. Either those files are not being included in the compilation (for example, the `RunNodeGenerator` target is skipped or its output path is excluded), or the global-using file is not being picked up, leaving the parser sources without a reference to the `Raven.CodeAnalysis.Syntax` namespace. Since `DiagnosticBagExtensions.g.cs` *is* compiled (warnings appear earlier in the log), the item list may be partially correct, making the global-using hypothesis worth validating first.【0e6d90†L1-L66】【0e6d90†L216-L236】
- Because every error shares this “generated syntax type missing” signature, they are almost certainly caused by the same underlying configuration or generator regression rather than distinct defects in individual parser files.

## Next steps
1. Inspect the `Raven.CodeAnalysis` project’s compile items (e.g., `msbuild /pp` or `dotnet build -bl`) to verify whether `Syntax/generated/*.cs` and `GlobalUsings.cs` are included. If `GlobalUsings.cs` is absent, add an explicit `using Raven.CodeAnalysis.Syntax;` to the parser sources to confirm the hypothesis, then restore the global import properly.
2. Confirm that the `RunNodeGenerator` MSBuild target executes and that its output files are fresh (check timestamps or rerun `dotnet run --project tools/NodeGenerator` manually). If the generator fails silently, fix the failure or adjust the target path so the produced files land where the project expects them.
3. Once the syntax types resolve, rerun `dotnet test test/Raven.CodeAnalysis.Tests` to surface any remaining test failures beyond the build break.
