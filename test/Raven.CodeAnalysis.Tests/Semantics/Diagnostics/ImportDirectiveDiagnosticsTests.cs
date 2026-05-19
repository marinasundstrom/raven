using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ImportDirectiveDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void RedundantGlobalImport_FileScopedNamespaceImports_ProducesHiddenDiagnostics()
    {
        var prelude = SyntaxTree.ParseText(
            """
            global {
                import System.*
                import System.Console.*
            }
            """,
            path: "Prelude.rvn");

        var source = SyntaxTree.ParseText(
            """
            namespace Samples.NamespaceMembers.Members

            import System.*
            import System.Console.*

            public func PrintSummary() {
                WriteLine("ok")
            }
            """,
            path: "Members.rvn");

        var compilation = CreateCompilation([prelude, source], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id)
            .ToArray();

        Assert.Equal(2, diagnostics.Length);
        Assert.All(diagnostics, diagnostic =>
        {
            Assert.Equal(DiagnosticSeverity.Hidden, diagnostic.Severity);
            Assert.Same(source, diagnostic.Location.SourceTree);
        });
    }

    [Fact]
    public void RedundantGlobalImport_NestedNamespaceImports_ProducesHiddenDiagnostics()
    {
        var prelude = SyntaxTree.ParseText(
            """
            global {
                import System.*
            }
            """,
            path: "Prelude.rvn");

        var source = SyntaxTree.ParseText(
            """
            namespace Outer {
                import System.*

                namespace Inner {
                    import System.*

                    class C {}
                }
            }
            """,
            path: "Nested.rvn");

        var compilation = CreateCompilation([prelude, source], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id)
            .ToArray();

        Assert.Equal(2, diagnostics.Length);
        Assert.All(diagnostics, diagnostic =>
        {
            Assert.Equal(DiagnosticSeverity.Hidden, diagnostic.Severity);
            Assert.Same(source, diagnostic.Location.SourceTree);
        });
    }
}
