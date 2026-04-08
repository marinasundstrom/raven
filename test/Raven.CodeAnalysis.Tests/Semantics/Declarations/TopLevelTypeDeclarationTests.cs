using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Declarations;

public sealed class TopLevelTypeDeclarationTests : CompilationTestBase
{
    [Fact]
    public void SingleTopLevelInterface_DoesNotReportDuplicateDeclaration()
    {
        var tree = SyntaxTree.ParseText(
            """
            interface IError { }
            """);

        var compilation = CreateCompilation(
            tree,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.TypeAlreadyDefined.Id);
    }
}
