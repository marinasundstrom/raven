using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class ExplicitInterfaceBindingAcrossSyntaxTreesTests : Raven.CodeAnalysis.Semantics.Tests.CompilationTestBase
{
    [Fact]
    public void ExplicitInterfacePropertyImplementation_ResolvesAcrossSyntaxTrees()
    {
        var errorTree = SyntaxTree.ParseText(
            """
            namespace System

            interface IError {
                val Message: string
                val Cause: IError? => null
            }

            interface IParseError: IError {
            }
            """);

        var parseTree = SyntaxTree.ParseText(
            """
            namespace System

            import System.*

            record ParseIntError(
                val Message: string
            ) : IParseError {
                val IError.Cause: IError? => null
            }
            """);

        var compilation = CreateCompilation(
            [errorTree, parseTree],
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.ExplicitInterfaceMemberNotFound.Id);
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }
}
