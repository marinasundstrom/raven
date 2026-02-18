using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExternSemanticTests : CompilationTestBase
{
    [Fact]
    public void ExternInstanceMethod_ReportsDiagnostic()
    {
        const string source = """
class Native {
    extern Sleep(milliseconds: int) -> unit;
}
""";

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.ExternMemberMustBeStatic.Id);
    }

    [Fact]
    public void ExternFunctionWithBody_ReportsDiagnostic()
    {
        const string source = """
extern func NativeSleep(milliseconds: int) -> unit {
    return
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.ExternMemberCannotHaveBody.Id);
    }

    [Fact]
    public void ExternFunctionSymbol_IsMarkedExtern()
    {
        const string source = "extern func NativeSleep(milliseconds: int) -> unit;";
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options);
        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(function));
        Assert.True(symbol.IsExtern);
    }

}
