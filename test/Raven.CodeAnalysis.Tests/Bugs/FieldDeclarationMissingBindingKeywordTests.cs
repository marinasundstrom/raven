using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class FieldDeclarationMissingBindingKeywordTests : Raven.CodeAnalysis.Semantics.Tests.CompilationTestBase
{
    [Fact]
    public void MemberWithoutBindingKeyword_DoesNotProducePropertyBindingDiagnostic()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var (compilation, _) = CreateCompilation(
            code,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.PropertyDeclarationRequiresBindingKeyword.Id);
    }

    [Fact]
    public void MemberWithoutBindingKeyword_ParsesAsIncompleteMember()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        var type = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        Assert.IsType<IncompleteMemberDeclarationSyntax>(Assert.Single(type.Members));
    }
}
