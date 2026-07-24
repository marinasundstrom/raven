using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RefFieldDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void RefStruct_CanDeclareRefField()
    {
        const string source = """
            ref struct Buffer {
                field Value: &int
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<FieldDeclarationSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration.Declaration.Declarators.Single()));

        Assert.Equal(RefKind.Ref, field.RefKind);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void OrdinaryStruct_CannotDeclareRefField()
    {
        const string source = """
            struct Buffer {
                field Value: &int
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefFieldRequiresRefStruct);
    }

    [Fact]
    public void Class_CannotDeclareRefField()
    {
        const string source = """
            class Buffer {
                field Value: &int
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefFieldRequiresRefStruct);
    }

    [Fact]
    public void RefField_CannotBeStatic()
    {
        const string source = """
            ref struct Buffer {
                static field Value: &int
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefFieldCannotBeStatic);
    }

    private void AssertHasDiagnostic(string source, DiagnosticDescriptor descriptor)
    {
        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Descriptor == descriptor);
    }
}
