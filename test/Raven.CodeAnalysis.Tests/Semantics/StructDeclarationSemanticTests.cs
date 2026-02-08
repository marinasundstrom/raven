using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StructDeclarationSemanticTests : CompilationTestBase
{
    [Fact]
    public void StructDeclaration_BindsAsValueType()
    {
        const string source = """
struct Point {
    var X: int = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var point = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(TypeKind.Struct, point.TypeKind);
        Assert.Equal(SpecialType.System_ValueType, point.BaseType?.SpecialType);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
