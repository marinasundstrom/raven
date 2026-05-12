using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class PatternSymbolInfoTests : CompilationTestBase
{
    [Fact]
    public void GetSymbolInfo_QualifiedEnumConstantInIsPattern_ResolvesEnumMember()
    {
        const string source = """
import System.Text.Json.*

func Test(element: JsonElement) -> bool {
    return element.ValueKind is JsonValueKind.Array
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error && diagnostic.Id != "RAV1014");

        var model = compilation.GetSemanticModel(tree);
        var memberName = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Array");

        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(memberName).Symbol);
        Assert.Equal("Array", field.Name);
        Assert.Equal("JsonValueKind", field.ContainingType.Name);
        Assert.Equal(TypeKind.Enum, field.ContainingType.TypeKind);
    }
}
