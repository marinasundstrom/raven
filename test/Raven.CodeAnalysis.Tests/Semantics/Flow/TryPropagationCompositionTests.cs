using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TryPropagationCompositionTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AwaitPropagation_ExposesDistinctAwaitAndPropagationTypes(bool diagnosticsFirst)
    {
        const string source = """
import System.*
import System.Threading.Tasks.*
class C {
    static func Foo(gate: Task<Result<int, string>>) -> Task<Result<int, string>> => gate
    static async func Read(gate: Task<Result<int, string>>) -> Task<Result<int, string>> {
        let value = await Foo(gate)?
        return .Ok(value)
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("await-propagation", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(
                Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"))],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var propagation = tree.GetRoot().DescendantNodes().OfType<PropagateExpressionSyntax>().Single();
        var awaitExpression = Assert.IsType<PrefixOperatorExpressionSyntax>(propagation.Expression);
        Assert.Equal(SyntaxKind.AwaitExpression, awaitExpression.Kind);
        Assert.Equal("Result", model.GetTypeInfo(awaitExpression).Type!.Name);
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(propagation).Type!.SpecialType);
        Assert.IsAssignableFrom<IAwaitOperation>(model.GetOperation(awaitExpression));
        Assert.IsAssignableFrom<IPropagationOperation>(model.GetOperation(propagation));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void ExplicitPropagation_UnwrapsOneCarrierLayer(bool diagnosticsFirst, bool shorthand)
    {
        var source = """
import System.*

class C {
    static func Get() -> Result<int, Exception> => .Ok(42)

    static func Preserve() -> Result<Result<int, Exception>, Exception> {
        let inner = (try Get())?
        return .Ok(inner)
    }

    static func Flatten() -> Result<int, Exception> {
        let value = ((try Get())?)?
        return .Ok(value)
    }

    static func Describe() -> Result<string, Exception> {
        let description = (try 42)? match {
            42 => "answer"
            _ => "other"
        }
        return .Ok(description)
    }
}
""";
        if (shorthand)
            source = source.Replace("(try Get())?", "try Get()?")
                .Replace("(try 42)?", "try 42?");
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("try-composition", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(
                Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"))],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var declarations = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>()
            .ToDictionary(d => d.Identifier.ValueText);
        var inner = Assert.IsAssignableFrom<INamedTypeSymbol>(
            Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarations["inner"])).Type);
        Assert.Equal("Result", inner.Name);
        Assert.Equal(SpecialType.System_Int32, inner.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32,
            Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarations["value"])).Type.SpecialType);
        Assert.Equal(SpecialType.System_String,
            Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarations["description"])).Type.SpecialType);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<TryExpressionSyntax>())
            Assert.IsAssignableFrom<ITryExpressionOperation>(model.GetOperation(syntax));
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<PropagateExpressionSyntax>())
            Assert.IsAssignableFrom<IPropagationOperation>(model.GetOperation(syntax));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
