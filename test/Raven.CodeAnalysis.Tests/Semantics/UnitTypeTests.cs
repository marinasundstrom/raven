using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UnitTypeTests
{
    [Fact]
    public void FunctionWithoutReturnType_DefaultsToUnit()
    {
        var source = """
class C {
    Foo() { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        Assert.True(SymbolEqualityComparer.Default.Equals(symbol.ReturnType, compilation.UnitTypeSymbol));
    }

    [Fact]
    public void UnitLiteral_BindsToUnitType()
    {
        var source = """
        let x = unit
        """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var typeInfo = model.GetTypeInfo(declarator.Initializer!.Value);
        Assert.True(SymbolEqualityComparer.Default.Equals(typeInfo.Type, compilation.UnitTypeSymbol));
    }
}
