using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UnitTypeTests : CompilationTestBase
{
    [Fact]
    public void FunctionWithoutReturnType_DefaultsToUnit()
    {
        var source = """
func ping() { }
let u = ping()
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ping = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var pingSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ping)!;
        Assert.Equal(SpecialType.System_Unit, pingSymbol.ReturnType.SpecialType);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initializer = declarator.Initializer!;
        var initType = model.GetTypeInfo(initializer.Value!).Type;
        Assert.NotNull(initType);
        Assert.Equal(SpecialType.System_Unit, initType!.SpecialType);
    }

    [Fact]
    public void ExplicitUnitTypeAnnotation_BindsToUnit()
    {
        var source = """
func ping() -> () { }
let x: () = ping()
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ping = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var pingSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ping)!;
        Assert.Equal(SpecialType.System_Unit, pingSymbol.ReturnType.SpecialType);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initializer = declarator.Initializer!;
        var initType = model.GetTypeInfo(initializer.Value!).Type;
        Assert.NotNull(initType);
        Assert.Equal(SpecialType.System_Unit, initType!.SpecialType);
    }
}
