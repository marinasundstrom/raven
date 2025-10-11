using System.Linq;
using System.Threading.Tasks;

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

    [Fact]
    public void TypeResolver_Wraps_Void_In_UnitTypeSymbol()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var type = compilation.GetType(typeof(void));

        Assert.NotNull(type);
        Assert.Equal(SpecialType.System_Unit, type!.SpecialType);
        Assert.Same(compilation.UnitTypeSymbol, type);
    }

    [Fact]
    public void TypeResolver_Wraps_Task_In_TaskOfUnit()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var type = (INamedTypeSymbol)compilation.GetType(typeof(Task))!;

        Assert.Equal(SpecialType.System_Threading_Tasks_Task, type.SpecialType);
        Assert.True(type.IsGenericType);
        Assert.Single(type.TypeArguments);
        Assert.Same(compilation.UnitTypeSymbol, type.TypeArguments[0]);
    }
}
