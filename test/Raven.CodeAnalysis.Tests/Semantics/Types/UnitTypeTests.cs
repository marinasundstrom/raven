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
val u = ping()
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
val x: () = ping()
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
    public void ReflectionTypeLoader_Wraps_Void_In_UnitTypeSymbol()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var type = compilation.GetType(typeof(void));

        Assert.NotNull(type);
        Assert.Equal(SpecialType.System_Unit, type!.SpecialType);
        Assert.Same(compilation.UnitTypeSymbol, type);
    }

    [Fact]
    public void ReflectionTypeLoader_Wraps_MetadataMethodVoidReturn_In_UnitTypeSymbol()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var console = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("System.Console"));
        var writeLineMethods = console.GetMembers("WriteLine")
            .OfType<IMethodSymbol>()
            .ToArray();
        var writeLine = writeLineMethods.First(method => method.Parameters is [{ Type: var parameterType }] &&
                (parameterType.SpecialType == SpecialType.System_String ||
                 parameterType is NullableTypeSymbol { UnderlyingType.SpecialType: SpecialType.System_String }));

        Assert.All(writeLineMethods, method => Assert.Equal(SpecialType.System_Unit, method.ReturnType.SpecialType));
        Assert.Equal(SpecialType.System_Unit, writeLine.ReturnType.SpecialType);
        Assert.Same(compilation.UnitTypeSymbol, writeLine.ReturnType);
        Assert.Equal("()", writeLine.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.RavenSignatureFormat));
    }

    [Fact]
    public void MetadataInvocationSymbol_Wraps_VoidReturn_In_UnitTypeSymbol()
    {
        var (compilation, tree) = CreateCompilation(
            "System.Console.WriteLine(true)",
            options: new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal(SpecialType.System_Unit, method.ReturnType.SpecialType);
        Assert.Same(compilation.UnitTypeSymbol, method.ReturnType);
        Assert.Equal("()", method.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.RavenSignatureFormat));
    }

}
