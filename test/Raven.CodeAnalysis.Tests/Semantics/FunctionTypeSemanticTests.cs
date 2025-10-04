using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionTypeSemanticTests
{
    [Fact]
    public void FunctionType_WithExplicitParameters_BindsToFunc()
    {
        const string source = "let f: (int, string) -> bool = (i, s) => true";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
        Assert.Equal(TypeKind.Delegate, named.TypeKind);
        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(named.ConstructedFrom);
        Assert.Equal("System.Func", definition.ToDisplayString());
        Assert.Collection(named.TypeArguments,
            t => Assert.Equal(SpecialType.System_Int32, t.SpecialType),
            t => Assert.Equal(SpecialType.System_String, t.SpecialType),
            t => Assert.Equal(SpecialType.System_Boolean, t.SpecialType));
    }

    [Fact]
    public void FunctionType_WithUnitReturn_UsesAction()
    {
        const string source = "let f: int -> unit = _ => ()";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
        Assert.Equal(TypeKind.Delegate, named.TypeKind);
        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(named.ConstructedFrom);
        Assert.Equal("System.Action", definition.ToDisplayString());
        Assert.Collection(named.TypeArguments,
            t => Assert.Equal(SpecialType.System_Int32, t.SpecialType));
    }

    [Fact]
    public void FunctionType_WithManyParameters_SynthesizesDelegate()
    {
        const string source = """
        let f: (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) -> int =
            (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => a0
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var named = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
        Assert.Equal(TypeKind.Delegate, named.TypeKind);
        Assert.StartsWith("<>f__Delegate", named.Name);
        Assert.Equal(18, named.TypeArguments.Length);
    }
}
