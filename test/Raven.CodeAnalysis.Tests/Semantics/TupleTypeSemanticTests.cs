using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TupleTypeSemanticTests
{
    [Fact]
    public void TupleTypeSyntax_BindsToTupleTypeSymbol_WithNames()
    {
        var source = """
        let t: (id: int, name: string) = (1, "")
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var tuple = Assert.IsAssignableFrom<ITupleTypeSymbol>(type);
        Assert.Collection(tuple.TupleElements,
            e => { Assert.Equal("id", e.Name); Assert.Equal(SpecialType.System_Int32, e.Type.SpecialType); },
            e => { Assert.Equal("name", e.Name); Assert.Equal(SpecialType.System_String, e.Type.SpecialType); });
    }

    [Fact]
    public void TupleExpression_TargetTyped_UsesDeclaredType_IgnoringNames()
    {
        var source = """
        let pair: (id: int, name: string) = (no: 42, identifier: "answer")
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initializerType = model.GetTypeInfo(declarator.Initializer!.Value).Type;
        var annotationType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        Assert.True(SymbolEqualityComparer.Default.Equals(annotationType, initializerType));
    }

    [Fact]
    public void TupleExpression_TargetTypedMismatch_ReportsDiagnostic()
    {
        var source = "let t: (int, string) = (1, 2)";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType, diagnostic.Descriptor);
    }
}
