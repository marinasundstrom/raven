using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PointerTypeSemanticTests : CompilationTestBase
{
    [Fact]
    public void PointerTypeSyntax_BindsToPointerTypeSymbol()
    {
        const string source = """
let value = 0
let pointer: *int = &value
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "pointer");

        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);
        var pointerType = Assert.IsAssignableFrom<IPointerTypeSymbol>(typeInfo.Type);
        Assert.Equal(SpecialType.System_Int32, pointerType.PointedAtType.SpecialType);
    }

    [Fact]
    public void AddressOfInitializer_DefaultsToByRefType()
    {
        const string source = """
class C {
    static Test() {
        var value = 0
        let alias = &value
    }
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "alias");

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var byRef = Assert.IsType<ByRefTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, byRef.ElementType.SpecialType);
    }

    [Fact]
    public void AddressOfInitializer_WithPointerAnnotation_RemainsPointer()
    {
        const string source = """
class C {
    static Test() {
        var value = 0
        let alias: *int = &value
    }
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "alias");

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var pointer = Assert.IsAssignableFrom<IPointerTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, pointer.PointedAtType.SpecialType);
    }
}
