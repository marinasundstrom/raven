using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class EnumTypeNameCollisionTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNetPropertyNamedAfterEnum_AllowsEnumConstantsAndDefaults(bool diagnosticsFirst)
        => VerifyPropertyCollision(diagnosticsFirst, TargetPlatform.DotNet);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClrPropertyNamedAfterEnum_WithHostReferences_AllowsEnumConstantsAndDefaults(bool diagnosticsFirst)
        => VerifyPropertyCollision(diagnosticsFirst, TargetPlatform.NeoCLR);

    private static void VerifyPropertyCollision(bool diagnosticsFirst, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("""
            enum Format { Xml, Markdown }
            class Options {
                init() { Format = Format.Markdown }
                init(format: Format = Format.Markdown) { Format = format }
                val Format: Format { get; init; }
            }
            """);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        options = options.WithTargetPlatform(target);
        var compilation = Compilation.Create("EnumCollision", [tree], TestMetadataReferences.Default, options);
        var model = compilation.GetSemanticModel(tree);
        var diagnostics = () => target == TargetPlatform.NeoCLR ? model.GetDiagnostics() : compilation.GetDiagnostics();
        if (diagnosticsFirst)
            Assert.DoesNotContain(diagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        foreach (var access in tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>())
        {
            Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(access.Expression).Symbol);
            var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(access).Symbol);
            Assert.Equal("Markdown", field.Name);
            Assert.True(field.IsConst);
            Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(access.Expression).Symbol);
        }
        Assert.DoesNotContain(diagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var constructor = compilation.GetTypeByMetadataName("Options")!.Constructors.Single(c => c.Parameters.Length == 1);
        Assert.True(constructor.Parameters[0].HasExplicitDefaultValue);
        Assert.Equal(1, constructor.Parameters[0].ExplicitDefaultValue);
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("func Pick(Format: Format) -> Format => Format.Markdown")]
    [InlineData("func Pick() -> Format { let Format: Format = .Xml; return Format.Markdown }")]
    [InlineData("class Options { var Format: Format = .Xml; func Pick() -> Format => Format.Markdown }")]
    public void SameTypedValueName_AllowsEnumConstant(string declaration)
    {
        var tree = SyntaxTree.ParseText("enum Format { Xml, Markdown }\n" + declaration);
        var compilation = Compilation.Create("EnumValueCollision", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void DifferentTypedValueName_PreservesInstanceMember()
    {
        var tree = SyntaxTree.ParseText("""
            enum Format { Xml, Markdown }
            class Other { val Markdown: int = 42 }
            func Pick(Format: Other) -> int => Format.Markdown
            """);
        var compilation = Compilation.Create("DifferentEnumName", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var access = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        Assert.IsAssignableFrom<IPropertySymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(access).Symbol);
    }
    [Fact]
    [Trait("Target", "Shared")]
    public void QualifiedInstance_IsNotReinterpretedAsEnumType()
    {
        var tree = SyntaxTree.ParseText("""
            enum Format { Xml, Markdown }
            class Options {
                val Format: Format = .Xml
                func Pick() -> Format => self.Format.Markdown
            }
            """);
        var compilation = Compilation.Create("QualifiedEnumValue", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

}
