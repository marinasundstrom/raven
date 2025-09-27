using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AttributeUsageTests : CompilationTestBase
{
    [Fact]
    public void AttributeAppliedToInvalidTarget_ReportsDiagnostic()
    {
        const string source = """
[System.Flags]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        _ = type.GetAttributes();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);

        Assert.Equal("RAV0502", diagnostic.Descriptor.Id);
        Assert.Equal("Attribute 'FlagsAttribute' is not valid on target 'class'. Valid targets are 'enum'", diagnostic.GetMessage());
        Assert.Equal(classDeclaration.AttributeLists.Single().Attributes.Single().GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void AttributeAppliedMultipleTimesWithoutAllowMultiple_ReportsDiagnostic()
    {
        const string source = """
[System.Serializable]
[System.Serializable]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        _ = type.GetAttributes();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);

        Assert.Equal("RAV0503", diagnostic.Descriptor.Id);
        Assert.Equal("Attribute 'SerializableAttribute' cannot be applied multiple times to the same 'class'", diagnostic.GetMessage());
        Assert.Equal(classDeclaration.AttributeLists[1].Attributes.Single().GetLocation(), diagnostic.Location);
    }
}
