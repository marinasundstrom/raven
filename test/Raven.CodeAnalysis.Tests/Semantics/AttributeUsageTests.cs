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
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.Text == "C");
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        _ = type.GetAttributes();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = diagnostics.First(d => d.Descriptor.Id == "RAV0502");

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
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.Text == "C");
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        _ = type.GetAttributes();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);

        Assert.Equal("RAV0503", diagnostic.Descriptor.Id);
        Assert.Equal("Attribute 'SerializableAttribute' cannot be applied multiple times to the same 'class'", diagnostic.GetMessage());
        Assert.Equal(classDeclaration.AttributeLists[1].Attributes.Single().GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void AttributeUsageAttribute_RespectsNamedAllowMultiple()
    {
        const string source = """
import System.*
import System.Text.Json.Serialization.*

[JsonDerivedType(typeof(Button), "Button")]
[JsonDerivedType(typeof(StackPanel), "StackPanel")]
class Control { }

class Button : Control { }
class StackPanel : Control { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.Text == "Control");
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        _ = type.GetAttributes();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV0503");
    }

    [Fact]
    public void AttributeArgumentMustBeConstant_ReportsDiagnostic()
    {
        const string source = """
import System.*

class WithProperty
{
    public static Value() -> int { return 42 }
}

class MyAttribute : Attribute
{
    init() { }

    public Value: int { get; set; }
}

[My(Value: WithProperty.Value())]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.Text == "C");
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;
        _ = type.GetAttributes();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);

        Assert.Equal("RAV0504", diagnostic.Descriptor.Id);
        Assert.Equal("Attribute argument must be a constant expression", diagnostic.GetMessage());
        Assert.Equal(classDeclaration.AttributeLists.Single().Attributes.Single().ArgumentList!.Arguments.Single().Expression.GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void AttributeNamedArgumentWithEquals_IsRejectedByParser()
    {
        const string source = """
import System.*

[AttributeUsage(AttributeTargets.Enum, AllowMultiple = true)]
class OnlyEnumAttribute : Attribute
{
    init() { }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = diagnostics.First(d => d.Descriptor.Id == "RAV1003");

        Assert.Equal("RAV1003", diagnostic.Descriptor.Id);
        Assert.Contains(":", diagnostic.GetMessage());
    }
}
