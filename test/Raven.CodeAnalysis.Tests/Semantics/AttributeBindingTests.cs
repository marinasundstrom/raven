using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class AttributeBindingTests : CompilationTestBase
{
    [Fact]
    public void AttributeNameIsResolvedFromImports()
    {
        const string source = """
import System.*

[Obsolete("use new")]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var attribute = Assert.Single(type.GetAttributes());

        Assert.Equal("ObsoleteAttribute", attribute.AttributeClass?.Name);
        Assert.Equal("use new", attribute.ConstructorArguments.Single().Value);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AttributeNameWithoutSuffix_BindsToAttributeType()
    {
        const string source = """
import System.*

[Obsolete]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var attribute = Assert.Single(type.GetAttributes());

        Assert.Equal("ObsoleteAttribute", attribute.AttributeClass?.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericAttributeNameWithoutSuffix_BindsTypeArguments()
    {
        const string source = """
import System.*

class MyAttribute<T> : System.Attribute
{
    public init(value: int) { }
}

[My<int>(42)]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(static c => c.Identifier.ValueText == "C");
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var attribute = Assert.Single(type.GetAttributes());
        var attributeClass = attribute.AttributeClass;

        Assert.Equal("MyAttribute", attributeClass?.Name);
        Assert.Single(attributeClass!.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, attributeClass.TypeArguments[0].SpecialType);
        Assert.Equal(42, attribute.ConstructorArguments.Single().Value);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AttributeNameIsResolvedAfterTopLevelStatements()
    {
        const string source = """
import System.*
import System.Console.*

Console.WriteLine("Test");

[Obsolete("Hello")]
class Foo { }
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var attribute = Assert.Single(type.GetAttributes());

        Assert.Equal("ObsoleteAttribute", attribute.AttributeClass?.Name);
        Assert.Equal("Hello", attribute.ConstructorArguments.Single().Value);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void TypeAttributes_AreNotAppliedToSynthesizedConstructors()
    {
        const string source = """
import System.*

[Obsolete]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var typeAttribute = Assert.Single(type.GetAttributes());
        Assert.Equal("ObsoleteAttribute", typeAttribute.AttributeClass?.Name);

        var constructor = Assert.Single(type.Constructors, static c => !c.IsStatic);
        Assert.DoesNotContain(constructor.GetAttributes(), static a => a.AttributeClass?.Name == "ObsoleteAttribute");
    }

    [Fact]
    public void AssemblyTargetedAttribute_BindsToAssemblySymbol()
    {
        const string source = """
import System.Runtime.Versioning.*

[assembly: TargetFramework(".NETCoreApp,Version=v9.0")]

class C { }
""";

        var (compilation, _) = CreateCompilation(source);
        _ = compilation.GetDiagnostics();
        var attribute = Assert.Single(compilation.Assembly.GetAttributes());

        Assert.Equal("TargetFrameworkAttribute", attribute.AttributeClass?.Name);
        Assert.Equal(".NETCoreApp,Version=v9.0", attribute.ConstructorArguments.Single().Value);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AssemblyTargetedAttribute_WithoutBlankLine_BindsToAssemblySymbol()
    {
        const string source = """
import System.Runtime.Versioning.*
[assembly: TargetFramework(".NETCoreApp,Version=v9.0")]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(declaration)!;
        _ = type.GetAttributes();
        Assert.Single(compilation.Assembly.GetAttributes());
        Assert.Empty(type.GetAttributes());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AssemblyTargetedAttribute_InsideNamespace_ReportsInvalidTargetContext()
    {
        const string source = """
import System.Runtime.Versioning.*

namespace N
{
    [assembly: TargetFramework(".NETCoreApp,Version=v9.0")]
    class C { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(declaration)!;
        _ = type.GetAttributes();

        var diagnostic = Assert.Single(compilation.GetDiagnostics(), static d => d.Descriptor.Id == "RAV0502");
        Assert.Equal("Attribute 'TargetFrameworkAttribute' is not valid on target 'assembly'. Valid targets are 'class'", diagnostic.GetMessage());
        Assert.Empty(compilation.Assembly.GetAttributes());
        Assert.Empty(type.GetAttributes());
    }

    [Fact]
    public void ReturnTargetedAttribute_OnMethod_BindsToReturnAttributesOnly()
    {
        const string source = """
import System.Diagnostics.CodeAnalysis.*

class C
{
    [return: MaybeNull]
    public GetName() -> string => "ok"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)model.GetDeclaredSymbol(declaration)!;

        Assert.Empty(method.GetAttributes());
        var returnAttribute = Assert.Single(method.GetReturnTypeAttributes());
        Assert.Equal("MaybeNullAttribute", returnAttribute.AttributeClass?.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ReturnTargetedAttribute_OnType_ReportsInvalidTargetContext()
    {
        const string source = """
import System.*

[return: Obsolete]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(declaration)!;
        _ = type.GetAttributes();

        var diagnostic = Assert.Single(compilation.GetDiagnostics(), static d => d.Descriptor.Id == "RAV0502");
        Assert.Equal("Attribute 'ObsoleteAttribute' is not valid on target 'return value'. Valid targets are 'class'", diagnostic.GetMessage());
        Assert.Empty(type.GetAttributes());
    }
}
