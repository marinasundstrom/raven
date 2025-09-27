using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SemanticFactsTests
{
    [Fact]
    public void IsDerivedFrom_ReturnsTrueForDirectBaseType()
    {
        var source = "class Base {} class Derived : Base {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var derived = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Derived")!;
        var baseType = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Base")!;

        Assert.True(SemanticFacts.IsDerivedFrom(derived, baseType));
        Assert.False(SemanticFacts.IsDerivedFrom(baseType, derived));
    }

    [Fact]
    public void IsDerivedFrom_ReturnsTrueForTypeParameterConstraint()
    {
        var source = "class Base {} class Container<T : Base> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Container")!;
        var typeParameter = container.TypeParameters[0];
        var baseType = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Base")!;

        Assert.True(SemanticFacts.IsDerivedFrom(typeParameter, baseType));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForClassImplementation()
    {
        var source = "interface IMarker {} class Implementation : IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var implementation = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Implementation")!;
        var marker = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IMarker")!;

        Assert.True(SemanticFacts.ImplementsInterface(implementation, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForTypeParameterConstraint()
    {
        var source = "interface IMarker {} class Container<T : IMarker> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Container")!;
        var typeParameter = container.TypeParameters[0];
        var marker = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IMarker")!;

        Assert.True(SemanticFacts.ImplementsInterface(typeParameter, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForInterfaceItself()
    {
        var source = "interface IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var marker = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IMarker")!;

        Assert.True(SemanticFacts.ImplementsInterface(marker, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForArrayInterfaces()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayType = compilation.CreateArrayTypeSymbol(intType);
        var enumerable = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;

        Assert.True(SemanticFacts.ImplementsInterface(arrayType, enumerable));
    }

    [Fact]
    public void ImplementsInterface_ArrayHonorsConstructedGenericInterfaces()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayType = compilation.CreateArrayTypeSymbol(intType);

        var enumerableDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfInt = (INamedTypeSymbol)enumerableDefinition.Construct(intType);
        var enumerableOfString = (INamedTypeSymbol)enumerableDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));
        var listDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IList`1")!;
        var listOfInt = (INamedTypeSymbol)listDefinition.Construct(intType);

        Assert.True(SemanticFacts.ImplementsInterface(arrayType, enumerableOfInt));
        Assert.False(SemanticFacts.ImplementsInterface(arrayType, enumerableOfString));
        Assert.True(SemanticFacts.ImplementsInterface(arrayType, listOfInt));
    }

    [Fact]
    public void ImplementsInterface_HandlesCovariantInterfaceArguments()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var listOfString = (INamedTypeSymbol)listDefinition.Construct(stringType);
        var enumerableDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfObject = (INamedTypeSymbol)enumerableDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        Assert.True(SemanticFacts.ImplementsInterface(listOfString, enumerableOfObject));
    }

    [Fact]
    public void ImplementsInterface_InterfaceHandlesCovariance()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var enumerableDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfString = (INamedTypeSymbol)enumerableDefinition.Construct(stringType);
        var enumerableOfObject = (INamedTypeSymbol)enumerableDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        Assert.True(SemanticFacts.ImplementsInterface(enumerableOfString, enumerableOfObject));
    }

    [Fact]
    public void ImplementsInterface_HandlesContravariantInterfaceArguments()
    {
        var source = """
import System.Collections.Generic

class Comparer : IComparer<object>
{
    init() {}

    Compare(x: object?, y: object?) -> int => 0
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var comparer = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Comparer")!;
        var comparerDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IComparer`1")!;
        var comparerOfString = (INamedTypeSymbol)comparerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(SemanticFacts.ImplementsInterface(comparer, comparerOfString));
    }

    [Fact]
    public void ImplementsInterface_SourceVariance_AllowsCovariantMatches()
    {
        var source = """
interface Producer<out T> {}

class Widget : Producer<string> {}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var interfaceSyntax = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var classSyntax = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var producerDefinition = (INamedTypeSymbol)model.GetDeclaredSymbol(interfaceSyntax)!;
        var widget = (INamedTypeSymbol)model.GetDeclaredSymbol(classSyntax)!;

        var producerOfObject = (INamedTypeSymbol)producerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        Assert.True(SemanticFacts.ImplementsInterface(widget, producerOfObject));
    }

    [Fact]
    public void ImplementsInterface_SourceVariance_AllowsContravariantMatches()
    {
        var source = """
interface Consumer<in T> {}

class Handler : Consumer<object> {}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var interfaceSyntax = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var classSyntax = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var consumerDefinition = (INamedTypeSymbol)model.GetDeclaredSymbol(interfaceSyntax)!;
        var handler = (INamedTypeSymbol)model.GetDeclaredSymbol(classSyntax)!;

        var consumerOfString = (INamedTypeSymbol)consumerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(SemanticFacts.ImplementsInterface(handler, consumerOfString));
    }
}
