using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConstructedMethodSymbolTests
{
    [Fact]
    public void ConstructedMethod_UsesConstructedContainerForDisplay()
    {
        var source = """
import System.*

class Outer<T>
{
    public static Wrap<U>(value: T, other: U) -> T => value;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-container",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Outer"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(stringType));

        var wrapDefinition = Assert.Single(
            constructedOuter
                .GetMembers("Wrap")
                .OfType<IMethodSymbol>());

        var constructedWrap = Assert.IsAssignableFrom<IMethodSymbol>(wrapDefinition.Construct(intType));

        Assert.Same(constructedOuter, constructedWrap.ContainingType);
        Assert.Equal("Outer`1", constructedWrap.ContainingType?.ToFullyQualifiedMetadataName());
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, Assert.Single(constructedWrap.TypeArguments)));

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, constructedWrap.ReturnType));
        Assert.Collection(
            constructedWrap.Parameters,
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(stringType, parameter.Type)),
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type)));

        var display = constructedWrap.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);
        Assert.Equal("Outer<string>.Wrap<int>(string, int) -> string", display);
    }

    [Fact]
    public void ConstructedMethod_ToStringAndDebuggerDisplay_UseDisplayStrings()
    {
        var source = """
import System.*

class Outer<T>
{
    public static Wrap<U>(value: T, other: U) -> T => value;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-debugger-display",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Outer"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(stringType));

        var wrapDefinition = Assert.Single(
            constructedOuter
                .GetMembers("Wrap")
                .OfType<IMethodSymbol>());

        var constructedWrap = Assert.IsAssignableFrom<IMethodSymbol>(wrapDefinition.Construct(intType));

        var errorDisplay = constructedWrap.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);
        Assert.Equal(errorDisplay, constructedWrap.ToString());

        var debuggerMethod = typeof(ConstructedMethodSymbol).GetMethod(
            "GetDebuggerDisplay",
            BindingFlags.Instance | BindingFlags.NonPublic);

        var debuggerDisplay = Assert.IsType<string>(debuggerMethod!.Invoke(constructedWrap, null));
        var fullyQualified = constructedWrap.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal($"{constructedWrap.Kind}: {fullyQualified}", debuggerDisplay);
    }

    [Fact]
    public void ConstructedMetadataMethod_DisplayUsesSubstitutedTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listOfString = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(stringType));

        var find = Assert.Single(listOfString.GetMembers("Find").OfType<IMethodSymbol>());

        var errorDisplay = find.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);
        Assert.Equal("List<string>.Find(Predicate<string>) -> string", errorDisplay);
        Assert.Equal(errorDisplay, find.ToString());

        var fullyQualified = find.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal("public System.Collections.Generic.List<string>.Find(match: System.Predicate<string>) -> string", fullyQualified);
    }

    [Fact]
    public void ConstructedMethod_SubstitutesMethodTypeParametersInNestedReturnType()
    {
        var source = """
class Outer<T> {
    public class Inner {
        public init(value: T) { }
    }
}

class Factory {
    public static Create<U>(value: U) -> Outer<U>.Inner {
        return Outer<U>.Inner(value)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-nested-return",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var factorySyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Factory");
        var outerSyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Outer");

        var factoryDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(factorySyntax));

        var createDefinition = Assert.Single(factoryDefinition.GetMembers("Create").OfType<IMethodSymbol>());

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructedCreate = Assert.IsAssignableFrom<IMethodSymbol>(createDefinition.Construct(intType));

        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedCreate.ReturnType);
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(outerSyntax));
        var outerInt = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));
        Assert.Equal("Inner", returnType.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(outerInt, returnType.ContainingType));
        Assert.Equal("Outer<int>.Inner", returnType.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat));
        Assert.Collection(
            constructedCreate.Parameters,
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type)));
    }

    [Fact]
    public void ConstructedMethod_SubstitutesMethodTypeParametersInNestedGenericReturnType()
    {
        var source = """
class Outer<T> {
    public class Inner<V> {
        public init(value: T, other: V) { }
    }
}

class Factory {
    public static Create<U, V>(value: U, other: V) -> Outer<U>.Inner<V> {
        return Outer<U>.Inner<V>(value, other)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-nested-generic-return",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var factorySyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Factory");
        var outerSyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Outer");

        var factoryDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(factorySyntax));

        var createDefinition = Assert.Single(factoryDefinition.GetMembers("Create").OfType<IMethodSymbol>());

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructedCreate = Assert.IsAssignableFrom<IMethodSymbol>(createDefinition.Construct(intType, stringType));

        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedCreate.ReturnType);
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(outerSyntax));
        var outerInt = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));
        Assert.Equal("Inner", returnType.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(outerInt, returnType.ContainingType));
        Assert.Collection(
            returnType.TypeArguments,
            argument => Assert.True(SymbolEqualityComparer.Default.Equals(stringType, argument)));

        Assert.Collection(
            constructedCreate.Parameters,
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type)),
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(stringType, parameter.Type)));
    }
}
