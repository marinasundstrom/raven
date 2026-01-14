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
}
