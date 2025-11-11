using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConstructedNamedTypeSymbolTests
{
    [Fact]
    public void ConstructedType_FromSource_SubstitutesTypeArgumentsInMethods()
    {
        var source = """
class Container<T>
{
    public Identity(value: T) -> T => value;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-source-method-substitution",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(stringType));

        var identity = Assert.Single(constructed.GetMembers("Identity").OfType<IMethodSymbol>());

        var parameter = Assert.Single(identity.Parameters);

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, identity.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, parameter.Type));
    }

    [Fact]
    public void ConstructedType_FromSource_SubstitutesNestedTypeMembers()
    {
        var source = """
class Container<T>
{
    public class Holder
    {
        var value: T;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-source-nested-type",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(intType));

        var holder = Assert.IsAssignableFrom<INamedTypeSymbol>(constructed.LookupType("Holder"));
        var field = Assert.Single(holder.GetMembers("value").OfType<IFieldSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, field.Type));
    }

    [Fact]
    public void ConstructedType_MethodGenerics_PreservesMethodTypeParameters()
    {
        var source = """
import System.*

class Container<T>
{
    public Combine<U>(factory: Func<T, U>, seed: T) -> U => factory(seed);
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-source-method-generics",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(stringType));

        var combine = Assert.IsType<SubstitutedMethodSymbol>(
            constructed
                .GetMembers("Combine")
                .OfType<IMethodSymbol>()
                .Single());

        var factoryParameter = combine.Parameters[0];
        var funcType = Assert.IsAssignableFrom<INamedTypeSymbol>(factoryParameter.Type);

        Assert.Equal("Func", funcType.Name);
        Assert.Equal(2, funcType.TypeArguments.Length);

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, funcType.TypeArguments[0]));

        var methodTypeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(funcType.TypeArguments[1]);
        Assert.Same(combine, methodTypeParameter.ContainingSymbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, combine.Parameters[1].Type));

        Assert.IsAssignableFrom<ITypeParameterSymbol>(combine.ReturnType);
        Assert.Same(combine, ((ITypeParameterSymbol)combine.ReturnType).ContainingSymbol);
    }

    [Fact]
    public void LookupType_SubstitutesOuterTypeArguments()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        var enumeratorType = Assert.IsAssignableFrom<INamedTypeSymbol>(listOfInt.LookupType("Enumerator"));

        Assert.Equal("Enumerator", enumeratorType.Name);
        Assert.Equal(1, enumeratorType.Arity);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, enumeratorType.TypeArguments[0]));
    }

    [Fact]
    public void IsMemberDefined_ReturnsSubstitutedMember()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        Assert.True(listOfInt.IsMemberDefined("Add", out var symbol));
        var addMethod = Assert.IsAssignableFrom<IMethodSymbol>(symbol);

        Assert.Single(addMethod.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, addMethod.Parameters[0].Type));
    }

    [Fact]
    public void GetMembers_SubstitutedMethod_UsesConstructedContainingType()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listOfString = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(stringType));

        var addRange = Assert.Single(
            listOfString.GetMembers("AddRange").OfType<IMethodSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(listOfString, addRange.ContainingType));

        var parameter = Assert.Single(addRange.Parameters);
        var enumerable = Assert.IsAssignableFrom<INamedTypeSymbol>(parameter.Type);

        Assert.Equal("IEnumerable", enumerable.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, enumerable.TypeArguments[0]));
    }

    [Fact]
    public void SubstitutedMethod_ReturnType_UsesTypeArgument()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listOfString = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(stringType));

        var find = Assert.Single(
            listOfString.GetMembers("Find").OfType<IMethodSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, find.ReturnType));

        var predicate = Assert.Single(find.Parameters);
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicate.Type);

        Assert.Equal("Predicate", predicateType.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, predicateType.TypeArguments[0]));
    }

    [Fact]
    public void TupleElements_AreSubstitutedFromDefinition()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var tupleDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.ValueTuple`2"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var tuple = Assert.IsAssignableFrom<INamedTypeSymbol>(tupleDefinition.Construct(intType, stringType));

        Assert.True(SymbolEqualityComparer.Default.Equals(tuple, tuple.UnderlyingTupleType));

        var elements = tuple.TupleElements;
        Assert.Equal(2, elements.Length);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, elements[0].Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, elements[1].Type));
    }
}
