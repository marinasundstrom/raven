extern alias Seed;

using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SymbolContractParityTests
{
    public static IEnumerable<object[]> Contracts()
    {
        yield return ["Symbol"];
        yield return ["NamespaceOrTypeSymbol"];
        yield return ["NamespaceSymbol"];
        yield return ["LambdaSymbol"];
        yield return ["LabelSymbol"];
        yield return ["AliasSymbol"];
        yield return ["MethodSymbol"];
        yield return ["MacroSymbol"];
        yield return ["MacroDeclarationSymbol"];
        yield return ["ParameterSymbol"];
        yield return ["FieldSymbol"];
        yield return ["PropertySymbol"];
        yield return ["EventSymbol"];
        yield return ["TypeSymbol"];
        yield return ["IntersectionTypeSymbol"];
        yield return ["StructuralUnionTypeSymbol"];
        yield return ["FunctionReturnTypeSymbol"];
        yield return ["NamedTypeSymbol"];
        yield return ["ArrayTypeSymbol"];
        yield return ["PointerTypeSymbol"];
        yield return ["AddressTypeSymbol"];
        yield return ["TupleTypeSymbol"];
        yield return ["UnionSymbol"];
        yield return ["UnionCaseTypeSymbol"];
        yield return ["TypeParameterSymbol"];
        yield return ["LocalSymbol"];
        yield return ["ErrorSymbol"];
        yield return ["ErrorTypeSymbol"];
        yield return ["AssemblySymbol"];
        yield return ["ModuleSymbol"];
    }

    [Theory]
    [MemberData(nameof(Contracts))]
    public void PublicSymbolInterfacePreservesShapeAndOwnsSignatureTypes(string name)
    {
        var port = typeof(TypedConstant).Assembly;
        var seed = typeof(Seed::Raven.CodeAnalysis.Symbol).Assembly;
        var expected = seed.GetType("Raven.CodeAnalysis." + name, true)!;
        var actual = port.GetType(expected.FullName!, true)!;
        Assert.True(actual.IsInterface);
        Assert.True(actual.IsPublic);
        Assert.Equal(expected.GetInterfaces().Select(Shape).Order(), actual.GetInterfaces().Select(Shape).Order());
        const BindingFlags flags = BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;
        Assert.Equal(expected.GetMethods(flags).Select(MethodShape).Order(), actual.GetMethods(flags).Select(MethodShape).Order());
        foreach (var inherited in actual.GetInterfaces()) AssertPortOwnership(inherited, port);
        foreach (var method in actual.GetMethods(flags))
        {
            AssertPortOwnership(method.ReturnType, port);
            foreach (var parameter in method.GetParameters()) AssertPortOwnership(parameter.ParameterType, port);
        }
    }

    [Fact]
    public void ComparerAndVisitorsBelongToPort()
    {
        var port = typeof(TypedConstant).Assembly;
        Assert.Same(port, typeof(SymbolEqualityComparer).Assembly);
        Assert.Same(port, typeof(SymbolVisitor).Assembly);
        Assert.Same(port, typeof(SymbolVisitor<>).Assembly);
        Assert.Same(port, typeof(SymbolEqualityComparer).GetMethod("Equals", [typeof(Symbol), typeof(Symbol)])!.GetParameters()[0].ParameterType.Assembly);
    }

    private static string MethodShape(MethodInfo method) => method.Name + "`" + method.GetGenericArguments().Length +
        ":" + method.IsStatic + ":" + method.IsAbstract + ":" + Shape(method.ReturnType) + "(" +
        string.Join(",", method.GetParameters().Select(parameter => Shape(parameter.ParameterType) + ":" + parameter.IsOut)) + ")";

    private static string Shape(Type type)
    {
        if (type.IsGenericParameter) return "!" + type.GenericParameterPosition;
        if (type.IsByRef) return Shape(type.GetElementType()!) + "&";
        if (type.IsArray) return Shape(type.GetElementType()!) + "[" + type.GetArrayRank() + "]";
        if (type.IsGenericType) return type.GetGenericTypeDefinition().FullName + "<" + string.Join(",", type.GetGenericArguments().Select(Shape)) + ">";
        return type.FullName!;
    }

    private static void AssertPortOwnership(Type type, Assembly port)
    {
        if (type.HasElementType) { AssertPortOwnership(type.GetElementType()!, port); return; }
        if (type.IsGenericParameter) return;
        if (type.Namespace?.StartsWith("Raven.CodeAnalysis", StringComparison.Ordinal) == true)
            Assert.Same(port, type.Assembly);
        if (type.IsGenericType)
            foreach (var argument in type.GetGenericArguments()) AssertPortOwnership(argument, port);
    }
}
