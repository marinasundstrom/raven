using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ObjectCreationBindingTests
{
    [Fact]
    public void NestedTypeCreation_UsesSubstitutedConstructor()
    {
        const string source = """
class Foo<T>
{
    public class Bar
    {
        public Bar() {}
    }
}

let value = Foo<int>.Bar();
""";

        VerifyConstructorBinding(source, creationIndex: 0);
    }

    [Fact]
    public void DoublyNestedCreation_UsesSubstitutedConstructor()
    {
        const string source = """
class Outer<T>
{
    public class Inner<U>
    {
        public Inner() {}
    }
}

let value = Outer<int>.Inner<string>();
""";

        VerifyConstructorBinding(source, creationIndex: 0);
    }

    [Fact]
    public void NestedGenericTypeMemberAccess_BindsAsConstructedTypeExpression()
    {
        const string source = """
class Outer<T>
{
    public class Inner<U>
    {
        public Inner() {}
    }
}

let value = Outer<int>.Inner<string>();
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "object-creation-member-access-type-binding",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();
        var memberAccess = Assert.IsType<MemberAccessExpressionSyntax>(invocation.Expression);

        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(memberAccess));
        var bound = Assert.IsType<BoundTypeExpression>(binder.GetOrBind(memberAccess));

        var int32 = compilation.GetSpecialType(SpecialType.System_Int32);
        var @string = compilation.GetSpecialType(SpecialType.System_String);
        var outerDef = compilation.GetTypeByMetadataName("Outer`1")!;
        var outerInt = outerDef.Construct(int32);
        var innerInOuterInt = outerInt.GetMembers("Inner").OfType<INamedTypeSymbol>().Single();
        var expected = innerInOuterInt.Construct(@string);

        Assert.Equal(expected, bound.Type, SymbolEqualityComparer.Default);
    }

    private static void VerifyConstructorBinding(string source, int creationIndex)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "object-creation-constructor-substitution",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .ElementAt(creationIndex);

        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(invocation));
        var bound = Assert.IsType<BoundObjectCreationExpression>(binder.GetOrBind(invocation));

        var constructedType = Assert.IsType<ConstructedNamedTypeSymbol>(bound.Type);
        var substitutedConstructor = Assert.IsType<SubstitutedMethodSymbol>(bound.Constructor);

        Assert.True(SymbolEqualityComparer.Default.Equals(constructedType, substitutedConstructor.ContainingType));

        Assert.All(constructedType.TypeArguments, argument => Assert.False(argument is ITypeParameterSymbol));
        Assert.All(constructedType.GetAllTypeArguments(), argument => Assert.False(argument is ITypeParameterSymbol));
    }
}
