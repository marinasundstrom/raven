using System.Linq;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConstructedMethodSymbolTests
{
    [Fact]
    public void ConstructedMethod_ObjectEqualityUsesConstructedIdentity()
    {
        const string source = """
class Factory {
    public static func Identity<T>(value: T) -> T => value
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "constructed-method-object-equality",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(syntaxTree);
        var factory = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        var definition = Assert.Single(factory.GetMembers("Identity").OfType<IMethodSymbol>());
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var firstInt = definition.Construct(intType);
        var secondInt = definition.Construct(intType);
        var constructedString = definition.Construct(stringType);

        Assert.True(((object)firstInt).Equals(firstInt));
        Assert.True(((object)firstInt).Equals(secondInt));
        Assert.Equal(((object)firstInt).GetHashCode(), ((object)secondInt).GetHashCode());
        Assert.False(((object)firstInt).Equals(constructedString));
    }

    [Fact]
    public void RepeatedConstruction_PreservesContainingSubstitutionAndConstraintOwnership()
    {
        const string source = """
class Container<TBase> {
    public static func Coerce<TDerived>(value: TDerived) -> TBase
        where TDerived: TBase
        => throw System.Exception()
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "repeated-constructed-method",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(syntaxTree);
        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var firstContainer = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(objectType));
        var secondContainer = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(objectType));
        var firstOpenMethod = Assert.Single(firstContainer.GetMembers("Coerce").OfType<IMethodSymbol>());
        var secondOpenMethod = Assert.Single(secondContainer.GetMembers("Coerce").OfType<IMethodSymbol>());
        var firstMethod = firstOpenMethod.Construct(stringType);
        var secondMethod = secondOpenMethod.Construct(stringType);
        var comparer = SymbolEqualityComparer.Default;

        Assert.NotSame(firstContainer, secondContainer);
        Assert.True(comparer.Equals(firstContainer, secondContainer));
        Assert.True(comparer.Equals(firstOpenMethod, secondOpenMethod));
        Assert.True(comparer.Equals(firstMethod, secondMethod));
        Assert.Equal(comparer.GetHashCode(firstMethod), comparer.GetHashCode(secondMethod));
        Assert.True(((object)firstMethod).Equals(secondMethod));

        var firstTypeParameter = Assert.Single(firstOpenMethod.TypeParameters);
        var secondTypeParameter = Assert.Single(secondOpenMethod.TypeParameters);
        Assert.Same(firstOpenMethod, firstTypeParameter.DeclaringMethodParameterOwner);
        Assert.Same(secondOpenMethod, secondTypeParameter.DeclaringMethodParameterOwner);
        Assert.Equal(SpecialType.System_Object, Assert.Single(firstTypeParameter.ConstraintTypes).SpecialType);
        Assert.Equal(SpecialType.System_Object, Assert.Single(secondTypeParameter.ConstraintTypes).SpecialType);
        Assert.Equal(SpecialType.System_Object, firstMethod.ReturnType.SpecialType);
        Assert.Equal(SpecialType.System_String, Assert.Single(firstMethod.Parameters).Type.SpecialType);
    }

    [Fact]
    public void ConstructedMethod_SubstitutesContainingTypeInsideGenericConstraint()
    {
        const string source = """
import System.Collections.Generic.*

class Container<T> {
    public func Copy<U>(value: U) -> U
        where U: IEnumerable<T>
        => value
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "constructed-method-nested-constraint",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(syntaxTree);
        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var container = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(stringType));
        var copy = Assert.Single(container.GetMembers("Copy").OfType<IMethodSymbol>());
        var typeParameter = Assert.Single(copy.TypeParameters);
        var constraint = Assert.IsAssignableFrom<INamedTypeSymbol>(Assert.Single(typeParameter.ConstraintTypes));

        Assert.Same(copy, typeParameter.DeclaringMethodParameterOwner);
        Assert.Equal("IEnumerable", constraint.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, Assert.Single(constraint.TypeArguments)));
    }

    [Fact]
    public void ConstructedMethod_SubstitutesArrayElementType()
    {
        var source = """
class Factory {
    public static func Wrap<T>(values: T[]) -> T[] => values
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "constructed-method-array-substitution",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var factorySyntax = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single();
        var model = compilation.GetSemanticModel(syntaxTree);
        var factory = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(factorySyntax));
        var wrap = Assert.Single(factory.GetMembers("Wrap").OfType<IMethodSymbol>());
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var constructedWrap = wrap.Construct(intType);
        var parameterArray = Assert.IsAssignableFrom<IArrayTypeSymbol>(Assert.Single(constructedWrap.Parameters).Type);
        var returnArray = Assert.IsAssignableFrom<IArrayTypeSymbol>(constructedWrap.ReturnType);

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameterArray.ElementType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, returnArray.ElementType));
        Assert.Equal(SpecialType.System_Array, parameterArray.BaseType?.SpecialType);
        Assert.Equal(SpecialType.System_Array, returnArray.BaseType?.SpecialType);
    }

    [Fact]
    public void ConstructedMethod_UsesConstructedContainerForDisplay()
    {
        var source = """
import System.*

class Outer<T>
{
    public static func Wrap<U>(value: T, other: U) -> T => value;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-container",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var outerSyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Outer");
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(outerSyntax));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(stringType));

        var wrapDefinition = Assert.Single(
            constructedOuter
                .GetMembers("Wrap")
                .OfType<IMethodSymbol>());

        var constructedWrap = Assert.IsAssignableFrom<IMethodSymbol>(wrapDefinition.Construct(intType));

        Assert.Same(constructedWrap, constructedWrap.UnderlyingSymbol);
        Assert.Same(constructedOuter, constructedWrap.ContainingType);
        Assert.Equal("Outer`1", constructedWrap.ContainingType?.ToFullyQualifiedMetadataName());
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, Assert.Single(constructedWrap.TypeArguments)));

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, constructedWrap.ReturnType));
        Assert.Collection(
            constructedWrap.Parameters,
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(stringType, parameter.Type)),
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type)));

        var display = constructedWrap.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);
        Assert.Contains("Wrap<int>", display);
        Assert.Contains("string", display);
        Assert.Contains("int", display);
        Assert.EndsWith("-> string", display, StringComparison.Ordinal);
    }

    [Fact]
    public void ConstructedMethod_ToStringAndDebuggerDisplay_UseDisplayStrings()
    {
        var source = """
import System.*

class Outer<T>
{
    public static func Wrap<U>(value: T, other: U) -> T => value;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-debugger-display",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var outerSyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Outer");
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(outerSyntax));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(stringType));

        var wrapDefinition = Assert.Single(
            constructedOuter
                .GetMembers("Wrap")
                .OfType<IMethodSymbol>());

        var constructedWrap = Assert.IsAssignableFrom<IMethodSymbol>(wrapDefinition.Construct(intType));

        var errorDisplay = constructedWrap.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);
        Assert.Contains("Wrap<int>", errorDisplay);
        Assert.Contains("-> string", errorDisplay, StringComparison.Ordinal);
        Assert.Contains("Wrap<int>", constructedWrap.ToString(), StringComparison.Ordinal);

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
        Assert.Contains("Find", errorDisplay, StringComparison.Ordinal);
        Assert.Contains("Predicate<string>", errorDisplay, StringComparison.Ordinal);
        Assert.Contains("-> string", errorDisplay, StringComparison.Ordinal);
        Assert.Contains("Find", find.ToString(), StringComparison.Ordinal);

        var fullyQualified = find.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Contains("System.Collections.Generic.List<string>", fullyQualified, StringComparison.Ordinal);
        Assert.Contains("Find", fullyQualified, StringComparison.Ordinal);
        Assert.Contains("System.Predicate<string>", fullyQualified, StringComparison.Ordinal);
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
    public static func Create<U>(value: U) -> Outer<U>.Inner {
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
    public static func Create<U, V>(value: U, other: V) -> Outer<U>.Inner<V> {
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

    [Fact]
    public void ConstructedMethod_CombinedWithConstructedNamedType_PreservesNestedSubstitution()
    {
        var source = """
class Outer<T> {
    public class Node<U> {
        public init(value: T, other: U) { }
    }

    public static func Make<U>(value: T, other: U) -> Outer<T>.Node<U> {
        return Outer<T>.Node<U>(value, other)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-method-combined-nested-substitution",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var outerSyntax = root.DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.Text == "Outer");
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(outerSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var outerInt = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));
        var makeDefinition = Assert.Single(outerInt.GetMembers("Make").OfType<IMethodSymbol>());
        var makeDefinitionReturnNode = Assert.IsAssignableFrom<INamedTypeSymbol>(makeDefinition.ReturnType);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(outerInt, makeDefinitionReturnNode.ContainingType),
            $"Definition return type should already be anchored to containing type. Expected '{outerInt.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}' but got '{makeDefinitionReturnNode.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}'. Return type: '{makeDefinitionReturnNode.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}'");
        var makeIntString = Assert.IsAssignableFrom<IMethodSymbol>(makeDefinition.Construct(stringType));

        Assert.True(SymbolEqualityComparer.Default.Equals(outerInt, makeIntString.ContainingType));

        var returnNode = Assert.IsAssignableFrom<INamedTypeSymbol>(makeIntString.ReturnType);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(outerInt, returnNode.ContainingType),
            $"Expected containing type '{outerInt.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}' but got '{returnNode.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}'. Return type: '{returnNode.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}'");
        Assert.Collection(
            returnNode.TypeArguments,
            argument => Assert.True(SymbolEqualityComparer.Default.Equals(stringType, argument)));

        Assert.Collection(
            makeIntString.Parameters,
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type)),
            parameter => Assert.True(SymbolEqualityComparer.Default.Equals(stringType, parameter.Type)));
    }

    [Fact]
    public void ConstructedMethod_OnNestedGenericType_PreservesEverySubstitutionLayer()
    {
        const string source = """
class Outer<T> {
    public class Inner<U> {
        public func Combine<V>(outer: T, inner: U, method: V) -> V
            where V: U
            => method
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "constructed-method-nested-container",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(syntaxTree);
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First()));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var outerInt = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));
        var innerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(outerInt.LookupType("Inner"));
        var innerObject = Assert.IsAssignableFrom<INamedTypeSymbol>(innerDefinition.Construct(objectType));
        var combineDefinition = Assert.Single(innerObject.GetMembers("Combine").OfType<IMethodSymbol>());
        var combineString = combineDefinition.Construct(stringType);

        Assert.True(SymbolEqualityComparer.Default.Equals(innerObject, combineString.ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(outerInt, innerObject.ContainingType));
        Assert.Collection(
            combineString.Parameters,
            parameter => Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType),
            parameter => Assert.Equal(SpecialType.System_Object, parameter.Type.SpecialType),
            parameter => Assert.Equal(SpecialType.System_String, parameter.Type.SpecialType));
        Assert.Equal(SpecialType.System_String, combineString.ReturnType.SpecialType);

        var methodTypeParameter = Assert.Single(combineDefinition.TypeParameters);
        Assert.Same(combineDefinition, methodTypeParameter.DeclaringMethodParameterOwner);
        Assert.Equal(SpecialType.System_Object, Assert.Single(methodTypeParameter.ConstraintTypes).SpecialType);

        using var image = new MemoryStream();
        var emitResult = compilation.Emit(image);
        Assert.True(emitResult.Success, string.Join(System.Environment.NewLine, emitResult.Diagnostics));
        var metadataCompilation = Compilation.Create(
                "constructed-method-nested-container-consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([
                .. TestMetadataReferences.Default,
                MetadataReference.CreateFromImage(image.ToArray()),
            ]);
        var metadataOuterDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            metadataCompilation.GetTypeByMetadataName("Outer`1"));
        var metadataOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(
            metadataOuterDefinition.Construct(metadataCompilation.GetSpecialType(SpecialType.System_Int32)));
        var metadataInnerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(metadataOuter.LookupType("Inner"));
        var metadataInner = Assert.IsAssignableFrom<INamedTypeSymbol>(
            metadataInnerDefinition.Construct(metadataCompilation.GetSpecialType(SpecialType.System_Object)));
        var metadataCombineDefinition = Assert.Single(metadataInner.GetMembers("Combine").OfType<IMethodSymbol>());
        var metadataCombine = metadataCombineDefinition.Construct(
            metadataCompilation.GetSpecialType(SpecialType.System_String));

        var comparer = SymbolEqualityComparer.Default;
        Assert.True(comparer.Equals(outerInt, metadataOuter), "Constructed outer types differ");
        var sourceInnerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(innerDefinition.OriginalDefinition);
        var peInnerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(metadataInnerDefinition.OriginalDefinition);
        Assert.Equal(sourceInnerDefinition.Name, peInnerDefinition.Name);
        Assert.Equal(sourceInnerDefinition.Arity, peInnerDefinition.Arity);
        Assert.True(comparer.Equals(sourceInnerDefinition.ContainingType, peInnerDefinition.ContainingType),
            $"Nested containing types differ: {sourceInnerDefinition.ContainingType} != {peInnerDefinition.ContainingType}");
        Assert.True(comparer.Equals(sourceInnerDefinition, peInnerDefinition),
            "Nested type definitions differ");
        Assert.True(comparer.Equals(innerObject, metadataInner), "Constructed nested types differ");
        Assert.True(comparer.Equals(combineString, metadataCombine), "Constructed nested methods differ");
        Assert.Equal(
            comparer.GetHashCode(combineString),
            comparer.GetHashCode(metadataCombine));
    }

}
