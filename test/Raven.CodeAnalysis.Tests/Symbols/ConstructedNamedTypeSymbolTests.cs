using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ConstructedNamedTypeSymbolTests : CompilationTestBase
{
    [Fact]
    public void NestedTypeConstruction_SubstitutesOuterTypeParameters()
    {
        const string source = """
val a = Foo<int>.Bar()

class Foo<T> {
    public class Bar {
        val value: T
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var fooDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Foo`1"));
        var barDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(fooDefinition.LookupType("Bar"));

        var fooInt = Assert.IsAssignableFrom<INamedTypeSymbol>(fooDefinition.Construct(intType));
        var barInFooInt = Assert.IsAssignableFrom<INamedTypeSymbol>(fooInt.LookupType("Bar"));
        var valueField = Assert.Single(barInFooInt.GetMembers("value").OfType<IFieldSymbol>());

        Assert.Equal(barDefinition, type.OriginalDefinition, SymbolEqualityComparer.Default);
        Assert.True(SymbolEqualityComparer.Default.Equals(barDefinition, barInFooInt.OriginalDefinition));
        Assert.True(SymbolEqualityComparer.Default.Equals(fooInt, barInFooInt.ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, valueField.Type));
        Assert.Equal(barInFooInt, type, SymbolEqualityComparer.Default);
    }

    [Fact]
    public void NestedGenericTypeConstruction_SubstitutesOuterAndInnerTypeParameters()
    {
        const string source = """
val b = Outer<int>.Inner<string>()

class Outer<A> {
    public class Inner<B> {
        val value: A
        val b: B
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Outer`1"));
        var outerInt = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));

        var innerDefinitionInOuterInt = Assert.IsAssignableFrom<INamedTypeSymbol>(outerInt.LookupType("Inner"));
        var innerIntString = Assert.IsAssignableFrom<INamedTypeSymbol>(innerDefinitionInOuterInt.Construct(stringType));

        var valueField = Assert.Single(innerIntString.GetMembers("value").OfType<IFieldSymbol>());
        var bField = Assert.Single(innerIntString.GetMembers("b").OfType<IFieldSymbol>());

        Assert.Equal(innerIntString, type, SymbolEqualityComparer.Default);
        Assert.True(SymbolEqualityComparer.Default.Equals(outerInt, innerIntString.ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, valueField.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, bField.Type));
    }

    [Fact]
    public void ConstructedMetadataType_AllInterfaces_ForSelfReferentialNumericInterface_DoesNotCrash()
    {
        var compilation = Compilation.Create("constructed-all-interfaces-self-reference", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var interfaceDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Numerics.INumber`1"));

        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(interfaceDefinition.Construct(intType));

        var exception = Record.Exception(() =>
        {
            var allInterfaces = constructed.AllInterfaces;
            Assert.NotEmpty(allInterfaces);
        });

        Assert.Null(exception);
    }

    [Fact]
    public void ConstructedMetadataType_AllInterfaces_SubstitutesConcreteTypeArguments()
    {
        var compilation = Compilation.Create("constructed-all-interfaces-substitution", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var listInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));
        var enumerableInterface = Assert.Single(listInt.AllInterfaces.Where(i => i.MetadataName == "IEnumerable`1"));

        Assert.Single(enumerableInterface.TypeArguments);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, enumerableInterface.TypeArguments[0]));
    }

    [Fact]
    public void ConstructedMetadataType_TaskRunDelegateComparisons_DoNotRecurse()
    {
        var compilation = Compilation.Create(
                "constructed-task-run-symbol-comparison",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var taskType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Threading.Tasks.Task"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var runMethods = taskType.GetMembers("Run")
            .OfType<IMethodSymbol>()
            .Where(m => m.Arity == 1 && m.Parameters.Length == 1)
            .ToArray();

        Assert.NotEmpty(runMethods);

        var delegateTypes = runMethods
            .SelectMany(method =>
            {
                var symbols = new List<INamedTypeSymbol>();
                if (method.Parameters[0].Type is INamedTypeSymbol originalDelegate)
                    symbols.Add(originalDelegate);

                if (method.Construct(intType) is IMethodSymbol constructed
                    && constructed.Parameters[0].Type is INamedTypeSymbol constructedDelegate)
                {
                    symbols.Add(constructedDelegate);
                }

                return symbols;
            })
            .ToArray();

        var exception = Record.Exception(() =>
        {
            for (var i = 0; i < delegateTypes.Length; i++)
            {
                for (var j = 0; j < delegateTypes.Length; j++)
                    _ = SymbolEqualityComparer.Default.Equals(delegateTypes[i], delegateTypes[j]);
            }
        });

        Assert.Null(exception);
    }

    [Fact]
    public void ConstructedMetadataType_AsyncInference_DoesNotStackOverflow()
    {
        const string source = """
import System.Threading.Tasks.*

val value = 42
val result = await Task.Run(async () => {
    await Task.Delay(10)
    return value
})
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "constructed-async-inference-regression",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var exception = Record.Exception(() =>
        {
            var diagnostics = compilation.GetDiagnostics();
            Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));
        });

        Assert.Null(exception);
    }

    [Fact]
    public void SourceNestedType_Substitution_UsesMethodTypeParameterSymbol_NotNameOrdinalOnly()
    {
        const string source = """
class Wrapper<T> {
    public class Node {
        public init(value: T) { }
    }
}

class Program {
    public static CreateNode<T>(value: T) -> Wrapper<T>.Node {
        return Wrapper<T>.Node(value)
    }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var programType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Program"));
        var method = Assert.Single(programType.GetMembers("CreateNode").OfType<IMethodSymbol>());
        var methodTypeParameter = Assert.Single(method.TypeParameters);
        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(method.ReturnType);
        var wrapperType = Assert.IsAssignableFrom<INamedTypeSymbol>(returnType.ContainingType);
        var wrapperArgument = Assert.Single(wrapperType.TypeArguments);

        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, wrapperArgument));
    }

    [Fact]
    public void SourceNestedTypeArgument_SubstitutesInnerConstructedArguments()
    {
        const string source = """
class Box<T> {
}

class Holder<T> {
    val value: T
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var boxType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Box`1"));
        var holderType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Holder`1"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var boxInt = Assert.IsAssignableFrom<INamedTypeSymbol>(boxType.Construct(intType));
        var holderOfBoxInt = Assert.IsAssignableFrom<INamedTypeSymbol>(holderType.Construct(boxInt));
        var valueField = Assert.Single(holderOfBoxInt.GetMembers("value").OfType<IFieldSymbol>());

        var valueType = Assert.IsAssignableFrom<INamedTypeSymbol>(valueField.Type);
        var nestedArgument = Assert.IsAssignableFrom<INamedTypeSymbol>(Assert.Single(valueType.TypeArguments));

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, nestedArgument));
    }

    [Fact]
    public void SourceNestedTypeArgument_SymbolEqualityComparerHashCode_DoesNotRecurse()
    {
        const string source = """
class Box<T> {
}

class Holder<T> {
    val value: T
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var boxType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Box`1"));
        var holderType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Holder`1"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var boxInt = Assert.IsAssignableFrom<INamedTypeSymbol>(boxType.Construct(intType));
        var holderInt = Assert.IsAssignableFrom<INamedTypeSymbol>(holderType.Construct(boxInt));
        var valueType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            Assert.Single(holderInt.GetMembers("value").OfType<IFieldSymbol>()).Type);

        var exception = Record.Exception(() =>
        {
            for (var i = 0; i < 32; i++)
                _ = SymbolEqualityComparer.Default.GetHashCode(valueType);
        });

        Assert.Null(exception);
    }

    [Fact]
    public void ConstructedNamedType_CombinedWithConstructedMethod_KeepsMethodOwnerTypeParameters()
    {
        const string source = """
class Outer<T> {
    public class Node<U> {
        val value: T
        val other: U
    }
}

class Factory {
    public static Create<T, U>(value: T, other: U) -> Outer<T>.Node<U> {
        return Outer<T>.Node<U>()
    }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var factory = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Factory"));
        var create = Assert.Single(factory.GetMembers("Create").OfType<IMethodSymbol>());
        var methodTypeParameters = create.TypeParameters;

        Assert.Equal(2, methodTypeParameters.Length);
        Assert.Equal(TypeParameterOwnerKind.Method, methodTypeParameters[0].OwnerKind);
        Assert.Equal(TypeParameterOwnerKind.Method, methodTypeParameters[1].OwnerKind);

        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(create.ReturnType);
        var containingOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(returnType.ContainingType);

        var outerArgument = Assert.Single(containingOuter.TypeArguments);
        var nodeArgument = Assert.Single(returnType.TypeArguments);

        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameters[0], outerArgument));
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameters[1], nodeArgument));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var createIntString = Assert.IsAssignableFrom<IMethodSymbol>(create.Construct(intType, stringType));
        var constructedReturnType = Assert.IsAssignableFrom<INamedTypeSymbol>(createIntString.ReturnType);
        var constructedOuterType = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedReturnType.ContainingType);

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, Assert.Single(constructedOuterType.TypeArguments)));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, Assert.Single(constructedReturnType.TypeArguments)));
    }
}
