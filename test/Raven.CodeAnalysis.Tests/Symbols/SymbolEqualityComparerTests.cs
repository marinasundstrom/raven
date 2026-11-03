using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SymbolEqualityComparerTests
{
    [Fact]
    public void DefaultComparer_DistinguishesNullability()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = intType.GetNullableType();

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<Symbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.False(dictionary.ContainsKey(nullableInt));
    }

    [Fact]
    public void IgnoringNullabilityComparer_DistinguishesNullableValueTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = intType.GetNullableType();

        var comparer = SymbolEqualityComparer.IgnoringNullability;
        Assert.False(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<Symbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.False(dictionary.ContainsKey(nullableInt));
    }

    [Fact]
    public void IgnoringNullabilityComparer_EquatesNullableReferenceTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var nullableString = stringType.GetNullableType();

        var comparer = SymbolEqualityComparer.IgnoringNullability;
        Assert.True(comparer.Equals(stringType, nullableString));
        Assert.Equal(comparer.GetHashCode(stringType), comparer.GetHashCode(nullableString));

        var dictionary = new Dictionary<Symbol, int>(comparer)
        {
            [stringType] = 1,
        };

        Assert.True(dictionary.ContainsKey(nullableString));
    }

    [Fact]
    public void Comparer_UnwrapsAliasSymbols()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var alias = AliasSymbolFactory.Create("Text", stringType);

        var comparer = SymbolEqualityComparer.Default;
        Assert.True(comparer.Equals(alias, alias.UnderlyingSymbol));
        Assert.Equal(comparer.GetHashCode(alias.UnderlyingSymbol), comparer.GetHashCode(alias));

        var set = new HashSet<Symbol>(comparer) { alias };
        Assert.Contains(alias.UnderlyingSymbol, set);
    }

    [Fact]
    public void Comparer_DistinguishesMetadataNameDifferences()
    {
        var comparer = SymbolEqualityComparer.Default;
        var explicitImplementation = new StubSymbol(SymbolKind.Method, "M", "IFoo.M");
        var ordinaryMethod = new StubSymbol(SymbolKind.Method, "M", "M");

        Assert.False(comparer.Equals(explicitImplementation, ordinaryMethod));

        var set = new HashSet<Symbol>(comparer) { explicitImplementation };
        Assert.DoesNotContain(ordinaryMethod, set);
    }

    [Fact]
    public void Comparer_DistinguishesConstructedGenerics()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var listOfInt = Assert.IsAssignableFrom<NamedTypeSymbol>(listDefinition.Construct(intType));
        var listOfString = Assert.IsAssignableFrom<NamedTypeSymbol>(listDefinition.Construct(stringType));

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(listOfInt, listOfString));

        var set = new HashSet<Symbol>(comparer) { listOfInt };
        Assert.DoesNotContain(listOfString, set);
    }

    [Fact]
    public void Comparer_EquatesConstructedGenericsWithSameArguments()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var first = Assert.IsAssignableFrom<NamedTypeSymbol>(listDefinition.Construct(intType));
        var second = Assert.IsAssignableFrom<NamedTypeSymbol>(listDefinition.Construct(intType));

        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(first, second));
        Assert.Equal(comparer.GetHashCode(first), comparer.GetHashCode(second));

        var set = new HashSet<Symbol>(comparer) { first };
        Assert.Contains(second, set);
    }

    [Fact]
    public void Comparer_EquatesConstructedSourceAndEmittedMetadataGenericMembers()
    {
        const string source = """
            public class Box<T> {
                private var current: T
                public init(value: T) { self.Value = value }
                public var Value: T
                public var self[index: T]: T {
                    get => current
                    set => current = value
                }
                public event Changed: System.Action<T>?
                public func Select<U>(value: U) -> T => throw System.Exception()
            }
            """;
        var sourceTree = SyntaxTree.ParseText(source);
        var sourceCompilation = Compilation.Create(
            "GenericBoundary",
            [sourceTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var sourceModel = sourceCompilation.GetSemanticModel(sourceTree);
        var sourceDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            sourceModel.GetDeclaredSymbol(sourceTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        var sourceString = sourceCompilation.GetSpecialType(SpecialType.System_String);
        var sourceInt = sourceCompilation.GetSpecialType(SpecialType.System_Int32);
        var sourceBox = Assert.IsAssignableFrom<NamedTypeSymbol>(sourceDefinition.Construct(sourceString));
        var sourceConstructor = Assert.Single(sourceBox.InstanceConstructors, constructor => constructor.Parameters.Length == 1);
        var sourceProperty = Assert.Single(sourceBox.GetMembers("Value").OfType<PropertySymbol>());
        var sourceIndexer = Assert.Single(sourceBox.GetMembers().OfType<PropertySymbol>(), property => property.IsIndexer);
        var sourceEvent = Assert.Single(sourceBox.GetMembers("Changed").OfType<EventSymbol>());
        var sourceMethod = Assert.Single(sourceBox.GetMembers("Select").OfType<MethodSymbol>()).Construct(sourceInt);

        using var image = new MemoryStream();
        var emitResult = sourceCompilation.Emit(image);
        Assert.True(emitResult.Success, string.Join(System.Environment.NewLine, emitResult.Diagnostics));

        var metadataCompilation = Compilation.Create(
                "GenericBoundaryConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([
                .. TestMetadataReferences.Default,
                MetadataReference.CreateFromImage(image.ToArray()),
            ]);
        var metadataDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            metadataCompilation.GetTypeByMetadataName("Box`1"));
        var metadataString = metadataCompilation.GetSpecialType(SpecialType.System_String);
        var metadataInt = metadataCompilation.GetSpecialType(SpecialType.System_Int32);
        var metadataBox = Assert.IsAssignableFrom<NamedTypeSymbol>(metadataDefinition.Construct(metadataString));
        var metadataConstructor = Assert.Single(metadataBox.InstanceConstructors, constructor => constructor.Parameters.Length == 1);
        var metadataProperty = Assert.Single(metadataBox.GetMembers("Value").OfType<PropertySymbol>());
        var metadataIndexer = Assert.Single(metadataBox.GetMembers().OfType<PropertySymbol>(), property => property.IsIndexer);
        var metadataEvent = Assert.Single(metadataBox.GetMembers("Changed").OfType<EventSymbol>());
        var metadataMethod = Assert.Single(metadataBox.GetMembers("Select").OfType<MethodSymbol>()).Construct(metadataInt);
        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(sourceCompilation.Assembly, metadataDefinition.ContainingAssembly),
            $"Source assembly '{sourceCompilation.Assembly.Name}' != metadata assembly '{metadataDefinition.ContainingAssembly?.Name}'");
        Assert.True(comparer.Equals(sourceDefinition.ContainingModule, metadataDefinition.ContainingModule),
            $"Source module '{sourceDefinition.ContainingModule?.Name}' != metadata module '{metadataDefinition.ContainingModule?.Name}'");
        Assert.True(comparer.Equals(sourceDefinition.ContainingNamespace, metadataDefinition.ContainingNamespace),
            "Source and metadata containing namespaces differ");
        Assert.True(comparer.Equals(sourceDefinition, metadataDefinition),
            "Source and metadata generic definitions differ");
        Assert.True(comparer.Equals(sourceBox, metadataBox));
        Assert.Equal(comparer.GetHashCode(sourceBox), comparer.GetHashCode(metadataBox));
        Assert.True(comparer.Equals(sourceConstructor, metadataConstructor));
        Assert.Equal(comparer.GetHashCode(sourceConstructor), comparer.GetHashCode(metadataConstructor));
        Assert.True(comparer.Equals(sourceConstructor.Parameters[0], metadataConstructor.Parameters[0]));
        Assert.Same(sourceConstructor, sourceConstructor.Parameters[0].ContainingSymbol);
        Assert.Same(metadataConstructor, metadataConstructor.Parameters[0].ContainingSymbol);
        Assert.True(comparer.Equals(sourceProperty.Type, metadataProperty.Type));
        Assert.True(comparer.Equals(sourceProperty, metadataProperty));
        Assert.Equal(comparer.GetHashCode(sourceProperty), comparer.GetHashCode(metadataProperty));
        Assert.True(comparer.Equals(sourceProperty.GetMethod, metadataProperty.GetMethod));
        Assert.Equal(comparer.GetHashCode(sourceProperty.GetMethod!), comparer.GetHashCode(metadataProperty.GetMethod!));
        Assert.Same(sourceProperty.GetMethod, sourceProperty.GetMethod);
        Assert.Same(sourceProperty.SetMethod, sourceProperty.SetMethod);
        Assert.Same(metadataProperty.GetMethod, metadataProperty.GetMethod);
        Assert.Same(metadataProperty.SetMethod, metadataProperty.SetMethod);
        Assert.Same(sourceProperty, sourceProperty.GetMethod!.AssociatedSymbol);
        Assert.Same(sourceProperty, sourceProperty.SetMethod!.AssociatedSymbol);
        Assert.Same(metadataProperty, metadataProperty.GetMethod!.AssociatedSymbol);
        Assert.Same(metadataProperty, metadataProperty.SetMethod!.AssociatedSymbol);
        Assert.True(comparer.Equals(sourceProperty.OriginalDefinition, metadataProperty.OriginalDefinition));
        AssertIndexer(sourceIndexer, sourceString);
        AssertIndexer(metadataIndexer, metadataString);
        Assert.True(comparer.Equals(sourceIndexer, metadataIndexer));
        Assert.Equal(comparer.GetHashCode(sourceIndexer), comparer.GetHashCode(metadataIndexer));
        AssertEvent(sourceEvent, sourceString);
        AssertEvent(metadataEvent, metadataString);
        Assert.True(comparer.Equals(sourceEvent, metadataEvent));
        Assert.Equal(comparer.GetHashCode(sourceEvent), comparer.GetHashCode(metadataEvent));
        Assert.True(comparer.Equals(sourceMethod.ContainingType, metadataMethod.ContainingType),
            "Constructed method containing types differ");
        Assert.True(comparer.Equals(sourceMethod.ContainingSymbol, metadataMethod.ContainingSymbol),
            "Constructed method containing symbols differ");
        Assert.Equal(sourceMethod.Kind, metadataMethod.Kind);
        Assert.Equal(sourceMethod.Name, metadataMethod.Name);
        Assert.Equal(sourceMethod.MetadataName, metadataMethod.MetadataName);
        Assert.True(comparer.Equals(sourceMethod.ReturnType, metadataMethod.ReturnType),
            $"Constructed method returns differ: {sourceMethod.ReturnType} != {metadataMethod.ReturnType}");
        Assert.True(comparer.Equals(sourceMethod.Parameters[0].Type, metadataMethod.Parameters[0].Type),
            $"Constructed method parameters differ: {sourceMethod.Parameters[0].Type} != {metadataMethod.Parameters[0].Type}");
        Assert.True(comparer.Equals(sourceMethod.Parameters[0], metadataMethod.Parameters[0]),
            "Constructed method parameter symbols differ");
        Assert.Same(sourceMethod, sourceMethod.Parameters[0].ContainingSymbol);
        Assert.Same(metadataMethod, metadataMethod.Parameters[0].ContainingSymbol);
        Assert.True(comparer.Equals(sourceMethod.TypeArguments[0], metadataMethod.TypeArguments[0]),
            "Constructed method type arguments differ");
        Assert.True(comparer.Equals(sourceMethod, metadataMethod));
        Assert.Equal(comparer.GetHashCode(sourceMethod), comparer.GetHashCode(metadataMethod));

        static void AssertIndexer(PropertySymbol indexer, TypeSymbol expectedType)
        {
            Assert.True(SymbolEqualityComparer.Default.Equals(expectedType, indexer.Type));
            var getMethod = Assert.IsAssignableFrom<MethodSymbol>(indexer.GetMethod);
            var setMethod = Assert.IsAssignableFrom<MethodSymbol>(indexer.SetMethod);
            var indexParameter = Assert.Single(getMethod.Parameters);
            Assert.True(SymbolEqualityComparer.Default.Equals(expectedType, indexParameter.Type));
            Assert.Same(getMethod, indexParameter.ContainingSymbol);
            Assert.Same(indexer, getMethod.AssociatedSymbol);
            Assert.Same(indexer, setMethod.AssociatedSymbol);
            Assert.All(setMethod.Parameters, parameter => Assert.Same(setMethod, parameter.ContainingSymbol));
            Assert.True(SymbolEqualityComparer.Default.Equals(expectedType, setMethod.Parameters[^1].Type));
        }

        static void AssertEvent(EventSymbol @event, TypeSymbol expectedArgument)
        {
            var eventType = Assert.IsAssignableFrom<NamedTypeSymbol>(@event.Type.GetNonNullableType());
            Assert.Equal("Action", eventType.Name);
            Assert.True(SymbolEqualityComparer.Default.Equals(expectedArgument, Assert.Single(eventType.TypeArguments)));
            var addMethod = Assert.IsAssignableFrom<MethodSymbol>(@event.AddMethod);
            var removeMethod = Assert.IsAssignableFrom<MethodSymbol>(@event.RemoveMethod);
            Assert.Same(@event, addMethod.AssociatedSymbol);
            Assert.Same(@event, removeMethod.AssociatedSymbol);
            Assert.All(addMethod.Parameters, parameter => Assert.Same(addMethod, parameter.ContainingSymbol));
            Assert.All(removeMethod.Parameters, parameter => Assert.Same(removeMethod, parameter.ContainingSymbol));
            Assert.True(SymbolEqualityComparer.Default.Equals(@event.Type, Assert.Single(addMethod.Parameters).Type));
            Assert.True(SymbolEqualityComparer.Default.Equals(@event.Type, Assert.Single(removeMethod.Parameters).Type));
        }
    }

    [Fact]
    public void Comparer_DistinguishesParametersWithSameType()
    {
        var comparer = SymbolEqualityComparer.Default;
        var containing = new StubSymbol(SymbolKind.Method, "Invoke", "Invoke");
        var parameterType = new StubTypeSymbol(TypeKind.Struct);

        var first = new StubParameterSymbol("a", containing, parameterType);
        var second = new StubParameterSymbol("b", containing, parameterType);

        Assert.False(comparer.Equals(first, second));

        var set = new HashSet<Symbol>(comparer) { first };
        Assert.DoesNotContain(second, set);
    }

    [Fact]
    public void Comparer_PreservesEqualityAcrossCompilations()
    {
        const string source = """
class C {
    class Nested {}

    func M() -> unit {
        let number = 42;
    }
}
""";

        var tree1 = SyntaxTree.ParseText(source);
        var tree2 = SyntaxTree.ParseText(source);

        var compilation1 = Compilation.Create(
            "test",
            [tree1],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var compilation2 = Compilation.Create(
            "test",
            [tree2],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model1 = compilation1.GetSemanticModel(tree1);
        var model2 = compilation2.GetSemanticModel(tree2);

        var root1 = tree1.GetRoot();
        var root2 = tree2.GetRoot();

        var classSyntax1 = Assert.IsAssignableFrom<ClassDeclarationSyntax>(Assert.Single(root1.Members));
        var classSyntax2 = Assert.IsAssignableFrom<ClassDeclarationSyntax>(Assert.Single(root2.Members));

        var class1 = Assert.IsAssignableFrom<NamedTypeSymbol>(model1.GetDeclaredSymbol(classSyntax1));
        var class2 = Assert.IsAssignableFrom<NamedTypeSymbol>(model2.GetDeclaredSymbol(classSyntax2));

        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(compilation1.Assembly, compilation2.Assembly));
        Assert.True(comparer.Equals(compilation1.GlobalNamespace, compilation2.GlobalNamespace));
        Assert.True(comparer.Equals(compilation1.Assembly.Modules.Single(), compilation2.Assembly.Modules.Single()));

        Assert.True(comparer.Equals(class1, class2));
        Assert.Equal(comparer.GetHashCode(class1), comparer.GetHashCode(class2));

        var classSet = new HashSet<Symbol>(comparer) { class1 };
        Assert.Contains(class2, classSet);

        var nested1 = Assert.IsAssignableFrom<NamedTypeSymbol>(class1.GetMembers("Nested").Single());
        var nested2 = Assert.IsAssignableFrom<NamedTypeSymbol>(class2.GetMembers("Nested").Single());

        Assert.True(comparer.Equals(nested1, nested2));
        Assert.Equal(comparer.GetHashCode(nested1), comparer.GetHashCode(nested2));

        var nestedSet = new HashSet<Symbol>(comparer) { nested1 };
        Assert.Contains(nested2, nestedSet);

        var method1 = Assert.IsAssignableFrom<MethodSymbol>(class1.GetMembers("M").Single());
        var method2 = Assert.IsAssignableFrom<MethodSymbol>(class2.GetMembers("M").Single());

        Assert.True(comparer.Equals(method1, method2));
        Assert.Equal(comparer.GetHashCode(method1), comparer.GetHashCode(method2));

        var methodSet = new HashSet<Symbol>(comparer) { method1 };
        Assert.Contains(method2, methodSet);

        var string1 = compilation1.GetSpecialType(SpecialType.System_String);
        var string2 = compilation2.GetSpecialType(SpecialType.System_String);

        Assert.True(comparer.Equals(string1, string2));
        Assert.Equal(comparer.GetHashCode(string1), comparer.GetHashCode(string2));

        var metadataSet = new HashSet<Symbol>(comparer) { string1 };
        Assert.Contains(string2, metadataSet);

        var declarator1 = root1.DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
        var declarator2 = root2.DescendantNodes().OfType<VariableDeclaratorSyntax>().First();

        var local1 = Assert.IsAssignableFrom<LocalSymbol>(model1.GetDeclaredSymbol(declarator1));
        var local2 = Assert.IsAssignableFrom<LocalSymbol>(model2.GetDeclaredSymbol(declarator2));

        Assert.True(comparer.Equals(local1, local2));
        Assert.Equal(comparer.GetHashCode(local1), comparer.GetHashCode(local2));

        var localSet = new HashSet<Symbol>(comparer) { local1 };
        Assert.Contains(local2, localSet);
    }

    [Fact]
    public void Comparer_DistinguishesFieldTypesAcrossCompilations()
    {
        const string template = """
class Sample {{
    field value: {0} = {1}
}}
""";

        var tree1 = SyntaxTree.ParseText(string.Format(template, "int", "0"));
        var tree2 = SyntaxTree.ParseText(string.Format(template, "string", "\"\""));

        var compilation1 = Compilation.Create(
            "test",
            [tree1],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var compilation2 = Compilation.Create(
            "test",
            [tree2],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model1 = compilation1.GetSemanticModel(tree1);
        var model2 = compilation2.GetSemanticModel(tree2);

        var root1 = tree1.GetRoot();
        var root2 = tree2.GetRoot();

        var fieldSyntax1 = root1.DescendantNodes().OfType<FieldDeclarationSyntax>().Single();
        var fieldSyntax2 = root2.DescendantNodes().OfType<FieldDeclarationSyntax>().Single();

        var declarator1 = fieldSyntax1.Declaration.Declarators.Single();
        var declarator2 = fieldSyntax2.Declaration.Declarators.Single();

        var field1 = Assert.IsAssignableFrom<FieldSymbol>(model1.GetDeclaredSymbol(declarator1));
        var field2 = Assert.IsAssignableFrom<FieldSymbol>(model2.GetDeclaredSymbol(declarator2));

        var comparer = SymbolEqualityComparer.Default;

        Assert.False(comparer.Equals(field1, field2));
        Assert.NotEqual(comparer.GetHashCode(field1), comparer.GetHashCode(field2));

        var set = new HashSet<Symbol>(comparer) { field1 };
        Assert.DoesNotContain(field2, set);
    }

    [Fact]
    public void Comparer_DistinguishesPropertyTypesAcrossCompilations()
    {
        const string template = """
class Sample {{
    val Value: {0} {{ get; }}
}}
""";

        var tree1 = SyntaxTree.ParseText(string.Format(template, "int"));
        var tree2 = SyntaxTree.ParseText(string.Format(template, "string"));

        var compilation1 = Compilation.Create(
            "test",
            [tree1],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var compilation2 = Compilation.Create(
            "test",
            [tree2],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model1 = compilation1.GetSemanticModel(tree1);
        var model2 = compilation2.GetSemanticModel(tree2);

        var propertySyntax1 = tree1.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var propertySyntax2 = tree2.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var property1 = Assert.IsAssignableFrom<PropertySymbol>(model1.GetDeclaredSymbol(propertySyntax1));
        var property2 = Assert.IsAssignableFrom<PropertySymbol>(model2.GetDeclaredSymbol(propertySyntax2));

        var comparer = SymbolEqualityComparer.Default;

        Assert.False(comparer.Equals(property1, property2));
        Assert.NotEqual(comparer.GetHashCode(property1), comparer.GetHashCode(property2));

        var set = new HashSet<Symbol>(comparer) { property1 };
        Assert.DoesNotContain(property2, set);
    }

    [Fact]
    public void Comparer_EquatesOpenGenericDefinitions()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var openInstance = Assert.IsAssignableFrom<NamedTypeSymbol>(
            listDefinition.Construct(listDefinition.TypeParameters.Cast<TypeSymbol>().ToArray()));

        var comparer = SymbolEqualityComparer.Default;

        Assert.True(comparer.Equals(listDefinition, openInstance));
        Assert.Equal(comparer.GetHashCode(listDefinition), comparer.GetHashCode(openInstance));
    }

    [Fact]
    public void Comparer_DistinguishesArrayElementTypes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var intArray = compilation.CreateArrayTypeSymbol(intType);
        var stringArray = compilation.CreateArrayTypeSymbol(stringType);

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(intArray, stringArray));

        var set = new HashSet<Symbol>(comparer) { intArray };
        Assert.DoesNotContain(stringArray, set);
    }

    [Fact]
    public void Comparer_DistinguishesFixedArraySizes()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var fixedThree = compilation.CreateArrayTypeSymbol(intType, fixedLength: 3);
        var fixedFour = compilation.CreateArrayTypeSymbol(intType, fixedLength: 4);
        var open = compilation.CreateArrayTypeSymbol(intType);

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(fixedThree, fixedFour));
        Assert.False(comparer.Equals(fixedThree, open));
    }

    [Fact]
    public void Comparer_DistinguishesShadowedLocalsWithSameName()
    {
        const string source = """
import System.Collections.Generic.*

func Main() {
    let values = List<int>()

    for let x in values {
        System.Console.WriteLine(x)
    }

    for let x in values {
        System.Console.WriteLine(x)
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);

        foreach (var forStatement in tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>())
            _ = model.GetBoundNode(forStatement);

        var xArguments = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Where(static invocation => invocation.Expression.ToString() == "System.Console.WriteLine")
            .Select(static invocation => invocation.ArgumentList.Arguments.Single().Expression)
            .OfType<IdentifierNameSyntax>()
            .ToArray();

        Assert.Equal(2, xArguments.Length);

        var firstLoopX = Assert.IsType<BoundLocalAccess>(model.GetBoundNode(xArguments[0]));
        var secondLoopX = Assert.IsType<BoundLocalAccess>(model.GetBoundNode(xArguments[1]));

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(firstLoopX.Local, secondLoopX.Local));
    }

    [Fact]
    public void Comparer_RecognizesMetadataDefinitions()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var constructedFrom = Assert.IsAssignableFrom<NamedTypeSymbol>(listDefinition.ConstructedFrom);
        var originalDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(listDefinition.OriginalDefinition);

        Assert.Same(listDefinition, constructedFrom);
        Assert.Same(listDefinition, originalDefinition);

        var comparer = SymbolEqualityComparer.Default;
        Assert.True(comparer.Equals(listDefinition, constructedFrom));
        Assert.True(comparer.Equals(listDefinition, originalDefinition));
    }

    [Fact]
    public void Comparer_DistinguishesTypeKinds()
    {
        var comparer = SymbolEqualityComparer.Default;

        var delegateSymbol = new StubTypeSymbol(TypeKind.Delegate);
        var classSymbol = new StubTypeSymbol(TypeKind.Class);

        Assert.False(comparer.Equals(delegateSymbol, classSymbol));

        var set = new HashSet<Symbol>(comparer) { delegateSymbol };
        Assert.DoesNotContain(classSymbol, set);
    }

    [Fact]
    public void Comparer_HandlesCyclicContainingSymbols()
    {
        var comparer = SymbolEqualityComparer.Default;

        var first = new CyclicSymbol(SymbolKind.Method, "Evaluate", "Evaluate");
        first.SetContainingSymbol(first);

        var second = new CyclicSymbol(SymbolKind.Method, "Evaluate", "Evaluate");
        second.SetContainingSymbol(second);

        Assert.True(comparer.Equals(first, second));

        var set = new HashSet<Symbol>(comparer) { first };
        Assert.Contains(second, set);

        var firstOuter = new CyclicSymbol(SymbolKind.Method, "Outer", "Outer");
        var firstInner = new CyclicSymbol(SymbolKind.Method, "Inner", "Inner");
        firstOuter.SetContainingSymbol(firstInner);
        firstInner.SetContainingSymbol(firstOuter);

        var secondOuter = new CyclicSymbol(SymbolKind.Method, "Outer", "Outer");
        var secondInner = new CyclicSymbol(SymbolKind.Method, "Inner", "Inner");
        secondOuter.SetContainingSymbol(secondInner);
        secondInner.SetContainingSymbol(secondOuter);

        Assert.True(comparer.Equals(firstOuter, secondOuter));

        set.Add(firstOuter);
        Assert.Contains(secondOuter, set);
    }

    private class StubSymbol : Symbol
    {
        public StubSymbol(
            SymbolKind kind,
            string name,
            string metadataName,
            Symbol? containingSymbol = null,
            NamedTypeSymbol? containingType = null,
            NamespaceSymbol? containingNamespace = null)
        {
            Kind = kind;
            Name = name;
            MetadataName = metadataName;
            ContainingSymbol = containingSymbol;
            ContainingType = containingType;
            ContainingNamespace = containingNamespace;
        }

        public SymbolKind Kind { get; }

        public string Name { get; }

        public string MetadataName { get; }

        public Symbol? ContainingSymbol { get; }

        public AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

        public ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

        public NamedTypeSymbol? ContainingType { get; }

        public NamespaceSymbol? ContainingNamespace { get; }

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public Symbol UnderlyingSymbol => this;

        public bool IsAlias => false;

        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);

        public bool Equals(Symbol? other) => ReferenceEquals(this, other);

        public override bool Equals(object? obj) => ReferenceEquals(this, obj);

        public override int GetHashCode() => HashCode.Combine(Kind, Name, MetadataName);

        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private sealed class CyclicSymbol : Symbol
    {
        public CyclicSymbol(SymbolKind kind, string name, string metadataName)
        {
            Kind = kind;
            Name = name;
            MetadataName = metadataName;
        }

        public SymbolKind Kind { get; }

        public string Name { get; }

        public string MetadataName { get; }

        public Symbol? ContainingSymbol { get; private set; }

        public AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

        public ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

        public NamedTypeSymbol? ContainingType => null;

        public NamespaceSymbol? ContainingNamespace => null;

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public bool IsAlias => false;

        public Symbol UnderlyingSymbol => this;

        public void SetContainingSymbol(Symbol symbol)
        {
            ContainingSymbol = symbol;
        }

        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

        public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);

        public bool Equals(Symbol? other) => ReferenceEquals(this, other);

        public override bool Equals(object? obj) => ReferenceEquals(this, obj);

        public override int GetHashCode() => HashCode.Combine(Kind, Name, MetadataName);

        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private sealed class StubParameterSymbol : StubSymbol, ParameterSymbol
    {
        public StubParameterSymbol(string name, Symbol containingSymbol, TypeSymbol type, RefKind refKind = RefKind.None)
            : base(SymbolKind.Parameter, name, name, containingSymbol)
        {
            Type = type;
            RefKind = refKind;
        }

        public TypeSymbol Type { get; }

        public bool IsVarParams => false;

        public RefKind RefKind { get; }

        public bool IsMutable => false;

        public bool HasExplicitDefaultValue => false;

        public object? ExplicitDefaultValue => null;
    }

    private sealed class StubTypeSymbol : StubSymbol, TypeSymbol
    {
        public StubTypeSymbol(TypeKind typeKind)
            : base(SymbolKind.Type, "Processor", "Processor")
        {
            TypeKind = typeKind;
        }

        public NamedTypeSymbol? BaseType => null;

        public TypeSymbol? OriginalDefinition => this;

        public SpecialType SpecialType => SpecialType.None;

        public TypeKind TypeKind { get; }

        public ImmutableArray<NamedTypeSymbol> Interfaces => ImmutableArray<NamedTypeSymbol>.Empty;

        public ImmutableArray<NamedTypeSymbol> AllInterfaces => ImmutableArray<NamedTypeSymbol>.Empty;

        public bool IsNamespace => false;

        public bool IsType => true;

        public ImmutableArray<Symbol> GetMembers() => ImmutableArray<Symbol>.Empty;

        public ImmutableArray<Symbol> GetMembers(string name) => ImmutableArray<Symbol>.Empty;

        public TypeSymbol? LookupType(string name) => null;

        public bool IsMemberDefined(string name, out Symbol? symbol)
        {
            symbol = null;
            return false;
        }
    }
}
