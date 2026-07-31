using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TupleTypeSemanticTests
{
    [Fact]
    public void UnderlyingTupleType_IsOnlyPresentForTupleSymbols()
    {
        var source = """
class Widget {}
let value: (id: int, name: string) = (1, "Raven")
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var widget = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        var declarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var tuple = Assert.IsAssignableFrom<ITupleTypeSymbol>(
            model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type);
        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetSpecialType(SpecialType.System_String));
        var unitType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetSpecialType(SpecialType.System_Unit));

        Assert.Null(widget.UnderlyingTupleType);
        Assert.Null(stringType.UnderlyingTupleType);
        Assert.Null(unitType.UnderlyingTupleType);
        Assert.NotNull(tuple.UnderlyingTupleType);
        Assert.True(tuple.IsTupleType);
        Assert.StartsWith("ValueTuple", tuple.UnderlyingTupleType.MetadataName, StringComparison.Ordinal);
        Assert.Null(tuple.UnderlyingTupleType.UnderlyingTupleType);
    }

    [Fact]
    public void TupleTypeSyntax_BindsToTupleTypeSymbol_WithNames()
    {
        var source = """
        let t: (id: int, name: string) = (1, "")
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var tuple = Assert.IsAssignableFrom<ITupleTypeSymbol>(type);
        Assert.Collection(tuple.TupleElements,
            e => { Assert.Equal("id", e.Name); Assert.Equal(SpecialType.System_Int32, e.Type.SpecialType); },
            e => { Assert.Equal("name", e.Name); Assert.Equal(SpecialType.System_String, e.Type.SpecialType); });
    }

    [Fact]
    public void TupleSymbol_PublicMemberQueries_UseProjectedAndUnderlyingMembers()
    {
        var source = "let t: (id: int, name: string) = (1, \"Raven\")";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var tuple = Assert.IsAssignableFrom<ITupleTypeSymbol>(model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type);

        Assert.True(tuple.IsMemberDefined("id", out var id));
        Assert.Equal(SymbolKind.Field, id?.Kind);
        Assert.True(tuple.IsMemberDefined("ToString", out var toString));
        Assert.Equal(SymbolKind.Method, toString?.Kind);
        Assert.Null(tuple.LookupType("DoesNotExist"));
        Assert.Same(tuple, tuple.Construct());
    }

    [Fact]
    public void TupleElements_AreCompleteProjectedFieldSymbols()
    {
        var source = "let t: (id: int, name: string) = (1, \"Raven\")";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var tuple = Assert.IsAssignableFrom<ITupleTypeSymbol>(model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type);
        var id = Assert.IsType<TupleFieldSymbol>(tuple.TupleElements[0]);

        Assert.IsNotType<PEFieldSymbol>(id);
        Assert.StartsWith("ValueTuple", id.ContainingType.Name, StringComparison.Ordinal);
        Assert.Same(id.ContainingType, id.ContainingSymbol);
        Assert.NotNull(id.ContainingAssembly);
        Assert.NotNull(id.ContainingModule);
        Assert.NotNull(id.ContainingNamespace);
        Assert.Equal("Item1", id.MetadataName);
        Assert.Equal(RefKind.None, id.RefKind);
        Assert.False(id.IsConst);
        Assert.False(id.IsRequired);
        Assert.False(id.IsReadOnly);
        Assert.False(id.IsStatic);
        Assert.Null(id.GetConstantValue());
        Assert.Empty(id.GetAttributes());
    }

    [Fact]
    public void TupleExpression_TargetTyped_UsesDeclaredType_IgnoringNames()
    {
        var source = """
        let pair: (id: int, name: string) = (no: 42, identifier: "answer")
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initializerType = model.GetTypeInfo(declarator.Initializer!.Value).Type;
        var annotationType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        Assert.True(SymbolEqualityComparer.Default.Equals(annotationType, initializerType));
    }

    [Fact]
    public void TupleExpression_TargetTyped_WithoutNames_Succeeds()
    {
        var source = """
        let pair: (int, string) = (42, "Bar")
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initializerType = model.GetTypeInfo(declarator.Initializer!.Value).Type;
        var annotationType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        Assert.True(SymbolEqualityComparer.Default.Equals(annotationType, initializerType));
    }

    [Fact]
    public void NamedTuple_ReturnedFromFunction_PreservesNamesForMemberAccess()
    {
        var source = """
        let person = GetPerson("Bob", 40)
        let name = person.name
        let age = person.age

        func GetPerson(name: string, age: int) -> (name: string, age: int) {
            return (name, age)
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        Assert.Empty(compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToDictionary(static declarator => declarator.Identifier.ValueText);

        var person = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators["person"]));
        var personType = Assert.IsAssignableFrom<ITupleTypeSymbol>(person.Type);
        Assert.Collection(
            personType.TupleElements,
            element =>
            {
                Assert.Equal("name", element.Name);
                Assert.Equal(SpecialType.System_String, element.Type.SpecialType);
            },
            element =>
            {
                Assert.Equal("age", element.Name);
                Assert.Equal(SpecialType.System_Int32, element.Type.SpecialType);
            });

        var name = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators["name"]));
        var age = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators["age"]));
        Assert.Equal(SpecialType.System_String, name.Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, age.Type.SpecialType);
    }

    [Fact]
    public void TupleExpression_TargetTypedMismatch_ReportsDiagnostic()
    {
        var source = "let t: (int, string) = (1, 2)";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType, diagnostic.Descriptor);
    }
}
