using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RecordClassSemanticTests : CompilationTestBase
{
    [Fact]
    public void RecordClassPrimaryConstructorCreatesPropertiesAndValueMembers()
    {
        var source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        var nameProperty = Assert.IsAssignableFrom<IPropertySymbol>(
            Assert.Single(person.GetMembers("Name")));
        var ageProperty = Assert.IsAssignableFrom<IPropertySymbol>(
            Assert.Single(person.GetMembers("Age")));

        Assert.Equal(SpecialType.System_String, nameProperty.Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, ageProperty.Type.SpecialType);
        Assert.NotNull(nameProperty.GetMethod);
        Assert.NotNull(ageProperty.GetMethod);
        Assert.Null(nameProperty.SetMethod);
        Assert.Null(ageProperty.SetMethod);

        Assert.Contains(
            person.GetMembers("Equals").OfType<IMethodSymbol>(),
            method => method.Parameters.Length == 1 &&
                      SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, person));
        Assert.Contains(
            person.GetMembers("Equals").OfType<IMethodSymbol>(),
            method => method.Parameters.Length == 1 &&
                      method.Parameters[0].Type.SpecialType == SpecialType.System_Object);
        Assert.Contains(
            person.GetMembers("GetHashCode").OfType<IMethodSymbol>(),
            method => method.Parameters.Length == 0 &&
                      method.ReturnType.SpecialType == SpecialType.System_Int32);
        Assert.Contains(
            person.GetMembers("op_Equality").OfType<IMethodSymbol>(),
            method => method.MethodKind == MethodKind.UserDefinedOperator);
        Assert.Contains(
            person.GetMembers("op_Inequality").OfType<IMethodSymbol>(),
            method => method.MethodKind == MethodKind.UserDefinedOperator);

        var deconstruct = Assert.Single(
            person.GetMembers("Deconstruct").OfType<IMethodSymbol>()
                .Where(method => method.Parameters.Length == 2));
        Assert.Equal(SpecialType.System_Unit, deconstruct.ReturnType.SpecialType);
        Assert.All(deconstruct.Parameters, parameter => Assert.Equal(RefKind.Out, parameter.RefKind));

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordPattern_BindsPrimaryConstructorProperties()
    {
        var source = """
            record class Person(Name: string, Age: int);

            val value: object = new Person("Ada", 42);

            val result = value match {
                Person(val name, val age) => name
                _ => ""
            };
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var recordPattern = tree.GetRoot().DescendantNodes().OfType<RecordPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(recordPattern));

        Assert.Equal(2, boundPattern.Arguments.Length);
        Assert.Equal("Deconstruct", boundPattern.DeconstructMethod.Name);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PositionalPattern_UsesDeconstructWhenAvailable()
    {
        var source = """
            record class Pair(Left: int, Right: int);

            val value: Pair = new Pair(1, 2);

            val result = value match {
                (val left, val right) => left
                _ => 0
            };
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var tuplePattern = tree.GetRoot().DescendantNodes().OfType<PositionalPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(tuplePattern));

        Assert.Equal(2, boundPattern.Arguments.Length);
        Assert.Equal("Deconstruct", boundPattern.DeconstructMethod.Name);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_WithPrimaryConstructorAndNoBody_Binds()
    {
        var source = """
            record class Person(Name: string, Age: int);
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        Assert.Equal(2, person.GetMembers().OfType<IPropertySymbol>().Count());
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordClass_WithPrimaryConstructorAndBody_Binds()
    {
        var source = """
            record class Person(Name: string, Age: int) {}
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.GetSemanticModel(tree);

        var person = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Person"));

        Assert.Equal(2, person.GetMembers().OfType<IPropertySymbol>().Count());
        Assert.Empty(compilation.GetDiagnostics());
    }
}
