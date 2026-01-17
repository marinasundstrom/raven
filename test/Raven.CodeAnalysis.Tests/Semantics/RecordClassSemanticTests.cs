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

        Assert.Empty(compilation.GetDiagnostics());
    }
}
