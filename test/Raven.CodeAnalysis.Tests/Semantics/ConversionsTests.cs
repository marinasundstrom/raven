using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConversionsTests : CompilationTestBase
{
    [Fact]
    public void IdentityConversion_SameType_IsImplicitAndNotAlias()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var conversion = compilation.ClassifyConversion(intType, intType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
        Assert.False(conversion.IsAlias);
    }

    [Fact]
    public void LiteralString_To_String_IsIdentity()
    {
        var compilation = CreateCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var literal = new LiteralTypeSymbol(stringType, "Foo", compilation);

        var conversion = compilation.ClassifyConversion(literal, stringType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
        Assert.False(conversion.IsAlias);
    }

    [Fact]
    public void LiteralChar_To_Char_IsIdentity()
    {
        var compilation = CreateCompilation();
        var charType = compilation.GetSpecialType(SpecialType.System_Char);
        var literal = new LiteralTypeSymbol(charType, 'a', compilation);

        var conversion = compilation.ClassifyConversion(literal, charType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
        Assert.False(conversion.IsAlias);
    }

    [Fact]
    public void LiteralInt_To_Long_IsImplicitNumeric()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);
        var literal = new LiteralTypeSymbol(intType, 42, compilation);

        var conversion = compilation.ClassifyConversion(literal, longType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsNumeric);
        Assert.False(conversion.IsIdentity);
        Assert.False(conversion.IsAlias);
    }

    [Fact]
    public void AliasType_To_UnderlyingType_FlagsAlias()
    {
        var source = """
        alias Text = System.String

        let value: Text = ""
        """;

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var aliasType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type!;
        Assert.True(aliasType.IsAlias);

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var conversion = compilation.ClassifyConversion(aliasType, stringType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
        Assert.True(conversion.IsAlias);
    }

    [Fact]
    public void UnderlyingType_To_AliasType_FlagsAlias()
    {
        var source = """
        alias Text = System.String

        let value: Text = ""
        """;

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var aliasType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type!;
        Assert.True(aliasType.IsAlias);

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var conversion = compilation.ClassifyConversion(stringType, aliasType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsIdentity);
        Assert.True(conversion.IsAlias);
    }

    [Fact]
    public void DerivedType_To_BaseType_IsImplicitReferenceConversion()
    {
        var source = """
        open class Base {}
        class Derived : Base {}
        """;

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var classes = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().ToArray();
        var baseType = (INamedTypeSymbol)model.GetDeclaredSymbol(classes[0])!;
        var derivedType = (INamedTypeSymbol)model.GetDeclaredSymbol(classes[1])!;

        var conversion = compilation.ClassifyConversion(derivedType, baseType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
        Assert.False(conversion.IsAlias);
    }

    [Fact]
    public void VariableDeclaration_InsertsCast_ForImplicitNumericConversion()
    {
        const string source = "let value: double = 1";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var boundDeclarator = Assert.IsType<BoundVariableDeclarator>(model.GetBoundNode(declarator));
        var initializer = Assert.IsType<BoundCastExpression>(boundDeclarator.Initializer);

        Assert.Equal(SpecialType.System_Double, initializer.Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, initializer.Expression.Type!.SpecialType);
    }

    [Fact]
    public void AssignmentExpression_InsertsCast_ForImplicitNumericConversion()
    {
        const string source = """
        let value: double = 0
        value = 1
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();
        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var cast = Assert.IsType<BoundCastExpression>(boundAssignment.Expression.Right);

        Assert.Equal(SpecialType.System_Double, cast.Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, cast.Expression.Type!.SpecialType);
    }

    [Fact]
    public void Assignment_NullLiteral_To_NullableReference_PreservesConvertedType()
    {
        const string source = """
        let value: string? = ""
        value = null
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();
        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var literal = Assert.IsType<BoundLiteralExpression>(boundAssignment.Expression.Right);
        var converted = Assert.IsType<NullableTypeSymbol>(literal.GetConvertedType());

        Assert.Equal(SpecialType.System_String, converted.UnderlyingType.SpecialType);
    }
}
