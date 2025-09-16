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
        class Base {}
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
}
