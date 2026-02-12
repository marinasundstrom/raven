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

        val value: Text = ""
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
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

        val value: Text = ""
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
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

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
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
    public void AddressType_To_ByRef_IsImplicit()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var address = new AddressTypeSymbol(intType);
        var refType = new RefTypeSymbol(intType);

        var conversion = compilation.ClassifyConversion(address, refType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.False(conversion.IsPointer);
    }

    [Fact]
    public void AddressType_To_Pointer_IsImplicitPointerConversion()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var address = new AddressTypeSymbol(intType);
        var pointer = (IPointerTypeSymbol)compilation.CreatePointerTypeSymbol(intType);

        var conversion = compilation.ClassifyConversion(address, pointer);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsPointer);
    }

    [Fact]
    public void AddressType_To_ByRef_AliasUnderlying_FlagsAlias()
    {
        const string source = """
        alias Text = System.String

        val value: Text = ""
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var aliasType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type!;
        Assert.True(aliasType.IsAlias);

        var address = new AddressTypeSymbol(aliasType);
        var refType = new RefTypeSymbol(aliasType);

        var conversion = compilation.ClassifyConversion(address, refType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsAlias);
    }

    [Fact]
    public void AddressType_To_Pointer_AliasUnderlying_FlagsAlias()
    {
        const string source = """
        alias Text = System.String

        val value: Text = ""
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var aliasType = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type!;
        Assert.True(aliasType.IsAlias);

        var address = new AddressTypeSymbol(aliasType);
        var pointer = (IPointerTypeSymbol)compilation.CreatePointerTypeSymbol(aliasType);

        var conversion = compilation.ClassifyConversion(address, pointer);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsPointer);
        Assert.True(conversion.IsAlias);
    }

    [Fact]
    public void Metadata_MethodInfo_DerivesFrom_MemberInfo()
    {
        var compilation = CreateCompilation();

        var methodInfo = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Reflection.MethodInfo")!;
        var memberInfo = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Reflection.MemberInfo")!;

        Assert.NotNull(methodInfo.BaseType);
        Assert.True(SemanticFacts.IsDerivedFrom(methodInfo, memberInfo, SymbolEqualityComparer.Default));

        var conversion = compilation.ClassifyConversion(memberInfo, methodInfo);
        Assert.True(conversion.Exists);
        Assert.False(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
    }

    [Fact]
    public void Metadata_ExplicitCast_MemberInfo_To_MethodInfo_Succeeds()
    {
        const string source = """
        import System.Reflection.*

        val type = typeof(System.Object)
        val members = type.GetMembers()
        val member = members[0]
        val method = (System.Reflection.MethodInfo)member
        """;

        var (compilation, tree) = CreateCompilation(source, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        var model = compilation.GetSemanticModel(tree);
        var cast = tree.GetRoot().DescendantNodes().OfType<CastExpressionSyntax>().Single();
        var expressionType = model.GetTypeInfo(cast.Expression).Type!;
        var targetType = model.GetTypeInfo(cast).Type!;

        var conversion = compilation.ClassifyConversion(expressionType, targetType);
        Assert.True(conversion.Exists);

        Assert.DoesNotContain(
            diagnostics,
            d => d.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void UserDefinedConversions_AreClassifiedWithImplicitAndExplicitFlags()
    {
        const string source = """
        class Box {
            public static implicit operator(value: Box) -> string { return "" }
            public static explicit operator(value: Box) -> int { return 0 }
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var boxDecl = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var boxType = (INamedTypeSymbol)model.GetDeclaredSymbol(boxDecl)!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var implicitConversion = compilation.ClassifyConversion(boxType, stringType);

        Assert.True(implicitConversion.Exists);
        Assert.True(implicitConversion.IsImplicit);
        Assert.True(implicitConversion.IsUserDefined);
        Assert.NotNull(implicitConversion.MethodSymbol);

        var explicitConversion = compilation.ClassifyConversion(boxType, intType);

        Assert.True(explicitConversion.Exists);
        Assert.False(explicitConversion.IsImplicit);
        Assert.True(explicitConversion.IsUserDefined);
        Assert.NotNull(explicitConversion.MethodSymbol);
    }
}
