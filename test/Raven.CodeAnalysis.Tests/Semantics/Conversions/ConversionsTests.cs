using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

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
    public void AliasType_To_UnderlyingType_FlagsAlias()
    {
        var source = """
        alias Text = System.String

        let value: Text = ""
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

        let value: Text = ""
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

        let value: Text = ""
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

        let value: Text = ""
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

        let type = typeof(System.Object)
        let members = type.GetMembers()
        let member = members[0]
        let method = (System.Reflection.MethodInfo)member
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
            static func implicit(value: Box) -> string { return "" }
            static func explicit(value: Box) -> int { return 0 }
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

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void UserDefinedConversion_PrefersExactTargetRegardlessOfDeclarationOrQueryOrder(
        bool reverseDeclarations,
        bool diagnosticsFirst)
    {
        const string targetDeclaration = """
open class Target {
    static func implicit(value: Source) -> Target { return default! }
}

class Specific : Target {}
""";
        const string sourceDeclaration = """
class Source {
    static func implicit(value: Source) -> Specific { return default! }
}
""";
        var first = reverseDeclarations ? sourceDeclaration : targetDeclaration;
        var second = reverseDeclarations ? targetDeclaration : sourceDeclaration;
        var source = $$"""
{{first}}
{{second}}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var sourceType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Source"));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Target"));
        var conversion = compilation.ClassifyConversion(sourceType, targetType);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUserDefined);
        Assert.True(SymbolEqualityComparer.Default.Equals(targetType, conversion.MethodSymbol?.ReturnType));

        if (!diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void UserDefinedConversion_PrefersExactSourceRegardlessOfDeclarationOrQueryOrder(
        bool reverseDeclarations,
        bool diagnosticsFirst)
    {
        var first = reverseDeclarations
            ? "static func implicit(value: Source) -> Target { return default! }"
            : "static func implicit(value: BaseSource) -> Target { return default! }";
        var second = reverseDeclarations
            ? "static func implicit(value: BaseSource) -> Target { return default! }"
            : "static func implicit(value: Source) -> Target { return default! }";
        var source = $$"""
open class BaseSource {}
class Source : BaseSource {}

class Target {
    {{first}}
    {{second}}
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var sourceType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Source"));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Target"));
        var conversion = compilation.ClassifyConversion(sourceType, targetType);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUserDefined);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            sourceType,
            conversion.MethodSymbol?.Parameters.Single().Type));

        if (!diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void UserDefinedConversion_ConflictingSourceAndTargetAdvantagesAreAmbiguous(
        bool reverseDeclarations,
        bool diagnosticsFirst)
    {
        const string targetDeclarations = """
open class Target {
    static func implicit(value: BaseSource) -> Target { return default! }
}

class Specific : Target {}
""";
        const string sourceDeclarations = """
open class BaseSource {}

class Source : BaseSource {
    static func implicit(value: Source) -> Specific { return default! }
}
""";
        var first = reverseDeclarations ? sourceDeclarations : targetDeclarations;
        var second = reverseDeclarations ? targetDeclarations : sourceDeclarations;
        var source = $$"""
{{first}}
{{second}}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var sourceType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Source"));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Target"));
        var conversion = compilation.ClassifyConversion(sourceType, targetType);

        Assert.False(conversion.Exists);

        if (!diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void UserDefinedConversion_ExactTargetSelectionMatchesEmittedMetadata(bool diagnosticsFirst)
    {
        const string librarySource = """
public open class Target {
    static func implicit(value: Source) -> Target { return default! }
}

public class Specific : Target {}

public class Source {
    static func implicit(value: Source) -> Specific { return default! }
}
""";
        var reference = TestMetadataFactory.CreateFromSource(
            librarySource,
            "user_defined_conversion_library");
        var compilation = Compilation.Create(
            "user_defined_conversion_consumer",
            [],
            [.. TestMetadataReferences.Default, reference],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var sourceType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Source"));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Target"));
        var conversion = compilation.ClassifyConversion(sourceType, targetType);

        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUserDefined);
        Assert.True(SymbolEqualityComparer.Default.Equals(targetType, conversion.MethodSymbol?.ReturnType));

        if (!diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
