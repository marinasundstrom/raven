using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SymbolDisplayTests : CompilationTestBase
{
    [Fact]
    public void ConstField_ToDisplayString_ExcludesStaticModifier()
    {
        const string source = """
class C {
    public const MaxValue: char = 'z'
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarator));

        var display = field.ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat);

        Assert.Equal("const MaxValue: char = 'z'", display);
        Assert.True(field.IsStatic);
        Assert.True(field.IsConst);
    }

    [Fact]
    public void Property_ToDisplayString_IncludesAccessorAccessibility()
    {
        const string source = """
class C {
    val Value: int { get; private set; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));

        var format = SymbolDisplayFormat.RavenCodeGenerationFormat
            .WithPropertyStyle(SymbolDisplayPropertyStyle.ShowReadWriteDescriptor);
        var display = symbol.ToDisplayString(format);

        Assert.Equal("Value: int { get; private set; }", display);
    }

    [Fact]
    public void Method_ToDisplayString_FormatsProtectedAccessibility()
    {
        const string source = """
class Base {
    protected func Run() -> unit { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(method));

        var display = symbol.ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat);

        Assert.Equal("protected Run() -> ()", display);
    }

    [Fact]
    public void Method_ToDisplayString_FormatsAllAccessibilityModifiers()
    {
        const string source = """
class Base {
    private func PrivateRun() -> unit { }
    internal func InternalRun() -> unit { }
    protected func ProtectedRun() -> unit { }
    protected internal func ProtectedInternalRun() -> unit { }
    private protected func PrivateProtectedRun() -> unit { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .ToDictionary(
                static declaration => declaration.Identifier.ValueText,
                declaration => Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(declaration)));

        methods["PrivateRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("private PrivateRun() -> ()");
        methods["InternalRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("internal InternalRun() -> ()");
        methods["ProtectedRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("protected ProtectedRun() -> ()");
        methods["ProtectedInternalRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("protected internal ProtectedInternalRun() -> ()");
        methods["PrivateProtectedRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("private protected PrivateProtectedRun() -> ()");
    }

    [Fact]
    public void Method_ToDisplayString_IncludesOutParameterModifiers()
    {
        const string source = """
class MacroArgument {
    func TryParseValue<T>(out value: int) -> bool { false }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(method));

        symbol.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat)
            .ShouldBe("func TryParseValue<T>(out value: int) -> bool");
        symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
            .ShouldBe("TryParseValue<T>(out value: int) -> bool");
    }

    [Fact]
    public void UnionType_ToDisplayString_IncludesUnionRepresentationKeyword()
    {
        const string source = """
record Cash(amount: decimal)
record Card(reference: string)

union Payment(Cash | Card)

union Response<T> {
    case Success(value: T)
    case Failure(message: string)
}

union struct ValueOption<T> {
    case Some(value: T)
    case None
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarations = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().ToArray();

        var payment = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declarations[0]));
        var response = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declarations[1]));
        var valueOption = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declarations[2]));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithKindOptions(
            SymbolDisplayFormat.MinimallyQualifiedFormat.KindOptions |
            SymbolDisplayKindOptions.IncludeTypeKeyword);
        var declarationFormat = format.WithMiscellaneousOptions(
            format.MiscellaneousOptions | SymbolDisplayMiscellaneousOptions.IncludeUnionMemberTypes);

        payment.ToDisplayString(format).ShouldBe("union class Payment");
        response.ToDisplayString(format).ShouldBe("union class Response<T>");
        valueOption.ToDisplayString(format).ShouldBe("union struct ValueOption<T>");

        payment.ToDisplayString(declarationFormat).ShouldBe("union class Payment(Cash | Card)");
        response.ToDisplayString(declarationFormat).ShouldBe("union class Response<T>(Success<T> | Failure)");
        valueOption.ToDisplayString(declarationFormat).ShouldBe("union struct ValueOption<T>(Some<T> | None)");
    }

    [Fact]
    public void UnionCaseSymbol_ToDisplayString_UsesCaseMemberKeyword()
    {
        const string source = """
union Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var okClause = tree.GetRoot().DescendantNodes().OfType<UnionCaseClauseSyntax>().First();
        var okSymbol = Assert.IsAssignableFrom<IUnionCaseTypeSymbol>(model.GetDeclaredSymbol(okClause));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithKindOptions(
            SymbolDisplayFormat.MinimallyQualifiedFormat.KindOptions |
            SymbolDisplayKindOptions.IncludeMemberKeyword);

        okSymbol.ToDisplayString(format).ShouldBe("case Ok<T>");
    }

    [Fact]
    public void GenericParenthesizedUnion_ToDisplayString_IncludesTypeArgumentsAndMemberTypes()
    {
        const string source = """
union Either<T1, T2>(T1 | T2)

func Test() {
    val value: Either<int, string> = 42
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var either = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));
        var value = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(local));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithKindOptions(
            SymbolDisplayFormat.MinimallyQualifiedFormat.KindOptions |
            SymbolDisplayKindOptions.IncludeTypeKeyword);
        var declarationFormat = format.WithMiscellaneousOptions(
            format.MiscellaneousOptions | SymbolDisplayMiscellaneousOptions.IncludeUnionMemberTypes);

        either.ToDisplayString(format).ShouldBe("union class Either<T1, T2>");
        value.Type.ToDisplayString(format).ShouldBe("union class Either<int, string>");

        either.ToDisplayString(declarationFormat).ShouldBe("union class Either<T1, T2>(T1 | T2)");
        value.Type.ToDisplayString(declarationFormat).ShouldBe("union class Either<int, string>(int | string)");
    }

    [Fact]
    public void SealedHierarchyTypes_ToDisplayString_UsesSealedModifierForClassAndInterface()
    {
        const string source = """
sealed class Expr {}
sealed interface HttpResponse {}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var classSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        var interfaceSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single()));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithKindOptions(
            SymbolDisplayFormat.MinimallyQualifiedFormat.KindOptions |
            SymbolDisplayKindOptions.IncludeTypeKeyword);

        classSymbol.ToDisplayString(format).ShouldBe("sealed class Expr");
        interfaceSymbol.ToDisplayString(format).ShouldBe("sealed interface HttpResponse");
    }
}
