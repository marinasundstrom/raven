using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class UnionSemanticTests : CompilationTestBase
{
    [Fact]
    public void UnionCasePayloads_WithUnresolvedTypes_ReportDiagnostics()
    {
        const string source = """
union KettleState {
    case Empty
    case Filled(water: Water)
    case Heating(water: Water, temperature: double)
    case Boiling(water: Water)
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var missingTypeDiagnostics = compilation.GetDiagnostics()
            .Where(static diagnostic =>
                diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext)
            .ToArray();

        Assert.Collection(
            missingTypeDiagnostics.OrderBy(static diagnostic => diagnostic.Location.SourceSpan.Start),
            water => Assert.Contains("Water", water.GetMessage(), StringComparison.Ordinal),
            water => Assert.Contains("Water", water.GetMessage(), StringComparison.Ordinal),
            water => Assert.Contains("Water", water.GetMessage(), StringComparison.Ordinal));
    }

    [Theory]
    [InlineData("private field _state: int = 0", "field declarations")]
    [InlineData("static field Instances: int = 0", "field declarations")]
    [InlineData("const Code: int = 1", "constant declarations")]
    [InlineData("event Changed: System.Action;", "event declarations")]
    [InlineData("init(value: int) {}", "constructors")]
    [InlineData("init {}", "constructors")]
    [InlineData("static func +(left: Result, right: Result) -> Result => left", "operator declarations")]
    [InlineData("class Metadata {}", "nested type declarations")]
    public void Union_OnlyAllowsCasesPropertiesIndexersAndOrdinaryMethods(string member, string memberKind)
    {
        var source = $$"""
union Result {
    case Ok(value: int)

    {{member}}
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.UnionMemberKindNotAllowed, diagnostic.Descriptor);
        Assert.Contains(memberKind, diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void Union_ComputedAndStaticPropertiesAndOrdinaryMethods_AreAllowed()
    {
        const string source = """
union Result {
    case Ok(value: int)

    val IsOk: bool => self is .Ok(_)
    static val DisplayName: string => "Result"

    val self[index: int]: bool {
        get => index == 0
    }

    func Describe() -> string => self.ToString()
    static func Create(value: int) -> Result => .Ok(value)
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Union_StorageProperties_ReportDiagnostics()
    {
        const string source = """
union Result {
    case Ok

    var Count: int = 0
    static var GlobalCount: int = 0
    val Cached: bool { get; }
    val DisplayName: string {
        get => field
    }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics().ToArray();

        Assert.Equal(4, diagnostics.Length);
        Assert.All(diagnostics, d => Assert.Equal(CompilerDiagnostics.UnionStoragePropertyNotAllowed, d.Descriptor));
        Assert.Contains(diagnostics, d => d.GetMessage().Contains("Count", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.GetMessage().Contains("GlobalCount", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.GetMessage().Contains("Cached", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.GetMessage().Contains("DisplayName", StringComparison.Ordinal));
    }

    [Fact]
    public void UnnamedGenericCasePayloads_ProjectToTheirCarrierAgainstNet11References()
    {
        const string source = """
namespace System

import System.*
import System.Option.*

union Option<T> {
    case Some(T)
    case None

    func Map<TResult>(mapper: T -> TResult) -> Option<TResult> {
        self match {
            Some(let value) => Some(mapper(value))
            None => None
        }
    }
}
""";

        var version = TargetFrameworkResolver.ResolveVersion("net11.0");
        var references = TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Where(File.Exists)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
        var (compilation, _) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithEmbedCoreTypes(true),
            references);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void TargetTypedMatchArms_PropagateThroughNestedIfAndBlockExpressions()
    {
        const string source = """
namespace System

import System.*
import System.Option.*

union Option<T> {
    case Some(value: T)
    case None

    func Filter(predicate: T -> bool) -> Option<T> {
        self match {
            Some(let value) => if predicate(value) { Some(value) } else { None }
            None => None
        }
    }

    func Tap(action: T -> ()) -> Option<T> {
        self match {
            Some(let value) => {
                action(value)
                Some(value)
            }
            None => None
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var errors = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();

        Assert.Empty(errors);
    }

    [Fact]
    public void GetDeclaredSymbol_ReturnsCaseSymbol()
    {
        const string source = """
union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var caseClause = unionDecl.Members.OfType<CaseDeclarationSyntax>().First();
        var symbol = model.GetDeclaredSymbol(caseClause);

        var caseSymbol = Assert.IsAssignableFrom<IUnionCaseTypeSymbol>(symbol);
        Assert.Equal("None", caseSymbol.Name);
    }

    [Fact]
    public void GenericCase_HasUnionSemanticOwnerAndCompanionMetadataOwner()
    {
        const string source = """
        union Result<T, E> {
            case Ok(value: T)
            case Error(error: E)
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var unionDeclaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var union = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDeclaration));
        var ok = Assert.Single(union.DeclaredCaseTypes, @case => @case.Name == "Ok");

        Assert.Same(union, ok.ContainingSymbol);
        Assert.Same(union, ok.ContainingType);
        Assert.NotSame(union, ok.MetadataContainingType);
        Assert.Equal("Result", ok.MetadataContainingType.Name);
        Assert.Equal(0, ok.MetadataContainingType.Arity);
    }

    [Fact]
    public void NonGenericCase_UsesUnionAsSemanticAndMetadataOwner()
    {
        const string source = """
        union Option {
            case Some(value: int)
            case None
        }
        """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var unionDeclaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var union = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDeclaration));
        var some = Assert.Single(union.DeclaredCaseTypes, @case => @case.Name == "Some");

        Assert.Same(union, some.ContainingSymbol);
        Assert.Same(union, some.ContainingType);
        Assert.Same(union, some.MetadataContainingType);
    }

    [Fact]
    public void UnnamedCasePayloads_ProjectStableMetadataNamesAndSourceLikeSignatures()
    {
        const string source = """
union Payload {
    case Single(int)
    case Pair(int, string)
    case Named(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var union = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(declaration));
        var single = union.DeclaredCaseTypes.Single(@case => @case.Name == "Single");
        var pair = union.DeclaredCaseTypes.Single(@case => @case.Name == "Pair");
        var named = union.DeclaredCaseTypes.Single(@case => @case.Name == "Named");

        var singleParameter = Assert.Single(single.ConstructorParameters);
        Assert.Equal("value", singleParameter.Name);
        Assert.True(singleParameter.HasImplicitName);
        Assert.Contains(singleParameter.GetAttributes(), attribute =>
            attribute.AttributeClass?.Name == "CompilerGeneratedAttribute" &&
            attribute.AttributeClass.ContainingNamespace?.ToMetadataName() == "System.Runtime.CompilerServices");
        Assert.Equal(
            Accessibility.Public,
            Assert.Single(single.GetMembers("Value").OfType<IPropertySymbol>()).DeclaredAccessibility);

        Assert.Collection(
            pair.ConstructorParameters,
            first =>
            {
                Assert.Equal("item1", first.Name);
                Assert.True(first.HasImplicitName);
            },
            second =>
            {
                Assert.Equal("item2", second.Name);
                Assert.True(second.HasImplicitName);
            });
        Assert.Equal(
            Accessibility.Public,
            Assert.Single(pair.GetMembers("Item1").OfType<IPropertySymbol>()).DeclaredAccessibility);
        Assert.Equal(
            Accessibility.Public,
            Assert.Single(pair.GetMembers("Item2").OfType<IPropertySymbol>()).DeclaredAccessibility);

        var namedParameter = Assert.Single(named.ConstructorParameters);
        Assert.Equal("message", namedParameter.Name);
        Assert.False(namedParameter.HasImplicitName);
        Assert.Single(named.GetMembers("Message").OfType<IPropertySymbol>());

        Assert.Equal("case Single(int)", single.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
        Assert.Equal("case Pair(int, string)", pair.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
        Assert.Equal("case Named(message: string)", named.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));

        var generatedNameFormat = SymbolDisplayFormat.RavenSignatureFormat.WithParameterOptions(
            SymbolDisplayFormat.RavenSignatureFormat.ParameterOptions |
            SymbolDisplayParameterOptions.IncludeGeneratedNames);
        Assert.Equal("case Single(value: int)", single.ToDisplayString(generatedNameFormat));
        Assert.Equal("case Pair(item1: int, item2: string)", pair.ToDisplayString(generatedNameFormat));
    }

    [Fact]
    public void UnionCasePayloads_CannotMixNamedAndUnnamedForms()
    {
        const string source = """
union Mixed {
    case Invalid(int, message: string)
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        Assert.Contains(compilation.GetDiagnostics(), diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.UnionCasePayloadStyleMixed);
    }

    [Fact]
    public void UnnamedCasePayloads_RoundTripThroughMetadataWithoutChangingSourceDisplay()
    {
        const string source = """
public union Payload {
    case Single(int)
    case Pair(int, string)
}
""";

        var reference = TestMetadataFactory.CreateFileReferenceFromSource(source, "UnnamedUnionPayloadMetadata");
        var compilation = Compilation.Create(
            "consumer",
            [],
            [.. TestMetadataReferences.Default, reference],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var payload = Assert.IsAssignableFrom<IUnionSymbol>(compilation.GetTypeByMetadataName("Payload"));
        var single = Assert.Single(payload.DeclaredCaseTypes, unionCase => unionCase.Name == "Single");
        var pair = Assert.Single(payload.DeclaredCaseTypes, unionCase => unionCase.Name == "Pair");

        Assert.True(Assert.Single(single.ConstructorParameters).HasImplicitName);
        Assert.All(pair.ConstructorParameters, parameter => Assert.True(parameter.HasImplicitName));
        Assert.Equal("case Single(int)", single.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
        Assert.Equal("case Pair(int, string)", pair.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
    }

    [Fact]
    public void UnionDeclaration_WithoutStorageModifier_DefaultsToStructUnionAndStructCases()
    {
        const string source = """
union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.Equal(TypeKind.Struct, unionSymbol.TypeKind);
        Assert.All(unionSymbol.CaseTypes, static @case => Assert.Equal(TypeKind.Struct, @case.TypeKind));
        Assert.Equal(2, unionSymbol.MemberTypes.Length);
    }

    [Fact]
    public void UnionDeclaration_WithStructModifier_UsesStructUnionAndStructCases()
    {
        const string source = """
union struct Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.Equal(TypeKind.Struct, unionSymbol.TypeKind);
        Assert.All(unionSymbol.CaseTypes, static @case => Assert.Equal(TypeKind.Struct, @case.TypeKind));
    }

    [Fact]
    public void BodyDefinedUnion_DeclaredCaseTypes_MatchesCaseTypes()
    {
        const string source = """
union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.Equal(2, unionSymbol.DeclaredCaseTypes.Length);
        Assert.Equal(
            unionSymbol.DeclaredCaseTypes.Select(static caseType => caseType.Name),
            unionSymbol.CaseTypes.Select(static caseType => caseType.Name));
    }

    [Fact]
    public void NominalUnionDeclaration_BindsDeclaredMemberTypes()
    {
        const string source = """
record Left(value: int)
record Right(message: string)

union Either(Left | Right)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.Equal(TypeKind.Struct, unionSymbol.TypeKind);
        Assert.Empty(unionSymbol.DeclaredCaseTypes);
        Assert.Collection(
            unionSymbol.CaseTypes,
            left => Assert.Equal("Left", left.Name),
            right => Assert.Equal("Right", right.Name));
        Assert.Collection(
            unionSymbol.MemberTypes,
            left => Assert.Equal("Left", left.Name),
            right => Assert.Equal("Right", right.Name));
    }

    [Fact]
    public void ParenthesizedUnionMatch_WithMissingMember_ReportsExhaustivenessDiagnostic()
    {
        const string source = """
func Test() -> int {
    let value: Either = "text"

    return match value {
        string text => text.Length
    }
}

union Either(int | string)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive));

        Assert.Contains("int", diagnostic.GetMessage(), StringComparison.Ordinal);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.False(info.IsExhaustive);
        Assert.Collection(info.MissingCases, missing => Assert.Equal("int", missing));
    }

    [Fact]
    public void ParenthesizedUnionMatch_WithAllMembers_IsExhaustive()
    {
        const string source = """
func Test() -> int {
    let value: Either = 1

    return match value {
        int number => number
        string text => text.Length
    }
}

union Either(int | string)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void NominalUnionDeclaration_WithNullableMember_MarksNullableContent()
    {
        const string source = """
union Foo(int | double?)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.True(unionSymbol.ContentMayBeNull);
        Assert.Equal(2, unionSymbol.MemberTypes.Length);
        Assert.False(unionSymbol.MemberTypes[1].IsNullable);
        Assert.Equal(SpecialType.System_Double, unionSymbol.MemberTypes[1].SpecialType);
    }

    [Fact]
    public void NominalUnionDeclaration_WithoutNullableMember_DoesNotMarkNullableContent()
    {
        const string source = """
union Foo(int | double)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.False(unionSymbol.ContentMayBeNull);

        var valueProperty = Assert.Single(unionSymbol.GetMembers("Value").OfType<IPropertySymbol>());
        Assert.True(valueProperty.Type.IsNullable);
    }

    [Fact]
    public void ParenthesizedUnionMatch_WithNullableMemberPattern_StillRequiresNullArm()
    {
        const string source = """
func Test() -> int {
    let value: Foo = 1

    return match value {
        int number => number
        double? number => 0
    }
}

union Foo(int | double?)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive));
        Assert.Contains("null", diagnostic.GetMessage(), StringComparison.Ordinal);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.False(info.IsExhaustive);
        Assert.Collection(info.MissingCases, missing => Assert.Equal("null", missing));
    }

    [Fact]
    public void NullLiteral_DoesNotConvertToUnionWithNullableContent()
    {
        const string source = """
union Foo(int | double?)

func Test() -> () {
    let value: Foo = null
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void NominalUnionDeclaration_WithExplicitNullMember_ReportsDiagnostic()
    {
        const string source = """
union Foo(int | double | null)
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TypeExpectedWithoutWildcard);
    }

    [Fact]
    public void NullTypeSyntax_OutsideParenthesizedUnionDeclaration_IsRejected()
    {
        const string source = """
func Test(value: int | null) -> () {
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.NotEmpty(diagnostics);
    }

    [Fact]
    public void ParenthesizedUnionMatch_WithLiteralAndNull_CoversNullButNotEntireMemberType()
    {
        const string source = """
func Test(x2: Test2) -> int {
    let r = match x2 {
        42 => 3
        null => 2
    }

    return r
}

union Test2(int?)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.False(info.IsExhaustive);
        Assert.Contains("int", info.MissingCases);
        Assert.DoesNotContain("null", info.MissingCases);
    }

    [Fact]
    public void ParenthesizedUnionMatch_WithOnlyNullArm_ReportsMissingMemberDiagnostic()
    {
        const string source = """
func Test() -> int {
    let x2: Test2 = 1

    let r = match x2 {
        null => 2
    }

    return r
}

union Test2(int?)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive));

        Assert.Contains("int", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.DoesNotContain("null", diagnostic.GetMessage(), StringComparison.Ordinal);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.False(info.IsExhaustive);
        Assert.Collection(info.MissingCases, missing => Assert.Equal("int", missing));
    }

    [Fact]
    public void ParenthesizedUnionMatch_WithTypedMemberAndNull_IsExhaustive()
    {
        const string source = """
func Test() -> int {
    let x2: Test2 = 1

    return match x2 {
        int value => value
        null => 2
    }
}

union Test2(int?)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void GenericNominalUnionDeclaration_BindsNestedGenericMemberTypes()
    {
        const string source = """
import System.Collections.Generic.*

union MyResult2<T>(List<T> | int)
union MyResult3(List<int> | string)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var unions = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().ToArray();
        var genericUnion = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unions[0]));
        var concreteUnion = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unions[1]));

        var genericListMember = Assert.IsAssignableFrom<INamedTypeSymbol>(genericUnion.MemberTypes[0]);
        Assert.Equal("System.Collections.Generic.List`1", genericListMember.OriginalDefinition!.ToFullyQualifiedMetadataName());
        Assert.True(SymbolEqualityComparer.Default.Equals(genericUnion.TypeParameters[0], genericListMember.TypeArguments[0]));
        Assert.Equal(SpecialType.System_Int32, genericUnion.MemberTypes[1].SpecialType);

        var concreteListMember = Assert.IsAssignableFrom<INamedTypeSymbol>(concreteUnion.MemberTypes[0]);
        Assert.Equal("System.Collections.Generic.List`1", concreteListMember.OriginalDefinition!.ToFullyQualifiedMetadataName());
        Assert.Equal(SpecialType.System_Int32, concreteListMember.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_String, concreteUnion.MemberTypes[1].SpecialType);
    }

    [Fact]
    public void RecursiveNominalUnion_TargetsNestedCollectionLiteralThroughArrayMember()
    {
        const string source = """
import System.Collections.Generic.*

union Value(string | int | Node | Value[])
record Node(Entries: IDictionary<string, Value>)

func Create() -> Node {
    return Node([
        "name": 1,
        "children": [ "leaf", 2 ]
    ])
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var recursiveArrayMember = Assert.IsAssignableFrom<IArrayTypeSymbol>(
            unionSymbol.MemberTypes.Single(static member => member is IArrayTypeSymbol));

        Assert.True(SymbolEqualityComparer.Default.Equals(unionSymbol, recursiveArrayMember.ElementType));

        var creationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "Node" });
        var creation = Assert.IsType<BoundObjectCreationExpression>(model.GetBoundNode(creationSyntax));
        var dictionaryConversion = Assert.IsType<BoundConversionExpression>(Assert.Single(creation.Arguments));
        Assert.True(dictionaryConversion.Conversion.Exists);
        var conversionTargetType = Assert.IsAssignableFrom<INamedTypeSymbol>(dictionaryConversion.Type);
        Assert.Equal("IDictionary`2", conversionTargetType.MetadataName);
        Assert.Equal(SpecialType.System_String, conversionTargetType.TypeArguments[0].SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(unionSymbol, conversionTargetType.TypeArguments[1]));

        var dictionary = Assert.IsType<BoundDictionaryExpression>(dictionaryConversion.Expression);
        var dictionaryType = Assert.IsAssignableFrom<INamedTypeSymbol>(dictionary.Type);
        Assert.Equal("Dictionary`2", dictionaryType.MetadataName);
        Assert.Equal(SpecialType.System_String, dictionaryType.TypeArguments[0].SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(unionSymbol, dictionaryType.TypeArguments[1]));

        var arrayConversion = Assert.IsType<BoundConversionExpression>(dictionary.Elements
            .OfType<DictionaryEntryBinding>()
            .Select(static entry => entry.Value)
            .Single(static value => value is BoundConversionExpression conversion &&
                conversion.Expression.Type is IArrayTypeSymbol));
        Assert.True(SymbolEqualityComparer.Default.Equals(unionSymbol, arrayConversion.Type));

        var array = Assert.IsType<BoundCollectionExpression>(arrayConversion.Expression);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(array.Type);
        Assert.True(SymbolEqualityComparer.Default.Equals(unionSymbol, arrayType.ElementType));
    }

    [Fact]
    public void GenericNominalUnionDeclaration_PreservesTypeParameterConstraints()
    {
        const string source = """
import System.Collections.Generic.*

union MyResult<T>(List<T> | int)
    where T : class
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var typeParameter = Assert.Single(unionSymbol.TypeParameters);

        Assert.True((typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0);
        var listMember = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.MemberTypes[0]);
        Assert.True(SymbolEqualityComparer.Default.Equals(typeParameter, listMember.TypeArguments[0]));
    }

    [Fact]
    public void MemberAccess_BindsToUnionCaseType()
    {
        const string source = """
func create() {
    let option = Option.Some(value: 42)
}

union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        Assert.NotNull(invocation);
    }

    [Fact]
    public void MemberBindingInvocation_TargetTypedCase_BindsConstructor()
    {
        const string source = """
func build() {
    let option : Option = .Some(value: 42)
}

union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        Assert.NotNull(invocation);
    }

    [Fact]
    public void MemberBindingInvocation_TargetTypedGenericCase_BindsWithoutErrors()
    {
        const string source = """
func build<T>(payload: T) -> Option<T> {
    let option: Option<T> = .Some(payload)
    return option
}

union class Option<T> {
    case None
    case Some(value: T)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberBindingExpressionSyntax);

        Assert.NotNull(invocation);
    }

    [Fact]
    public void ParenthesizedUnion_NominalDeconstructionPattern_BindsWithoutCaseLookupDiagnostics()
    {
        const string source = """
record Cash(amount: decimal)
record Card(reference: string)

union Payment(Cash | Card)

func Describe(value: Payment) -> string {
    return match value {
        Cash(let amount) => "cash $amount"
        Card(let reference) => "card $reference"
    }
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void IdentifierInvocation_TargetTypedGenericUnionStructCase_BindsWithoutErrors()
    {
        const string source = """
namespace System

import System.*
import System.Option.*

union struct Option<T> {
    case Some(value: T)
    case None
}

class C<T> {
    func M(payload: T) -> Option<T> {
        let option: Option<T> = Some(payload)
        return option
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Some");

        Assert.NotNull(invocation);
    }

    [Fact]
    public void UnionMemberBody_UnqualifiedCaseConstructionAndPatterns_BindWithoutErrors()
    {
        const string source = """
import Option.*

union class Option<T> {
    case Some(value: T)
    case None

    func Normalize(fallback: T) -> Option<T> {
        let current: Option<T> = Some(fallback)

        return match self {
            Some(let value) => Some(value)
            None => current
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedUserUnionCaseInvocation_WithoutImport_ReportsMissingName()
    {
        const string source = """
func build() {
    let result = TempTooLow(12)
}

union class HeaterResult {
    case TempTooLow(value: int)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic =>
            diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id);
    }

    [Fact]
    public void UnqualifiedUserUnionCaseInvocation_WithWildcardImport_BindsWithoutErrors()
    {
        const string source = """
import HeaterResult.*

func build() -> HeaterResult {
    return TempTooLow(12)
}

union class HeaterResult {
    case TempTooLow(value: int)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedUserUnionCaseInvocation_WithGlobalWildcardImportInAnotherFile_BindsWithoutErrors()
    {
        var prelude = SyntaxTree.ParseText(
            """
            global {
                import HeaterResult.*
            }
            """,
            path: "Prelude.rvn");

        var source = SyntaxTree.ParseText(
            """
            func build() -> HeaterResult {
                return TempTooLow(12)
            }

            union HeaterResult {
                case TempTooLow(value: int)
            }
            """,
            path: "Main.rvn");

        var compilation = CreateCompilation([prelude, source], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void SourceImport_WhenAlreadyGloballyImported_ReportsRedundantImport()
    {
        var prelude = SyntaxTree.ParseText(
            """
            global {
                import HeaterResult.*
            }
            """,
            path: "Prelude.rvn");

        var source = SyntaxTree.ParseText(
            """
            import HeaterResult.*

            func build() -> HeaterResult {
                return TempTooLow(12)
            }

            union HeaterResult {
                case TempTooLow(value: int)
            }
            """,
            path: "Main.rvn");

        var compilation = CreateCompilation([prelude, source], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        var redundantImport = Assert.Single(diagnostics.Where(d => d.Id == CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id));
        Assert.Equal(DiagnosticSeverity.Hidden, redundantImport.Severity);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void UnqualifiedUserUnionCasePattern_WithoutImport_DoesNotBindAsCasePattern()
    {
        const string source = """
func describe(result: HeaterResult) -> int {
    return match result {
        TempTooLow(let temp) => temp
    }
}

union class HeaterResult {
    case TempTooLow(value: int)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic =>
            diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id &&
            diagnostic.GetMessage().Contains("TempTooLow", StringComparison.Ordinal));
    }

    [Fact]
    public void UnqualifiedUserUnionCasePattern_WithinDeclaringUnion_BindsWithoutImport()
    {
        const string source = """
union class HeaterResult {
    case TempTooLow(value: int)
    case Available

    func describe() -> int {
        return self match {
            TempTooLow(let temp) => temp
            Available => 0
        }
    }

    func isAvailable() -> bool => self is Available
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedUserUnionCasePattern_WithWildcardImport_BindsWithoutErrors()
    {
        const string source = """
import HeaterResult.*

func describe(result: HeaterResult) -> int {
    return match result {
        TempTooLow(let temp) => temp
    }
}

union class HeaterResult {
    case TempTooLow(value: int)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MemberAccessInvocation_OnUnconstructedCarrier_CaseArgumentsInferFromConstructor()
    {
        const string source = """
func build() {
    let result: Result<int, string> = Result.Ok(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax);

        Assert.NotNull(invocation);
    }

    [Fact]
    public void MemberAccessInvocation_OnUnconstructedCarrier_InLambdaReturn_BindsWithoutErrors()
    {
        const string source = """
import System.*

func build() {
    let factory: Func<int, Result<int, string>> = x => Result.Ok(x)
    let result: Result<int, string> = factory(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member &&
                            member.Name.Identifier.ValueText == "Ok");

        Assert.NotNull(invocation);
    }

    [Fact]
    public void MemberAccessInvocation_OnUnconstructedCarrier_InGenericLambdaReturn_BindsWithoutErrors()
    {
        const string source = """
import System.*

func build<T>(value: T) -> Result<T, string> {
    let factory: Func<T, Result<T, string>> = x => Result.Ok(x)
    return factory(value)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member &&
                            member.Name.Identifier.ValueText == "Ok");

        Assert.NotNull(invocation);
    }

    [Fact]
    public void UnionCaseCanonicalForms_BindWithoutErrors()
    {
        const string source = """
func build() {
    let resultA: Result<int, string> = .Ok(2)
    let resultB: Result<int, string> = .Ok<int>(2)
    let resultC: Result<int, string> = Result<int, string>.Ok(2)
    let resultD: Result<int, string> = .Ok(2)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedCaseInvocation_WithoutTargetType_ReportsDiagnostic()
    {
        const string source = """
import Result.*

func build() {
    let result = Ok(2)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d =>
            d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext &&
            d.GetMessage().Contains("Ok", StringComparison.Ordinal));
    }

    [Fact]
    public void UnqualifiedCaseInvocation_WithExplicitTargetType_BindsWithoutErrors()
    {
        const string source = """
func build() {
    let result: Result<int, string> = Ok(2)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MemberQualifiedCaseInvocation_WithoutTargetType_BindsWithoutErrors()
    {
        const string source = """
func build() {
    let caseValue = Result.Ok(2)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void ExhaustiveOptionMatch_DoesNotReportMissingCaseDiagnostic()
    {
        const string source = """
func format(option: Option<int>) -> string {
    return match option {
        .Some(let value) => "some ${value}"
        .None => "none"
    }
}

union class Option<T> {
    case Some(value: T)
    case None
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);
        Assert.True(diagnostics.All(d => d.Severity != DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MemberQualifiedCaseInvocation_WithExplicitCaseTypeArguments_BindsWithoutErrors()
    {
        const string source = """
func build() {
    let caseValue = Result.Ok<int>(2)
    let resultValue: Result<int, string> = Result.Ok<int>(2)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MemberQualifiedCaseInvocation_OnConstructedCarrier_PreservesConstructedTypeArguments()
    {
        const string source = """
func build() {
    let err = Result<int, string>.Error("boom")
    let result: Result<int, string> = err
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MatchWithCaseConstructors_ConvertsToDeclaredGenericUnionReturnType()
    {
        const string source = """
func build() -> Result<int, Err> {
    let value: int? = null

    return match value {
        null => .Error(.MissingName)
        let v => .Ok(v ?? 0)
    }
}

union Err {
    case MissingUser
    case MissingName
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MatchExpression_WithUserDefinedUnionCases_PrefersTargetUnionType()
    {
        const string source = """
func build(flag: bool) -> Response<int, string> {
    return match flag {
        true => .Ok(42)
        false => .Error("boom")
    }
}

union class Response<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchExpression));
        var matchType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundMatch.Type);
        Assert.Equal("Response", matchType.Name);
    }

    [Fact]
    public void IfExpression_InTypedLocal_TargetTypesUnionCaseBranches()
    {
        const string source = """
func build(needsAttention: bool) {
    let status: GreenhouseStatus = if needsAttention {
        .NeedsAttention(["Check ventilation"])
    } else {
        .OperatingNormally
    }
}

union GreenhouseStatus {
    case OperatingNormally
    case NeedsAttention(notices: string[])
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MatchStatement_ImplicitReturn_WithUserDefinedUnionCases_BindsWithoutErrors()
    {
        const string source = """
func build(flag: bool) -> Response<int, string> {
    match flag {
        true => .Ok(42)
        false => .Error("boom")
    }
}

union class Response<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var matchStatement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var boundStatement = Assert.IsType<BoundExpressionStatement>(model.GetBoundNode(matchStatement));
        var boundMatch = Assert.IsType<BoundMatchExpression>(boundStatement.Expression);
        var matchType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundMatch.Type);
        Assert.Equal("Response", matchType.Name);
    }

    [Fact]
    public void IfExpression_WithUserDefinedUnionCases_PrefersTargetUnionType()
    {
        const string source = """
func build(flag: bool) -> Response<int, string> {
    return if flag { .Ok(42) } else { .Error("boom") }
}

union Response<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var ifExpression = tree.GetRoot().DescendantNodes().OfType<IfExpressionSyntax>().Single();
        var boundIf = Assert.IsType<BoundIfExpression>(model.GetBoundNode(ifExpression));
        var ifType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundIf.Type);
        Assert.Equal("Response", ifType.Name);
    }

    [Fact]
    public void IfStatement_ImplicitReturn_WithUserDefinedUnionCases_BindsWithoutErrors()
    {
        const string source = """
func build(flag: bool) -> Response<int, string> {
    if flag {
        Response.Ok(42)
    } else {
        Response.Error("boom")
    }
}

union Response<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var ifStatement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var boundIf = Assert.IsType<BoundIfStatement>(model.GetBoundNode(ifStatement));
        Assert.NotNull(boundIf);
    }

    [Fact]
    public void UnionCaseCanonicalForms_PayloadAndParameterless_BindWithoutErrors()
    {
        const string source = """
func build() {
    let s1: Option<int> = .Some(1)
    let s2: Option<int> = .Some(2)
    let s3: Option<int> = Option.Some(3)
    let s4: Option<int> = Option<int>.Some(4)

    let n1: Option<int> = .None
    let n2: Option<int> = .None
}

union Option<T> {
    case Some(value: T)
    case None
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void TargetTypedParameterlessUnionCase_BindsInConstructorArgument()
    {
        const string source = """
func build() {
    let theme = Theme(.None)
    let theme2 = Theme(.None)
}

record Theme(PrimaryColor: Option<string>)

union Option<T> {
    case Some(value: T)
    case None
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void AsyncReturn_TargetTypedCase_BindsUnionCase()
    {
        const string source = """
import System.Threading.Tasks.*

async func fetch() -> Task<Result<string>> {
    return .Ok(value: "done")
}

union class Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static d => d.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        Assert.NotNull(invocation);
    }

    [Fact]
    public void PrintBoundTree_IncludesSynthesizedUnionMethodBodies()
    {
        const string source = """
union Token {
    case Identifier(text: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);
            model.PrintBoundTree(colorize: false, includeBinderInfo: false, includeBinderChainOnRoots: false);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        Assert.Contains("=== Synthesized Method Bodies ===", output);
        Assert.Contains("SynthesizedMethod=virtual override Token.ToString() -> string", output);
        Assert.Contains("SynthesizedMethod=virtual override Token.Identifier.ToString() -> string", output);
        Assert.Contains("<RavenFormatUnionValue>(value: object) -> string", output);
        Assert.DoesNotContain("<RavenUnionDisplayName>", output);
        Assert.DoesNotContain("<RavenFriendlyTypeName>", output);
        Assert.DoesNotContain("TypeOfExpression", output);
    }

    [Fact]
    public void PrintBoundTree_UsesReflectionFreeGenericUnionFormatting()
    {
        const string source = """
union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);
            model.PrintBoundTree(colorize: false, includeBinderInfo: false, includeBinderChainOnRoots: false, includeErrorNodes: true);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        Assert.Contains("SynthesizedMethod=virtual override Result<T, E>.ToString() -> string", output);
        Assert.Contains("FieldAccess [Type=Result<T, E>.Ok<T>, Symbol=Result<T, E>.<OkPayload>: Result<T, E>.Ok<T>, Field=Result<T, E>.<OkPayload>: Result<T, E>.Ok<T>]", output);
        Assert.Contains("<RavenFormatUnionValue>(value: object) -> string", output);
        Assert.DoesNotContain("<RavenUnionDisplayName>", output);
        Assert.DoesNotContain("<RavenFriendlyTypeName>", output);
        Assert.DoesNotContain("TypeOfExpression", output);
    }

    [Fact]
    public void Union_ToStringOverride_SuppressesSynthesizedToString()
    {
        const string source = """
union class Result<T> {
    case Ok(value: T)

    override func ToString() -> string {
        return "custom"
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var toStringMethods = unionSymbol.GetMembers("ToString").OfType<IMethodSymbol>()
            .Where(m => SymbolEqualityComparer.Default.Equals(m.ContainingType, unionSymbol))
            .ToArray();

        Assert.Single(toStringMethods);
    }

    [Fact]
    public void Union_EqualsGetHashCodeAndEqualityOperators_ReportDiagnostics()
    {
        const string source = """
union Result {
    case Ok

    override func Equals(other: object?) -> bool {
        return true
    }

    override func GetHashCode() -> int {
        return 42
    }

    static func ==(left: Result, right: Result) -> bool {
        return true
    }

    static func !=(left: Result, right: Result) -> bool {
        return false
    }
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionSpecialMemberNotSupported && d.GetMessage().Contains("Equals", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionSpecialMemberNotSupported && d.GetMessage().Contains("GetHashCode", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionSpecialMemberNotSupported && d.GetMessage().Contains("operator ==", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionSpecialMemberNotSupported && d.GetMessage().Contains("operator !=", StringComparison.Ordinal));
    }

    [Fact]
    public void Union_ReservedValueAndHasValueNames_ReportDiagnostics()
    {
        const string source = """
union Result {
    case Ok

    val Value: int = 1
    val HasValue: bool = true
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionMemberNameReserved && d.GetMessage().Contains("Value", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnionMemberNameReserved && d.GetMessage().Contains("HasValue", StringComparison.Ordinal));
    }

    [Fact]
    public void PrintBoundTree_IncludesSynthesizedUnionTryGetValueBody()
    {
        const string source = """
union Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);
            model.PrintBoundTree(colorize: false, includeBinderInfo: false, includeBinderChainOnRoots: false, includeErrorNodes: true);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        Assert.Contains("SynthesizedMethod=Result.TryGetValue(out value: Result.Ok) -> bool", output);
        Assert.Contains("ByRefAssignmentExpression [Type=(), ElementType=Result.Ok, UnitType=()]", output);
        Assert.Contains("ParameterAccess [Type=Result.Ok, Symbol=out value: Result.Ok, Parameter=out value: Result.Ok]", output);
        Assert.Contains("FieldAccess [Type=Result.Ok, Symbol=Result.<OkPayload>: Result.Ok, Field=Result.<OkPayload>: Result.Ok]", output);
    }

    [Fact]
    public void PrintBoundTree_IncludesSynthesizedUnionCaseDeconstructBody()
    {
        const string source = """
union Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);
            model.PrintBoundTree(colorize: false, includeBinderInfo: false, includeBinderChainOnRoots: false, includeErrorNodes: true);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        Assert.Contains("SynthesizedMethod=Result.Ok.Deconstruct(out Value: int) -> ()", output);
        Assert.Contains("ByRefAssignmentExpression [Type=(), ElementType=int, UnitType=()]", output);
        Assert.Contains("Symbol=Result.Ok.get_Value() -> int", output);
    }

    [Fact]
    public void UnionCasePropertyGetter_HasSynthesizedBody()
    {
        const string source = """
union Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var resultType = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var okType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultType.CaseTypes.Single(c => c.Name == "Ok"));
        var valueProperty = okType.GetMembers("Value").OfType<IPropertySymbol>().Single();
        var getter = Assert.IsAssignableFrom<IMethodSymbol>(valueProperty.GetMethod);

        Assert.True(compilation.TryGetSynthesizedMethodBody(getter, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        var returnStatement = Assert.IsType<BoundReturnStatement>(Assert.Single(body!.Statements));
        var fieldAccess = Assert.IsType<BoundFieldAccess>(returnStatement.Expression);
        Assert.Equal("<value>k__BackingField", fieldAccess.Field.Name);
    }

    [Fact]
    public void UnionValueProperty_HasSynthesizedBody()
    {
        const string source = """
union Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var valueProperty = Assert.Single(unionSymbol.GetMembers("Value").OfType<IPropertySymbol>().Where(p => SymbolEqualityComparer.Default.Equals(p.ContainingType, unionSymbol)));
        var getter = Assert.IsAssignableFrom<IMethodSymbol>(valueProperty.GetMethod);

        Assert.True(compilation.TryGetSynthesizedMethodBody(getter, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement => Assert.IsType<BoundIfStatement>(statement),
            statement => Assert.IsType<BoundIfStatement>(statement),
            statement =>
            {
                var returnStatement = Assert.IsType<BoundReturnStatement>(statement);
                Assert.IsType<BoundLiteralExpression>(returnStatement.Expression);
            });
    }

    [Fact]
    public void UnionValueProperty_Type_IsNullableObjectForClassUnionCases()
    {
        const string source = """
union Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var valueProperty = Assert.Single(unionSymbol.GetMembers("Value").OfType<IPropertySymbol>().Where(p => SymbolEqualityComparer.Default.Equals(p.ContainingType, unionSymbol)));

        Assert.Equal("object?", valueProperty.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.True(valueProperty.Type.IsNullable);
    }

    [Fact]
    public void UnionValueProperty_Type_IsNullableObjectForStructUnion()
    {
        const string source = """
union struct Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var valueProperty = Assert.Single(unionSymbol.GetMembers("Value").OfType<IPropertySymbol>().Where(p => SymbolEqualityComparer.Default.Equals(p.ContainingType, unionSymbol)));

        Assert.Equal("object?", valueProperty.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.True(valueProperty.Type.IsNullable);
    }

    [Fact]
    public void UnionValueProperty_Type_IsNullableObjectForClassUnionWithNullableMember()
    {
        const string source = """
union Maybe(string? | int)
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var valueProperty = Assert.Single(unionSymbol.GetMembers("Value").OfType<IPropertySymbol>().Where(p => SymbolEqualityComparer.Default.Equals(p.ContainingType, unionSymbol)));

        Assert.Equal("object?", valueProperty.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.True(valueProperty.Type.IsNullable);
    }

    [Fact]
    public void UnionHasValueProperty_HasSynthesizedBody()
    {
        const string source = """
union struct Result {
    case Ok(value: int)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var hasValueProperty = Assert.Single(unionSymbol.GetMembers("HasValue").OfType<IPropertySymbol>().Where(p => SymbolEqualityComparer.Default.Equals(p.ContainingType, unionSymbol)));
        var getter = Assert.IsAssignableFrom<IMethodSymbol>(hasValueProperty.GetMethod);

        Assert.Equal("bool", hasValueProperty.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.True(compilation.TryGetSynthesizedMethodBody(getter, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        var returnStatement = Assert.IsType<BoundReturnStatement>(Assert.Single(body!.Statements));
        Assert.IsAssignableFrom<BoundExpression>(returnStatement.Expression);
    }

    [Fact]
    public void StructUnionMatch_ParameterAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_FieldAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
class Holder {
    val current: Result<int> = .Ok(1)

    func format() -> string {
        return match self.current {
            .Ok(let payload) => payload.ToString()
            .Error(let message) => message
        }
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_PropertyAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
class Holder {
    val Current: Result<int> {
        get {
            .Ok(1)
        }
    }

    func format() -> string {
        return match self.Current {
            .Ok(let payload) => payload.ToString()
            .Error(let message) => message
        }
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_DefaultLocalAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format() -> string {
    let result: Result<int> = default

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_DefaultLocalReassignedToActiveValueIsExhaustive()
    {
        const string source = """
func format() -> string {
    var result: Result<int> = default
    result = .Ok(1)

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_BranchAssignedDefaultAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format(useDefault: bool) -> string {
    var result: Result<int> = .Ok(1)

    if useDefault {
        result = default
    }

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_NestedMatchAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format(useNested: bool) -> string {
    var result: Result<int> = default

    if useNested {
        return match result {
            .Ok(let payload) => payload.ToString()
            .Error(let message) => message
        }
    }

    return ""
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_WhileAssignedDefaultAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format(useDefault: bool) -> string {
    var result: Result<int> = .Ok(1)

    while useDefault {
        result = default
        break
    }

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_ForAssignedDefaultAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format(values: int[]) -> string {
    var result: Result<int> = .Ok(1)

    for value in values {
        result = default
    }

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_TryAssignedDefaultAllCasesCoveredIsSourceExhaustive()
    {
        const string source = """
func format() -> string {
    var result: Result<int> = .Ok(1)

    try {
        result = default
    } catch {
    }

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_FinallyAssignedActiveValueIsExhaustive()
    {
        const string source = """
func format() -> string {
    var result: Result<int> = default

    try {
        result = default
    } finally {
        result = .Ok(1)
    }

    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionMatch_ParameterCatchAllArmReportsRedundant()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
        _ => "uninitialized"
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        Assert.Contains(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.MatchExpressionCatchAllRedundant);

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var info = model.GetMatchExhaustiveness(matchExpression);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void StructUnionArgument_DefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run() {
    consume(default)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_DefaultLocalReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run() {
    let result: Result<int> = default
    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_ReassignedActiveLocalDoesNotReportInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run() {
    var result: Result<int> = default
    result = .Ok(1)
    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault);
    }

    [Fact]
    public void StructUnionArgument_ParameterForwardingDoesNotReportInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func forward(result: Result<int>) {
    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault);
    }

    [Fact]
    public void StructUnionArgument_NamedDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run() {
    consume(result: default)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_OmittedDefaultOptionalReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int> = default) {
}

func run() {
    consume()
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_InstanceMethodDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
class Sink {
    func consume(result: Result<int>) {
    }
}

func run() {
    let sink = Sink()
    sink.consume(default)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_ConstructorDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
class Box(result: Result<int>) {
}

func run() {
    Box(default)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_ParamsElementDefaultLocalReportsInactiveDefaultState()
    {
        const string source = """
func consumeAll(results: Result<int> ...) {
}

func run() {
    let result: Result<int> = default
    consumeAll(.Ok(1), result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("results", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_GenericMethodDefaultLocalReportsInactiveDefaultState()
    {
        const string source = """
func consume<T>(result: Result<T>) {
}

func run() {
    let result: Result<int> = default
    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_ExtensionReceiverDefaultLocalReportsInactiveDefaultState()
    {
        const string source = """
extension ResultExtensions<T> for Result<T> {
    func Touch() -> unit {
    }
}

func run() {
    let result: Result<int> = default
    result.Touch()
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_DelegateInvocationDefaultLocalReportsInactiveDefaultState()
    {
        const string source = """
func run() {
    let sink: Result<int> -> unit = value => ()
    let result: Result<int> = default
    sink(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_FieldAccessReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

class Holder {
    val current: Result<int> = .Ok(1)

    func run() {
        consume(self.current)
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_PropertyAccessReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

class Holder {
    val Current: Result<int> {
        get {
            .Ok(1)
        }
    }

    func run() {
        consume(self.Current)
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_IfExpressionAllBranchesActiveDoesNotReportInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run(flag: bool) {
    let result: Result<int> = if flag { .Ok(1) } else { .Error("boom") }
    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault);
    }

    [Fact]
    public void StructUnionArgument_IfExpressionDefaultBranchReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run(flag: bool) {
    let result: Result<int> = if flag { .Ok(1) } else { default }
    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionArgument_IfStatementAllBranchesActiveDoesNotReportInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run(flag: bool) {
    var result: Result<int> = default

    if flag {
        result = .Ok(1)
    } else {
        result = .Error("boom")
    }

    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault);
    }

    [Fact]
    public void StructUnionArgument_IfStatementDefaultBranchReportsInactiveDefaultState()
    {
        const string source = """
func consume(result: Result<int>) {
}

func run(flag: bool) {
    var result: Result<int> = .Ok(1)

    if flag {
        result = .Error("boom")
    } else {
        result = default
    }

    consume(result)
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionArgumentMayBeDefault));
        Assert.Contains("result", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_DefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
func make() -> Result<int> {
    return default
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_DefaultLocalReportsInactiveDefaultState()
    {
        const string source = """
func make() -> Result<int> {
    let result: Result<int> = default
    return result
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_ReassignedActiveLocalDoesNotReportInactiveDefaultState()
    {
        const string source = """
func make() -> Result<int> {
    var result: Result<int> = default
    result = .Ok(1)
    return result
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault);
    }

    [Fact]
    public void StructUnionReturn_ParameterDoesNotReportInactiveDefaultState()
    {
        const string source = """
func forward(result: Result<int>) -> Result<int> {
    return result
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault);
    }

    [Fact]
    public void StructUnionReturn_ParameterMatchReconstructionDoesNotReportInactiveDefaultState()
    {
        const string source = """
func forward(result: Result<int>) -> Result<int> {
    return match result {
        .Ok(let value) => .Ok(value)
        .Error(let message) => .Error(message)
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault);
        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);
        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.MatchExpressionCatchAllRedundant);
    }

    [Fact]
    public void StructUnionReturn_MatchExpressionAllBranchesActiveDoesNotReportInactiveDefaultState()
    {
        const string source = """
func make(flag: bool) -> Result<int> {
    return match flag {
        true => .Ok(1)
        false => .Error("boom")
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        Assert.DoesNotContain(compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault);
    }

    [Fact]
    public void StructUnionReturn_MatchExpressionDefaultBranchReportsInactiveDefaultState()
    {
        const string source = """
func make(flag: bool) -> Result<int> {
    return match flag {
        true => .Ok(1)
        false => default
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_ImplicitDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
func make() -> Result<int> {
    default
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_ArrowBodyDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
func make() -> Result<int> => default

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_FieldAccessReportsInactiveDefaultState()
    {
        const string source = """
class Holder {
    val current: Result<int> = .Ok(1)

    func make() -> Result<int> {
        return self.current
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_PropertyAccessReportsInactiveDefaultState()
    {
        const string source = """
class Holder {
    val Current: Result<int> {
        get {
            .Ok(1)
        }
    }

    func make() -> Result<int> {
        return self.Current
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_PropertyGetterDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
class Holder {
    val Current: Result<int> {
        get {
            default
        }
    }
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_AsyncDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
import System.Threading.Tasks.*

async func make() -> Task<Result<int>> {
    await Task.FromResult(0)
    return default
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void StructUnionReturn_LambdaDefaultLiteralReportsInactiveDefaultState()
    {
        const string source = """
func run() {
    let factory: () -> Result<int> = () -> Result<int> => default
}

union struct Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(static d => d.Descriptor == CompilerDiagnostics.StructUnionReturnMayBeDefault));
        Assert.Contains("Result<int>", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void UnionCarrierConstructor_HasSynthesizedBody()
    {
        const string source = """
union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var someCase = unionSymbol.CaseTypes.Single(c => c.Name == "Some");
        var constructor = unionSymbol
            .GetMembers(".ctor")
            .OfType<IMethodSymbol>()
            .Single(m => m.Parameters.Length == 1 &&
                         SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, someCase));

        Assert.True(compilation.TryGetSynthesizedMethodBody(constructor, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<Tag>", fieldAssignment.Field.Name);
                var value = Assert.IsType<BoundLiteralExpression>(fieldAssignment.Right);
                Assert.Equal((byte)2, value.Value);
            },
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<SomePayload>", fieldAssignment.Field.Name);
            },
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }

    [Fact]
    public void UnionCaseConstructor_HasSynthesizedBody()
    {
        const string source = """
union Option {
    case Some(value: int, label: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var someCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Some"));
        var constructor = someCase.GetMembers(".ctor").OfType<IMethodSymbol>().Single();

        Assert.True(compilation.TryGetSynthesizedMethodBody(constructor, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<value>k__BackingField", fieldAssignment.Field.Name);
            },
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<label>k__BackingField", fieldAssignment.Field.Name);
            },
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }

    [Fact]
    public void UnqualifiedCaseInvocation_BindsWhenUniqueInScope()
    {
        const string source = """
func create() -> Result<int, string> {
    return .Ok(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedCaseInvocation_InAsyncTaskResultReturn_BindsWithoutErrors()
    {
        const string source = """
import System.*
import System.Net.Http.*
import System.Threading.Tasks.*

async func fetch(url: string) -> Task<Result<string, string>> {
    use client = HttpClient()

    try {
        use response = await client.GetAsync(url)
        response.EnsureSuccessStatusCode()
        let responseBody = await response.Content.ReadAsStringAsync()
        return .Ok(responseBody)
    } catch (HttpRequestException e) {
        return .Error(e.Message)
    } catch (TaskCanceledException) {
        return .Error("Request timed out or was canceled.")
    }
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedCaseInvocation_ReportsAmbiguousWhenMultipleCasesMatch()
    {
        const string source = """
import Result.*
import Option.*

class C {
    func create() {
        var value = Ok(42)
    }
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}

union Option<T> {
    case Ok(value: T)
    case None
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        _ = model.GetBoundNode(invocation);

        var diagnostics = model.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
    }

    [Fact]
    public void UnqualifiedGenericCaseInvocation_BindsWhenUniqueInScope()
    {
        const string source = """
func create() -> Result<int, string> {
    return .Ok<int>(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedGenericCaseInvocation_ReportsAmbiguousWhenMultipleCasesMatch()
    {
        const string source = """
import Result.*
import Option.*

class C {
    func create() {
        var value = Ok<int>(42)
    }
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}

union Option<T> {
    case Ok(value: T)
    case None
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single(s => s.Expression is GenericNameSyntax);
        _ = model.GetBoundNode(invocation);

        var diagnostics = model.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
    }

    [Fact]
    public void UnqualifiedCaseInvocation_ReportsAmbiguousWhenUnionCasesAreWildcardImported()
    {
        const string source = """
namespace A {
    union Result<T, E> {
        case Ok(value: T)
        case Error(message: E)
    }
}

namespace B {
    union Option<T> {
        case Ok(value: T)
        case None
    }
}

import A.Result.*
import B.Option.*

class C {
    func create() {
        var value = Ok(42)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        _ = model.GetBoundNode(invocation);

        var diagnostics = model.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
    }

    [Fact]
    public void UnqualifiedCaseInvocation_AmbiguousDiagnosticMessage_IncludesUnionCarrierName()
    {
        // Verify that the ambiguity diagnostic message uses the carrier union name in the format
        // "UnionName<TypeParams>.CaseName" rather than just "CaseName<TypeParams>".
        const string source = """
import Result.*
import Option.*

class C {
    func create() {
        var value = Ok(42)
    }
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}

union Option<T> {
    case Ok(value: T)
    case None
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        _ = model.GetBoundNode(invocation);

        var diagnostics = model.GetDiagnostics();
        var ambiguousDiag = diagnostics.Single(d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
        var message = ambiguousDiag.GetMessage();

        // Both candidates should be displayed as "UnionName<TypeParams>.CaseName"
        Assert.Contains("Result<T, E>.Ok", message, StringComparison.Ordinal);
        Assert.Contains("Option<T>.Ok", message, StringComparison.Ordinal);
    }

    [Fact]
    public void UnqualifiedCaseInvocation_AmbiguousDiagnosticMessage_IncludesNamespaceWhenUnionNamesCollide()
    {
        // When two union carriers share the same short name but live in different namespaces,
        // the diagnostic message should include the namespace to disambiguate.
        const string source = """
namespace A {
    union Result<T, E> {
        case Ok(value: T)
        case Error(message: E)
    }
}

namespace B {
    union Result<T> {
        case Ok(value: T)
        case None
    }
}

import A.Result.*
import B.Result.*

class C {
    func create() {
        var value = Ok(42)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        _ = model.GetBoundNode(invocation);

        var diagnostics = model.GetDiagnostics();
        var ambiguousDiag = diagnostics.Single(d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
        var message = ambiguousDiag.GetMessage();

        // Since both carriers are named 'Result', namespace must be included for disambiguation.
        Assert.Contains("A.Result<T, E>.Ok", message, StringComparison.Ordinal);
        Assert.Contains("B.Result<T>.Ok", message, StringComparison.Ordinal);
    }

    [Fact]
    public void AliasToUnionCaseType_BindsAndConvertsToCarrier()
    {
        const string source = """
alias ResultOk = Result.Ok

func create() -> Result<int, string> {
    return ResultOk(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void AliasToUnionCaseType_WithGenericArguments_BindsAndConvertsToCarrier()
    {
        const string source = """
alias ResultOk = Result.Ok

func create() -> Result<int, string> {
    return ResultOk<int>(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void AliasToClosedGenericUnionCaseType_ReportsInvalidAliasTarget()
    {
        const string source = """
alias ResultOk = Result<int, string>.Ok

func create() -> Result<int, string> {
    return ResultOk(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidAliasType);
    }

    [Fact]
    public void Union_DoesNotDeclareImplicitConversionPerCase()
    {
        const string source = """
union Option {
    case None
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var conversionMethods = unionSymbol
            .GetMembers("op_Implicit")
            .OfType<IMethodSymbol>()
            .ToArray();

        Assert.Empty(conversionMethods);
    }

    [Fact]
    public void GenericUnionCases_UseOnlyReferencedTypeParameters()
    {
        const string source = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
    case Pending
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Ok"));
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Error"));
        var pendingCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Pending"));

        Assert.Equal(1, okCase.Arity);
        Assert.Equal(1, errorCase.Arity);
        Assert.Equal(0, pendingCase.Arity);
    }

    [Fact]
    public void UnionCaseParameters_ComplexTypes_BindViaBindTypeSyntaxPath()
    {
        const string source = """
union Payloads {
    case Callback(fn: (int, string) -> bool)
    case Pair(value: (left: int, right: string))
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var callbackCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Callback"));
        var callbackCtor = callbackCase.InstanceConstructors.Single();
        var callbackType = Assert.IsAssignableFrom<INamedTypeSymbol>(callbackCtor.Parameters[0].Type);
        Assert.Equal(TypeKind.Delegate, callbackType.TypeKind);
        var invoke = callbackType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);
        Assert.Equal(2, invoke.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);

        var pairCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Pair"));
        var pairCtor = pairCase.InstanceConstructors.Single();
        var tupleType = Assert.IsAssignableFrom<ITupleTypeSymbol>(pairCtor.Parameters[0].Type);
        Assert.Equal("left", tupleType.TupleElements[0].Name);
        Assert.Equal(SpecialType.System_Int32, tupleType.TupleElements[0].Type.SpecialType);
        Assert.Equal("right", tupleType.TupleElements[1].Name);
        Assert.Equal(SpecialType.System_String, tupleType.TupleElements[1].Type.SpecialType);
    }

    [Fact]
    public void ConstructedGenericUnionCases_ProjectConcreteTypeArguments()
    {
        const string source = """
union Result {
    case Ok(value: int)
    case Error(error: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var constructedUnion = Assert.IsAssignableFrom<IUnionSymbol>(
            unionSymbol.Construct(
                compilation.GetSpecialType(SpecialType.System_Int32),
                compilation.GetSpecialType(SpecialType.System_String)));

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedUnion.CaseTypes.Single(c => c.Name == "Ok"));
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedUnion.CaseTypes.Single(c => c.Name == "Error"));

        Assert.Equal(SpecialType.System_Int32, okCase.Constructors.Single().Parameters.Single().Type.SpecialType);
        Assert.Equal(SpecialType.System_String, errorCase.Constructors.Single().Parameters.Single().Type.SpecialType);
    }

    [Fact]
    public void UnionCase_UsesLogicalNameAndScopedMetadataName()
    {
        const string source = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Ok"));
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single(c => c.Name == "Error"));

        Assert.Equal("Ok", okCase.Name);
        Assert.Equal("Ok`1", okCase.MetadataName);
        Assert.Equal("Error", errorCase.Name);
        Assert.Equal("Error`1", errorCase.MetadataName);
    }

    [Fact]
    public void CaseParameters_AreExposedAsGetterOnlyProperties()
    {
        const string source = """
union Option {
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.CaseTypes.Single();

        var property = caseSymbol.GetMembers("Value").OfType<IPropertySymbol>().Single();
        Assert.Equal(Accessibility.Public, property.DeclaredAccessibility);
        Assert.NotNull(property.GetMethod);
        Assert.Null(property.SetMethod);
        Assert.Equal(SpecialType.System_Int32, property.Type.SpecialType);

        var backingField = caseSymbol.GetMembers().OfType<IFieldSymbol>()
            .Single(f => f.Name == "<value>k__BackingField");
        Assert.Equal(Accessibility.Private, backingField.DeclaredAccessibility);
    }

    [Fact]
    public void UnitLikeCase_HasNoPayloadPropertiesOrDeconstructMethod()
    {
        const string source = """
union Option {
    case None
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single());
        var constructor = caseSymbol.InstanceConstructors.Single();

        Assert.Empty(constructor.Parameters);
        Assert.Empty(caseSymbol.GetMembers().OfType<IPropertySymbol>());
        Assert.Empty(caseSymbol.GetMembers("Deconstruct").OfType<IMethodSymbol>());
    }

    [Fact]
    public void PositionalCaseParameters_ProjectCamelCaseToPascalCasePropertiesAndDeconstructParameters()
    {
        const string source = """
union Status {
    case Open(reason: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single());
        var constructor = caseSymbol.InstanceConstructors.Single();

        var parameter = Assert.Single(constructor.Parameters);
        Assert.Equal("reason", parameter.Name);
        Assert.Equal(SpecialType.System_String, parameter.Type.SpecialType);

        var property = caseSymbol.GetMembers("Reason").OfType<IPropertySymbol>().Single();
        Assert.Equal(SpecialType.System_String, property.Type.SpecialType);
        Assert.Null(property.SetMethod);
        Assert.Empty(caseSymbol.GetMembers("reason").OfType<IPropertySymbol>());

        var deconstruct = caseSymbol.GetMembers("Deconstruct").OfType<IMethodSymbol>().Single();
        var deconstructParameter = Assert.Single(deconstruct.Parameters);
        Assert.Equal("Reason", deconstructParameter.Name);
        Assert.Equal(RefKind.Out, deconstructParameter.RefKind);
        Assert.Equal(SpecialType.System_String, deconstructParameter.Type.SpecialType);
    }

    [Fact]
    public void StructLikeCaseFields_AreExposedAsConstructorParametersAndGetterOnlyProperties()
    {
        const string source = """
union Status {
    case Closed {
        Code: int
        Reason: string? = null
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.CaseTypes.Single());
        var constructor = caseSymbol.InstanceConstructors.Single();

        Assert.Collection(
            constructor.Parameters,
            code =>
            {
                Assert.Equal("Code", code.Name);
                Assert.Equal(SpecialType.System_Int32, code.Type.SpecialType);
                Assert.False(code.HasExplicitDefaultValue);
            },
            reason =>
            {
                Assert.Equal("Reason", reason.Name);
                Assert.Equal(SpecialType.System_String, reason.Type.GetNonNullableType().SpecialType);
                Assert.True(reason.Type.IsNullable);
                Assert.True(reason.HasExplicitDefaultValue);
                Assert.Null(reason.ExplicitDefaultValue);
            });

        var codeProperty = caseSymbol.GetMembers("Code").OfType<IPropertySymbol>().Single();
        var reasonProperty = caseSymbol.GetMembers("Reason").OfType<IPropertySymbol>().Single();
        Assert.Null(codeProperty.SetMethod);
        Assert.Null(reasonProperty.SetMethod);

        var deconstruct = caseSymbol.GetMembers("Deconstruct").OfType<IMethodSymbol>().Single();
        Assert.Collection(
            deconstruct.Parameters,
            code =>
            {
                Assert.Equal("Code", code.Name);
                Assert.Equal(RefKind.Out, code.RefKind);
                Assert.Equal(SpecialType.System_Int32, code.Type.SpecialType);
            },
            reason =>
            {
                Assert.Equal("Reason", reason.Name);
                Assert.Equal(RefKind.Out, reason.RefKind);
                Assert.Equal(SpecialType.System_String, reason.Type.GetNonNullableType().SpecialType);
                Assert.True(reason.Type.IsNullable);
            });
    }

    [Fact]
    public void StructLikeCaseFieldDefaults_MayPrecedeRequiredFields()
    {
        const string source = """
union Status {
    case Closed {
        Reason: string? = null
        Code: int
    }
}

class Factory {
    func Create() -> Status {
        return .Closed {
            Code = 7
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        _ = unionSymbol.CaseTypes.ToArray();
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void StructLikeCaseConstruction_BindsTrailingAssignmentsAsNamedConstructorArguments()
    {
        const string source = """
union Status {
    case Closed {
        Code: int
        Reason: string? = null
    }
}

class Factory {
    func Create() -> Status {
        return .Closed {
            Code = 7
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;
        var returnStatement = boundBody.Statements.OfType<BoundReturnStatement>().Single();
        var unionCaseExpression = Assert.IsType<BoundUnionCaseExpression>(returnStatement.Expression);

        Assert.Equal("Closed", unionCaseExpression.CaseType.Name);
        Assert.Equal(2, unionCaseExpression.Arguments.Length);
        Assert.Equal(SpecialType.System_Int32, unionCaseExpression.Arguments[0].Type.SpecialType);
        Assert.True(unionCaseExpression.Arguments[1] is BoundLiteralExpression { Value: null });
    }

    [Fact]
    public void StructLikeCaseConstruction_ReportsMissingRequiredField()
    {
        const string source = """
union Status {
    case Closed {
        Code: int
        Reason: string? = null
    }
}

class Factory {
    func Create() -> Status {
        return .Closed {
            Reason = "done"
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }

    [Fact]
    public void SynthesizedUnionMembers_AreRegisteredOnUnionAndCaseSymbols()
    {
        const string source = """
union Option {
    case Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.CaseTypes.Single();

        var constructor = Assert.Single(caseSymbol.GetMembers(".ctor").OfType<IMethodSymbol>());
        Assert.DoesNotContain(unionSymbol.GetMembers(".ctor"), m => SymbolEqualityComparer.Default.Equals(m, constructor));

        var payloadProperty = Assert.Single(caseSymbol.GetMembers().OfType<IPropertySymbol>());
        Assert.DoesNotContain(unionSymbol.GetMembers(payloadProperty.Name), m => SymbolEqualityComparer.Default.Equals(m, payloadProperty));

        var getter = payloadProperty.GetMethod;
        Assert.NotNull(getter);
        Assert.DoesNotContain(unionSymbol.GetMembers(getter!.Name), m => SymbolEqualityComparer.Default.Equals(m, getter));

        var caseToString = Assert.Single(caseSymbol.GetMembers("ToString").OfType<IMethodSymbol>());
        Assert.DoesNotContain(unionSymbol.GetMembers("ToString"), m => SymbolEqualityComparer.Default.Equals(m, caseToString));

        var tryGet = unionSymbol
            .GetMembers("TryGetValue")
            .OfType<IMethodSymbol>()
            .Single(m => SymbolEqualityComparer.Default.Equals(m.Parameters.Single().GetByRefElementType(), caseSymbol));
        Assert.DoesNotContain(caseSymbol.GetMembers("TryGetValue"), m => SymbolEqualityComparer.Default.Equals(m, tryGet));
    }

    [Fact]
    public void CaseToUnionConversion_ClassifiedAsDiscriminatedUnion()
    {
        const string source = """
union Option<T> {
    case None
    case Some(value: T)
}

class Container {
    func Create() -> Option<int> {
        return Option.Some(value: 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.CaseTypes.Single(c => c.Name == "Some");

        var constructedUnion = (INamedTypeSymbol)unionSymbol.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var conversion = compilation.ClassifyConversion(caseSymbol, constructedUnion);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUnion);
        Assert.False(conversion.IsUserDefined);
        Assert.Null(conversion.MethodSymbol);
        Assert.NotNull(conversion.ConstructorSymbol);
    }

    [Fact]
    public void Lowerer_LowersUnionCaseToConstructorAndConversion()
    {
        const string source = """
union Option {
    case None
    case Some(value: int)
}

class Container {
    func Create() -> Option {
        return Option.Some(value: 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;
        var returnStatement = boundBody.Statements.OfType<BoundReturnStatement>().Single();
        var unionCaseExpression = Assert.IsType<BoundUnionCaseExpression>(returnStatement.Expression);
        Assert.Equal("Option", unionCaseExpression.UnionType.Name);
        Assert.Equal("Some", unionCaseExpression.CaseType.Name);

        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);
        var invocations = CollectInvocationExpressions(loweredBody);
        Assert.DoesNotContain(invocations, invocation => invocation.Method.Name == "Create");
    }

    [Fact]
    public void UnionSymbol_ExposesCaseTypedConstructors()
    {
        const string source = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        foreach (var caseSymbol in unionSymbol.CaseTypes)
        {
            Assert.Contains(
                unionSymbol.Constructors,
                constructor => constructor.Parameters.Length == 1 &&
                               string.Equals(
                                   constructor.Parameters[0].Type.TryGetUnionCase()?.Name,
                                   caseSymbol.Name,
                                   StringComparison.Ordinal));
        }
    }

    [Fact]
    public void UnionSymbol_DoesNotExposeImplicitParameterlessConstructor()
    {
        const string source = """
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        Assert.DoesNotContain(
            unionSymbol.InstanceConstructors,
            constructor => !constructor.IsStatic && constructor.Parameters.Length == 0);
    }

    [Fact]
    public void CasePattern_BindsPayloadType()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union class Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_ImplicitPayloadDesignations_BindLocals()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union class Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_ImplicitPayloadDesignations_InMatch_BindLocals()
    {
        const string source = """
func describe(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union class Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_UnqualifiedCaseAndDeconstruction_BindWithoutErrors()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
        .Error(let message) => message
    }
}

union class Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_DeconstructsUnitPositionalAndStructLikeDeclaredCases()
    {
        const string source = """
func describe(status: Status) -> string {
    return match status {
        .Unknown => "unknown"
        .Open(let openReason) => openReason
        .Closed(let closedReason, let closedCode) => closedReason + closedCode.ToString()
    }
}

union Status {
    case Unknown
    case Open(reason: string)
    case Closed {
        Reason: string
        Code: int
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var designations = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .ToDictionary(designation => designation.Identifier.ValueText);

        var openReason = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designations["openReason"]));
        var closedReason = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designations["closedReason"]));
        var closedCode = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designations["closedCode"]));

        Assert.Equal("string", openReason.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("string", closedReason.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("int", closedCode.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void ParenthesizedUnionPattern_DeconstructsNominalAlternativesThroughTheirOwnShape()
    {
        const string source = """
record Cash(Amount: decimal)
record Card(Reference: string)

union Payment(Cash | Card)

func describe(payment: Payment) -> string {
    return match payment {
        Cash(let amount) => amount.ToString()
        Card(let reference) => reference
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var designations = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .ToDictionary(designation => designation.Identifier.ValueText);

        var amount = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designations["amount"]));
        var reference = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designations["reference"]));

        Assert.Equal("decimal", amount.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("string", reference.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void CasePattern_UnitLikeCaseWithPayloadReportsArgumentCountMismatch()
    {
        const string source = """
func describe(status: Status) -> string {
    return match status {
        .Unknown(let payload) => payload.ToString()
        .Open(let reason) => reason
    }
}

union Status {
    case Unknown
    case Open(reason: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CasePatternArgumentCountMismatch);
    }

    [Fact]
    public void CasePattern_UnqualifiedSingleArm_BindsFromImportedCaseAndReportsMissingCase()
    {
        const string source = """
import Result.*

func format(result: Result<int>) -> string {
    return match result {
        Ok(let payload) => payload.ToString()
    }
}

union Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAV2100", diagnostic.Descriptor.Id);
        Assert.Contains("Error", diagnostic.GetMessage());
    }

    [Fact]
    public void CasePattern_NonGenericUnion_BindsTryGetMethods()
    {
        const string source = """
func describe(value: Test) -> string {
    return match value {
        .Something(let text) => text
        .Nothing => "none"
    }
}

union class Test {
    case Something(value: string)
    case Nothing
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        Assert.Equal(2, unionSymbol.GetMembers("TryGetValue").OfType<IMethodSymbol>().Count());
    }

    [Fact]
    public void CasePattern_MissingArm_ReportsExhaustivenessDiagnostic()
    {
        const string source = """
func describe(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) => payload.ToString()
    }
}

union Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.IsEmpty || diagnostics.Any(d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive),
            string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_WithGuard_RemainsInExhaustivenessCheck()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok(let payload) when payload > 1 => "ok ${payload}"
        .Error(let message) => "error ${message}"
    }
}

union Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.IsEmpty || diagnostics.Any(d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive),
            string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_WithPartialArguments_IsNotExhaustive()
    {
        const string source = """
func area(shape: Shape) -> int {
    return match shape {
        .Circle(let r) => r * r * 3
        .Rectangle(4, let h) => 42
    }
}

union Shape {
    case Circle(radius: int)
    case Rectangle(width: int, height: int)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.IsEmpty || diagnostics.Any(d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive),
            string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_ReportsArgumentCountMismatch()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return match result {
        .Ok() => "ok"
        _ => "none"
    }
}

union Result<T> {
    case Ok(value: T)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.IsEmpty || diagnostics.Any(d => d.Descriptor == CompilerDiagnostics.CasePatternArgumentCountMismatch),
            string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedCaseConstructor_ExplicitReturn_MatchingTypeArgs_NoErrors()
    {
        // Ok(42) used as a plain constructor call; type is inferred from arguments, not the return type.
        const string source = """
func build() -> Result<int, string> {
    return .Ok(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedCaseConstructor_ImplicitReturn_MatchingTypeArgs_NoErrors()
    {
        // Ok(42) as the trailing implicit-return expression; type is inferred from arguments.
        const string source = """
func build() -> Result<int, string> {
    .Ok(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void UnqualifiedCaseConstructor_TargetTypeDoesNotOverrideArgumentInference()
    {
        // Even though the return type is Result<(), string>, Ok(42) should be inferred as
        // Ok<int> from its argument — not Ok<Unit> from the target type — so a RAV1503 is
        // expected rather than a "no overload" error.
        const string source = """
func build() -> Result<(), string> {
    return .Ok(42)
}

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }

    private static IReadOnlyList<BoundInvocationExpression> CollectInvocationExpressions(BoundNode node)
    {
        var collector = new InvocationCollector();
        collector.Visit(node);
        return collector.Invocations;
    }

    private sealed class InvocationCollector : BoundTreeWalker
    {
        private readonly List<BoundInvocationExpression> _invocations = new();

        public IReadOnlyList<BoundInvocationExpression> Invocations => _invocations;

        public override void VisitInvocationExpression(BoundInvocationExpression node)
        {
            if (node is null)
                return;

            _invocations.Add(node);
            base.VisitInvocationExpression(node);
        }
    }
}
