using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

namespace Raven.LanguageServer.Tests;

public class LanguageServerHoverPresentationTests
{
    [Fact]
    public void CaptureInfoSyntaxFallback_IsLimitedToFunctionSyntax()
    {
        const string code = """
func Main() {
    val local = DescribePoint((5, 1))
    val lambda = func(value: int) -> int {
        return value + 1
    }
}

func DescribePoint(point: (int, int)) -> string {
    return "$point"
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var root = syntaxTree.GetRoot();
        var invocationIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "DescribePoint");
        var functionStatement = root.DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "DescribePoint");
        var functionExpression = root.DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var shouldComputeCaptureInfoFromSyntax = typeof(HoverHandler)
            .GetMethod("ShouldComputeCaptureInfoFromSyntax", BindingFlags.NonPublic | BindingFlags.Static)!;

        ((bool)shouldComputeCaptureInfoFromSyntax.Invoke(null, [invocationIdentifier])!).ShouldBeFalse();
        ((bool)shouldComputeCaptureInfoFromSyntax.Invoke(null, [functionStatement])!).ShouldBeTrue();
        ((bool)shouldComputeCaptureInfoFromSyntax.Invoke(null, [functionExpression])!).ShouldBeTrue();
    }

    [Fact]
    public void PromotedPrimaryConstructorParameter_ShowsPropertyKindAndContainingType()
    {
        const string code = """
class Foo(private var name: string) {
    func Test() -> string => name
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes().OfType<IdentifierNameSyntax>().Single(id => id.Identifier.ValueText == "name");
        var symbol = semanticModel.GetSymbolInfo(identifier).Symbol.ShouldBeAssignableTo<IParameterSymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);

        kind.ShouldBe("Property");
        containing.ShouldBe("class Foo");
    }

    [Fact]
    public void PromotedPrimaryConstructorParameter_HoverSignature_IncludesBindingKeyword()
    {
        const string code = """
class MessageText {
}

record ApplicationError(val Message: MessageText)
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var parameterSyntax = root.DescendantNodes().OfType<ParameterSyntax>().Single();
        var symbol = semanticModel.GetDeclaredSymbol(parameterSyntax).ShouldBeAssignableTo<IParameterSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [symbol, parameterSyntax, semanticModel])!;
        signature.ShouldStartWith("val Message:");
    }

    [Fact]
    public void RecordDeclarationHoverSignature_IndentsMultiLinePrimaryConstructorParameters()
    {
        const string code = """
record Person(
val Name: string
val Age: int
val Items: string[]
)
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var declaration = syntaxTree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var tryBuildTypeDeclarationSyntaxSignature = typeof(HoverHandler)
            .GetMethod("TryBuildTypeDeclarationSyntaxSignature", BindingFlags.NonPublic | BindingFlags.Static)!;
        object?[] args = [syntaxTree.GetText(), declaration, null];

        var built = (bool)tryBuildTypeDeclarationSyntaxSignature.Invoke(null, args)!;
        var signature = (string)args[2]!;

        built.ShouldBeTrue();
        signature.ShouldBe("""
record Person(
    val Name: string
    val Age: int
    val Items: string[]
)
""");
    }

    [Fact]
    public void TypePositionResolution_TreatsArrayTypeAsWholeSyntax()
    {
        const string code = """
class C {
    func Test() -> unit {
        val names: string[] = null
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "names", "string");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBeAssignableTo<ArrayTypeSyntax>();
        resolution.Value.Symbol.ShouldBeAssignableTo<IArrayTypeSymbol>();
    }

    [Fact]
    public void AttributeNameHover_ResolvesConstructorDirectly()
    {
        const string code = """
open class Attribute {}

class InfoAttribute : Attribute
{
    public init() {}
}

[Info]
class Widget {}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().Single(token =>
            token.Kind == SyntaxKind.IdentifierToken &&
            token.ValueText == "Info");
        var attribute = token.GetAncestor<AttributeSyntax>();
        attribute.ShouldNotBeNull();
        var symbolInfo = semanticModel.GetSymbolInfo(attribute);
        (symbolInfo.Symbol ?? symbolInfo.CandidateSymbols.FirstOrDefault()).ShouldNotBeNull();
        var resolveAttributeHoverDirect = typeof(HoverHandler)
            .GetMethod("TryResolveAttributeHoverDirect", BindingFlags.NonPublic | BindingFlags.Static)!;

        var resolution = (SymbolResolutionResult?)resolveAttributeHoverDirect.Invoke(
            null,
            [semanticModel, root, token.SpanStart + 1]);

        resolution.ShouldNotBeNull();
        resolution!.Value.Node.ShouldBeAssignableTo<IdentifierNameSyntax>()
            .Identifier.ValueText.ShouldBe("Info");
        var constructor = resolution.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        constructor.MethodKind.ShouldBe(MethodKind.Constructor);
        constructor.ContainingType?.Name.ShouldBe("InfoAttribute");
    }

    [Fact]
    public void TypePositionResolution_TreatsGenericNameAsWholeSyntax()
    {
        const string code = """
import System.Collections.Generic.*

class C {
    func Test() -> unit {
        val values: List<int> = null
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "values", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBeAssignableTo<PredefinedTypeSyntax>();
        resolution.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>()
            .ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
            .ShouldBe("int");
    }

    [Fact]
    public void TypePositionResolution_ResolvesGenericTypeArgumentIdentifier()
    {
        const string code = """
import System.Collections.Generic.*

union JsonValue(string | double | bool | JsonObject)
record JsonObject(Properties: IDictionary<string, JsonValue>)
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(token =>
            token.ValueText == "string" &&
            token.Parent?.AncestorsAndSelf().OfType<GenericNameSyntax>().Any(generic => generic.Identifier.ValueText == "IDictionary") == true);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>()
            .ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
            .ShouldBe("string");
    }

    [Fact]
    public void TypePositionResolution_TreatsPointerTypeAsWholeSyntax()
    {
        const string code = """
class C {
    unsafe static func Test() -> unit {
        var value = 0
        val pointer: *int = &value
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "pointer", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBeAssignableTo<PointerTypeSyntax>();
        resolution.Value.Symbol.ShouldBeAssignableTo<IPointerTypeSymbol>();
    }

    [Fact]
    public void TypePositionResolution_TreatsByRefTypeAsWholeSyntax()
    {
        const string code = """
class C {
    func Test(value: &int) -> unit {
    }
}
""";

        var (resolution, _) = ResolveParameterTypeAtIdentifier(code, "value", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBeAssignableTo<ByRefTypeSyntax>();
        resolution.Value.Symbol.ShouldNotBeNull();
    }

    [Fact]
    public void TypePositionResolution_TreatsNullableTypeAsWholeSyntax()
    {
        const string code = """
class C {
    func Test() -> unit {
        val maybe: int? = null
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "maybe", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBeAssignableTo<NullableTypeSyntax>();
        resolution.Value.Symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldEndWith("?");
    }

    [Fact]
    public void TypePositionResolution_TreatsTupleConstituentAsActualType()
    {
        const string code = """
class C {
    func Test() -> unit {
        val pair: (int, string) = null
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "pair", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldNotBeAssignableTo<TupleTypeSyntax>();
        resolution.Value.Symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
    }

    [Fact]
    public void TypePositionResolution_TreatsFunctionConstituentAsActualType()
    {
        const string code = """
class C {
    func Test() -> unit {
        val callback: (int) -> string = null
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "callback", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldNotBeAssignableTo<FunctionTypeSyntax>();
        resolution.Value.Symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
    }

    [Fact]
    public void TypePositionResolution_TreatsUnionConstituentAsActualType()
    {
        const string code = """
class C {
    func Test() -> unit {
        val either: int | string = null
    }
}
""";

        var (resolution, _) = ResolveTypeAnnotationAtIdentifier(code, "either", "int");

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldNotBeAssignableTo<UnionTypeSyntax>();
        resolution.Value.Symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
    }

    [Fact]
    public void TypePositionResolution_TreatsCompoundTypeSeparatorAsWholeSyntax()
    {
        const string code = """
class C {
    func Test() -> unit {
        val either: int | string = null
        val pair: (int, string) = null
        val callback: (int) -> string = null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();

        ResolveAtToken("either", SyntaxKind.BarToken).Value.Node.ShouldBeAssignableTo<UnionTypeSyntax>();
        ResolveAtToken("pair", SyntaxKind.OpenParenToken).Value.Node.ShouldBeAssignableTo<TupleTypeSyntax>();
        ResolveAtToken("callback", SyntaxKind.ArrowToken).Value.Node.ShouldBeAssignableTo<FunctionTypeSyntax>();

        SymbolResolutionResult? ResolveAtToken(string declaratorName, SyntaxKind kind)
        {
            var declarator = root.DescendantNodes()
                .OfType<VariableDeclaratorSyntax>()
                .Single(declarator => declarator.Identifier.ValueText == declaratorName);
            var token = declarator.TypeAnnotation!.Type.DescendantTokens().First(token => token.Kind == kind);
            return SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart);
        }
    }

    [Fact]
    public void TypePositionHover_ConstructedGenericUnionUsesTypeArguments()
    {
        const string code = """
union class Option<T> {
    case Some(value: T)
    case None
}

record Payload(val Value: int)

class C {
    func Test() -> unit {
        val item: Option<Payload> = null
    }
}
""";

        var (resolution, semanticModel) = ResolveTypeAnnotationAtIdentifier(code, "item", "Option");
        var root = semanticModel.SyntaxTree.GetRoot();

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, resolution.Value.Node.Span.Start])!;

        signature.ShouldBe("union class Option<Payload>");
    }

    [Fact]
    public void TypePositionHover_ConstructedMetadataOptionUsesTypeArguments()
    {
        const string code = """
import System.*

class C {
    func Test() -> unit {
        val item: Option<string> = null
    }
}
""";

        var (resolution, semanticModel) = ResolveTypeAnnotationAtIdentifier(code, "item", "Option", includeRavenCore: true);
        var root = semanticModel.SyntaxTree.GetRoot();

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, resolution.Value.Node.Span.Start])!;

        signature.ShouldBe("union class Option<string>");
    }

    [Fact]
    public void TypePositionHover_UnionSyntaxUsesConstructedStandardUnionArguments()
    {
        const string code = """
record Left(val Value: int)
record Right(val Value: bool)

class C {
    func Test() -> unit {
        val test: Left | Right = null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location))
            .AddReferences(MetadataReference.CreateFromFile(GetRavenCoreReferencePath()));

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = semanticModel.SyntaxTree.GetRoot();
        var typeSyntax = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "test")
            .TypeAnnotation!.Type;
        var barToken = typeSyntax.DescendantTokens().Single(token => token.Kind == SyntaxKind.BarToken);
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, barToken.SpanStart);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBeAssignableTo<UnionTypeSyntax>();

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, resolution.Value.Node.Span.Start])!;

        signature.ShouldBe("union class Union<Left, Right>: IUnion");
    }

    [Fact]
    public void TypePositionHover_TupleSyntaxUsesNominalValueTupleAndInterfaces()
    {
        const string code = """
class C {
    func Test() -> unit {
        val pair: (status: int, message: string) = null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var tupleType = root.DescendantNodes().OfType<TupleTypeSyntax>().Single();
        var typeInfo = semanticModel.GetTypeInfo(tupleType);
        var tupleSymbol = typeInfo.Type.ShouldBeAssignableTo<ITupleTypeSymbol>();

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [tupleSymbol, tupleType, semanticModel, root, tupleType.OpenParenToken.SpanStart])!;

        signature.ShouldStartWith("struct ValueTuple<int, string>: ");
        signature.ShouldContain("IStructuralComparable");
        signature.ShouldContain("IStructuralEquatable");
        signature.ShouldContain("IComparable");
        signature.ShouldContain("IComparable<ValueTuple<int, string>>");
        signature.ShouldContain("IEquatable<ValueTuple<int, string>>");
        signature.ShouldContain("ITuple");
        signature.ShouldNotContain("(status: int, message: string)");
    }

    [Fact]
    public void TypePositionHover_TupleElementTypesResolveForNamedAndUnnamedElements()
    {
        const string code = """
class C {
    func Test() -> unit {
        val named: (status: int, message: string) = null
        val unnamed: (int, string) = null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        SignatureForTupleElement("named", 0).ShouldStartWith("struct int");
        SignatureForTupleElement("named", 1).ShouldStartWith("class string");
        SignatureForTupleElement("unnamed", 0).ShouldStartWith("struct int");
        SignatureForTupleElement("unnamed", 1).ShouldStartWith("class string");

        string SignatureForTupleElement(string declaratorName, int elementIndex)
        {
            var tupleElement = root.DescendantNodes()
                .OfType<VariableDeclaratorSyntax>()
                .Single(declarator => declarator.Identifier.ValueText == declaratorName)
                .TypeAnnotation!.Type
                .DescendantNodes()
                .OfType<TupleElementSyntax>()
                .ElementAt(elementIndex);
            var offset = tupleElement.Type.Span.Start + 1;
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, offset);

            resolution.ShouldNotBeNull();
            resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
            resolution.Value.Node.ShouldBe(tupleElement.Type);

            return (string)buildSignatureForHover.Invoke(
                null,
                [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, offset])!;
        }
    }

    [Fact]
    public void InvocationTargetHover_TargetTypedMemberBindingUnionCaseUsesInferredUnionArguments()
    {
        const string code = """
union class Option<T> {
    case Some(value: T)
    case None
}

record Foo(
    val Name: string,
    val Item: Option<string>
)

class C {
    func Test() -> unit {
        val foo = Foo(
            Name: "Foo",
            Item: .Some("Foo")
        )
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().Single(token =>
            token.Kind == SyntaxKind.IdentifierToken &&
            token.ValueText == "Some" &&
            token.Parent is IdentifierNameSyntax { Parent: MemberBindingExpressionSyntax });
        var hoverOffset = token.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var constructor = resolution!.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        constructor.MethodKind.ShouldBe(MethodKind.Constructor);

        var buildSignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, hoverOffset])!;

        signature.ShouldBe("Some<string>(value: string)");
    }

    [Fact]
    public void ExtensionMethodHover_ShowsExtensionKindAndQualifiedContainingType()
    {
        const string code = """
namespace Demo.Tools

class Widget

class Runner {
    func Test(widget: Widget) -> int {
        widget.Double()
    }
}

extension WidgetExtensions for Widget {
    func Double() -> int => 2
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name.Identifier.ValueText == "Double")
            .Name;
        var symbol = semanticModel.GetSymbolInfo(identifier).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);

        kind.ShouldBe("Extension method");
        containing.ShouldBe("Demo.Tools.WidgetExtensions");
    }

    [Fact]
    public void TopLevelFunctionHover_ShowsNamespaceContainingDisplay()
    {
        const string code = """
namespace Utilities {
    public func AddOne(value: int) -> int => value + 1
}

func Run() -> int {
    return Utilities.AddOne(1)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name.Identifier.ValueText == "AddOne")
            .Name;
        var symbol = semanticModel.GetSymbolInfo(identifier).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildHoverText = typeof(HoverHandler)
            .GetMethod("BuildHoverText", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);
        var hoverText = (string)buildHoverText.Invoke(null, ["func AddOne(value: int) -> int", kind, containing, null, ImmutableArray<ISymbol>.Empty, false])!;

        kind.ShouldBe("Function");
        containing.ShouldBe("namespace Utilities");
        hoverText.ShouldContain("Function in namespace `Utilities`");
    }

    [Fact]
    public void LocalHover_InTopLevelFunction_ShowsContainingFunction()
    {
        const string code = """
func GetConnectionString() -> string {
    val trimmed = "value"
    return trimmed
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.ValueText == "trimmed");
        var symbol = semanticModel.GetDeclaredSymbol(declarator).ShouldBeAssignableTo<ILocalSymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildHoverText = typeof(HoverHandler)
            .GetMethod("BuildHoverText", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);
        var hoverText = (string)buildHoverText.Invoke(null, ["val trimmed: string", kind, containing, null, ImmutableArray<ISymbol>.Empty, false])!;

        kind.ShouldBe("Local");
        containing.ShouldBe("function GetConnectionString");
        hoverText.ShouldContain("Local in `function GetConnectionString`");
    }

    [Fact]
    public void ExtensionMethodHover_Signature_IsPrefixedWithExtensionTag()
    {
        const string code = """
namespace Demo.Tools

class Widget

class Runner {
    func Test(widget: Widget) -> int {
        widget.Double()
    }
}

extension WidgetExtensions for Widget {
    func Double() -> int => 2
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var access = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.ValueText == "Double");
        var symbol = semanticModel.GetSymbolInfo(access.Name).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        var buildDisplaySignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildDisplaySignatureForHover.Invoke(null, [symbol, access.Name, semanticModel, root, access.Name.Span.Start])!;

        signature.ShouldStartWith("(extension) ");
        signature.ShouldContain("func Double()");
    }

    [Fact]
    public void StaticExtensionMethodHover_UsesExtensionPresentation()
    {
        const string code = """
namespace Demo.Tools

class Widget

class Runner {
    func Test() -> Widget {
        Widget.Build()
    }
}

extension WidgetExtensions for Widget {
    static func Build() -> Widget => Widget()
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var access = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.ValueText == "Build");
        var symbol = semanticModel.GetSymbolInfo(access.Name).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildDisplaySignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);
        var signature = (string)buildDisplaySignatureForHover.Invoke(null, [symbol, access.Name, semanticModel, root, access.Name.Span.Start])!;

        kind.ShouldBe("Extension method");
        containing.ShouldBe("Demo.Tools.WidgetExtensions");
        signature.ShouldStartWith("(extension) ");
        signature.ShouldContain("func Build()");
    }

    [Fact]
    public void StaticExtensionPropertyHover_UsesExtensionPresentation()
    {
        const string code = """
namespace Demo.Tools

class Counter

class Runner {
    func Test() -> int {
        Counter.Total
    }
}

extension CounterExtensions for Counter {
    static val Total: int {
        get { return 42; }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var access = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.ValueText == "Total");
        var symbol = semanticModel.GetSymbolInfo(access.Name).Symbol.ShouldBeAssignableTo<IPropertySymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildDisplaySignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);
        var signature = (string)buildDisplaySignatureForHover.Invoke(null, [symbol, access.Name, semanticModel, root, access.Name.Span.Start])!;

        kind.ShouldBe("Extension property");
        containing.ShouldBe("Demo.Tools.CounterExtensions");
        signature.ShouldStartWith("(extension) ");
        signature.ShouldContain("val Total:");
    }

    [Fact]
    public void SealedHierarchyTypeHover_Signature_ShowsSealedModifier()
    {
        const string code = """
sealed interface HttpResponse {}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declaration = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var symbol = semanticModel.GetDeclaredSymbol(declaration).ShouldBeAssignableTo<INamedTypeSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [symbol, declaration, semanticModel])!;

        signature.ShouldBe("sealed interface HttpResponse");
    }

    [Fact(Skip = "Stale union-case hover coverage: resolver behavior for qualified case invocations needs redesign around compiler API results.")]
    public void QualifiedUnionCaseInvocation_HoverPrefersUnionCaseOverImportedMember()
    {
        const string code = """
import System.Console.*

class Runner {
    func Run() {
        val x = System.Result<int, string>.Error("42")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var ravenCorePath = Path.GetFullPath(Path.Combine(
            AppContext.BaseDirectory,
            "..",
            "..",
            "..",
            "..",
            "..",
            "src",
            "Raven.Core",
            "bin",
            "Debug",
            "net10.0",
            "Raven.Core.dll"));
        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(ravenCorePath));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var errorIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "Error");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            errorIdentifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution.Value.Symbol.Name.ShouldBe("Error");
        resolution.Value.Symbol.ShouldBeAssignableTo<INamedTypeSymbol>().IsUnionCase.ShouldBeTrue();
    }

    [Fact]
    public void MetadataResultCasePatternHover_UsesScrutineeTypeArguments()
    {
        const string code = """
import System.*
import System.Result.*

class C {
    func Test() {
        val ok: Result<int, string> = Ok(2)

        match ok {
            Ok(_) => {}
            Error(_) => {}
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var references = LanguageServerTestReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCoreReferencePath())])
            .ToArray();
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var errorIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "Error");
        errorIdentifier.Parent.ShouldBeAssignableTo<NominalDeconstructionPatternSyntax>();
        var matchStatement = errorIdentifier.GetAncestor<MatchStatementSyntax>();
        matchStatement.ShouldNotBeNull();
        matchStatement.Expression.ToString().ShouldBe("ok");
        var scrutineeSymbol = semanticModel.GetSymbolInfo(matchStatement.Expression).Symbol.ShouldBeAssignableTo<ILocalSymbol>();
        scrutineeSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("Result<int, string>");

        var hoverOffset = errorIdentifier.Identifier.SpanStart + 1;
        var hoverToken = root.FindToken(hoverOffset);
        hoverToken.ShouldBe(errorIdentifier.Identifier);
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();

        var buildSignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, hoverOffset])!;

        signature.ShouldContain("Error");
        signature.ShouldContain("string");
        signature.ShouldNotContain(" E");
    }

    [Fact]
    public void MetadataResultCasePatternHover_PrefersUnionCaseOverVisibleTypeWithSameName()
    {
        const string code = """
import System.*
import System.Result.*

class Ok {}

class C {
    func Test() {
        val result: Result<int, string> = .Ok(2)

        match result {
            Ok(val value) => {}
            Error(_) => {}
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var references = LanguageServerTestReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCoreReferencePath())])
            .ToArray();
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var okPatternIdentifier = root.DescendantNodes()
            .OfType<NominalDeconstructionPatternSyntax>()
            .Select(pattern => Assert.IsType<IdentifierNameSyntax>(pattern.Type))
            .Single(identifier => identifier.Identifier.ValueText == "Ok");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            okPatternIdentifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        var symbol = resolution.Value.Symbol.ShouldBeAssignableTo<INamedTypeSymbol>();
        symbol.IsUnionCase.ShouldBeTrue();
        symbol.Name.ShouldBe("Ok");

        var buildSignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, okPatternIdentifier.Identifier.SpanStart + 1])!;

        signature.ShouldContain("Ok");
        signature.ShouldContain("int");
        signature.ShouldNotContain("class Ok {}");
    }

    [Fact]
    public void ImportDirectiveNameHover_DoesNotResolveErrorType()
    {
        const string code = """
import System.*
import System.Console
import System.Collections.Generic.*
import System.Text.Json
import System.Text.Json.Serialization.*
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.ConsoleApplication));

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().Single(token =>
            token.Kind == SyntaxKind.IdentifierToken &&
            token.ValueText == "Serialization");
        var hoverOffset = token.SpanStart + 1;

        var shouldSuppressSemanticHover = typeof(HoverHandler)
            .GetMethod("ShouldSuppressSemanticHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var suppressed = (bool)shouldSuppressSemanticHover.Invoke(null, [root, hoverOffset])!;

        suppressed.ShouldBeTrue();
        SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset).ShouldBeNull();
    }

    [Fact]
    public void GenericConstructorInvocationHover_UsesConstructedConstructor()
    {
        const string code = """
import System.*

open class Endpoint {
    init(handler: Delegate) { }
}

class GET : Endpoint {
    init(pattern: string, handler: () -> string) : base(handler) { }
}

class GET<T> : Endpoint {
    init(pattern: string, handler: T -> string) : base(handler) { }
}

val endpoint = GET("/{id:int}", func (id: int) => id.ToString())
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error).ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var getIdentifier = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Select(static invocation => invocation.Expression)
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "GET");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            getIdentifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        var getConstructor = resolution.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        getConstructor.MethodKind.ShouldBe(MethodKind.Constructor);
        getConstructor.ContainingType.Name.ShouldBe("GET");
        getConstructor.ContainingType.TypeArguments.Single().SpecialType.ShouldBe(SpecialType.System_Int32);

        var buildDisplaySignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildDisplaySignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, getIdentifier.Identifier.SpanStart + 1])!;

        signature.ShouldStartWith("GET<int>(");
        signature.ShouldContain("handler: int -> string");

    }

    [Fact]
    public void NestedInvocationArgumentHover_PrefersCallableOverOuterResultType()
    {
        const string code = """
class DbContextOptionsBuilder {}

class Services {
    static func UseNpgsql(connectionString: string) -> DbContextOptionsBuilder {
        DbContextOptionsBuilder()
    }
}

class VehicleAppServices {
    static func GetConnectionString() -> string => ""
}

val builder = Services.UseNpgsql(VehicleAppServices.GetConnectionString())
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var getConnectionString = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name.Identifier.ValueText == "GetConnectionString")
            .Name;
        var hoverOffset = getConnectionString.Identifier.SpanStart + 1;

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.InvocationTarget);
        var method = resolution.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        method.Name.ShouldBe("GetConnectionString");

        var buildDisplaySignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildDisplaySignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, hoverOffset])!;

        signature.ShouldBe("static func GetConnectionString() -> string");
    }

    [Fact]
    public void MetadataVoidMethodHover_ProjectsReturnTypeAsUnit()
    {
        const string code = """
System.Console.WriteLine(true)
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var writeLine = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name.Identifier.ValueText == "WriteLine")
            .Name;
        var hoverOffset = writeLine.Identifier.SpanStart + 1;

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.InvocationTarget);
        var method = resolution.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        method.Name.ShouldBe("WriteLine");

        var buildDisplaySignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildDisplaySignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, hoverOffset])!;

        signature.ShouldContain("func WriteLine");
        signature.ShouldContain("-> ()");
        signature.ShouldNotContain("Void");
    }

    [Fact]
    public void InvocationTargetHoverDirect_UsesBoundOverloadForMetadataMethods()
    {
        const string code = """
import System.*

func Main() -> unit {
    val value = 42
    Console.WriteLine("value: ${value.ToString()}")
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var instrumentation = new PerformanceInstrumentation();
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(
                OutputKind.ConsoleApplication,
                performanceInstrumentation: instrumentation));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var writeLine = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name.Identifier.ValueText == "WriteLine")
            .Name;
        var hoverOffset = writeLine.Identifier.SpanStart + 1;

        var tryResolveInvocationTargetHoverDirect = typeof(HoverHandler)
            .GetMethod("TryResolveInvocationTargetHoverDirect", BindingFlags.NonPublic | BindingFlags.Static)!;
        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        var resolution = (SymbolResolutionResult?)tryResolveInvocationTargetHoverDirect.Invoke(
            null,
            [semanticModel, root, hoverOffset]);
        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);

        resolution.ShouldNotBeNull();
        var method = resolution!.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        method.Name.ShouldBe("WriteLine");
        instrumentation.BinderReentry.TotalBindExecutions.ShouldBe(0);
        delta.BoundNodeBindFallbacks.ShouldBe(0);

        var buildDisplaySignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildDisplaySignatureForResolvedHover.Invoke(
            null,
            [resolution.Value, semanticModel, root, hoverOffset])!;

        signature.ShouldContain("value: string");
        signature.ShouldNotContain("value: bool");
        signature.ShouldContain("-> ()");
    }

    [Fact]
    public void GenericInvocationTypeArgumentHover_ResolvesTypeArgument()
    {
        const string code = """
class C {
    static func Identity<T>(value: T) -> T {
        value
    }

    static func Run() -> unit {
        val value = Identity<string>("ok")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var typeArgument = root.DescendantNodes()
            .OfType<GenericNameSyntax>()
            .Single(name => name.Identifier.ValueText == "Identity")
            .TypeArgumentList.Arguments.Single().Type;
        var token = typeArgument.DescendantTokens().Single(static token => token.ValueText == "string");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.TypePosition);
        resolution.Value.Node.ShouldBe(typeArgument);
        resolution.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>()
            .ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
            .ShouldBe("string");

        var invocationName = root.DescendantNodes()
            .OfType<GenericNameSyntax>()
            .Single(name => name.Identifier.ValueText == "Identity");
        var invocationResolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            invocationName.Identifier.SpanStart + 1);

        invocationResolution.ShouldNotBeNull();
        invocationResolution!.Value.Kind.ShouldBe(SymbolResolutionKind.InvocationTarget);

        var buildDisplaySignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildDisplaySignatureForResolvedHover.Invoke(
            null,
            [invocationResolution.Value, semanticModel, root, invocationName.Identifier.SpanStart + 1])!;

        signature.ShouldBe("static func Identity<string>(value: string) -> string");
    }

    [Fact]
    public void ArgumentIdentifierHover_ResolvesLocalInsteadOfInvocationTargetType()
    {
        const string code = """
import System.Text.Json.*

record Foo(val Name: string)

val foo = Foo("Foo")
val options = JsonSerializerOptions with {
    WriteIndented = true
}

val str = JsonSerializer.Serialize(foo, options)
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var argumentIdentifier = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "Serialize" })
            .ArgumentList.Arguments.Last()
            .Expression.ShouldBeAssignableTo<IdentifierNameSyntax>();

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            argumentIdentifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.Identifier);
        var local = resolution.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>();
        local.Name.ShouldBe("options");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, argumentIdentifier.Identifier.SpanStart + 1])!;

        signature.ShouldStartWith("val options:");
        signature.ShouldContain("JsonSerializerOptions");
    }

    [Fact]
    public void NestedArgumentIdentifierHover_ResolvesLocalInsteadOfExpressionType()
    {
        const string code = """
class C {
    func Test(value: int) -> unit { }

    func Run() -> unit {
        val b: int = 41
        Test(1 + b)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var argumentIdentifier = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "Test" })
            .ArgumentList.Arguments.Single()
            .Expression.DescendantNodesAndSelf()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "b");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            argumentIdentifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.Identifier);
        var local = resolution.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>();
        local.Name.ShouldBe("b");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, argumentIdentifier.Identifier.SpanStart + 1])!;

        signature.ShouldBe("val b: int");
    }

    [Fact]
    public void LambdaParameterReferenceHover_UsesAvailableDelegateParameterFastPath()
    {
        const string code = """
func Apply(value: int, transform: int -> int) -> int {
    transform(value)
}

func Main() -> int {
    Apply(1, value => value + 1)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var valueReference = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(identifier => identifier.Identifier.ValueText == "value");
        var hoverOffset = valueReference.Identifier.SpanStart + 1;

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var parameter = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
        parameter.Name.ShouldBe("value");
        parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
    }

    [Fact]
    public void TrailingBlockParameterHover_UsesContextualDelegateParameterType()
    {
        const string code = """
class Store {
    func Find(id: int) -> string { id.ToString() }
}

func Use(handler: int -> string) -> string {
    handler(42)
}

func Route(store: Store) -> string {
    Use { id =>
        store.Find(id)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error).ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var trailingBlock = root.DescendantNodes().OfType<TrailingBlockExpressionSyntax>().Single();
        trailingBlock.Parameter.ShouldNotBeNull();
        var parameter = trailingBlock.Parameter!;

        var parameterSymbol = semanticModel.GetFunctionExpressionParameterSymbol(parameter);
        parameterSymbol.ShouldNotBeNull();
        parameterSymbol!.Name.ShouldBe("id");
        parameterSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");

        var hoverOffset = parameter.Identifier.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var resolvedParameter = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
        resolvedParameter.Name.ShouldBe("id");
        resolvedParameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolvedParameter, resolution.Value.Node, semanticModel, root, hoverOffset])!;

        signature.ShouldContain("id: int");
        signature.ShouldNotContain("<Error>");
    }

    [Fact]
    public void DelegateTypeHover_UsesRavenFunctionTypeSignature()
    {
        const string code = """
class Functions {
    func Apply(value: int, transform: (int) -> int) -> int {
        transform(value)
    }

    func Test() -> int {
        val increment: (int) -> int = x => x + 1
        increment(1)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var ravenCorePath = Path.GetFullPath(Path.Combine(
            AppContext.BaseDirectory,
            "..",
            "..",
            "..",
            "..",
            "..",
            "src",
            "Raven.Core",
            "bin",
            "Debug",
            "net10.0",
            "Raven.Core.dll"));
        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(ravenCorePath));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var functionTypeSyntax = root.DescendantNodes().OfType<FunctionTypeSyntax>().First();
        var localDeclarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "increment");
        var localSymbol = semanticModel.GetDeclaredSymbol(localDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        var delegateType = localSymbol.Type.ShouldBeAssignableTo<INamedTypeSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [delegateType, functionTypeSyntax, semanticModel])!;
        signature.ShouldStartWith("(");
        signature.ShouldContain("->");
        signature.ShouldNotContain("Func(");
    }

    [Fact]
    public void LocalHover_WithErrorType_UsesAngleBracketPlaceholder()
    {
        const string code = """
class Broken {
    func Test() -> unit {
        val value: MissingType = 42
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var localDeclarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "value");
        var localSymbol = semanticModel.GetDeclaredSymbol(localDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [localSymbol, localDeclarator, semanticModel])!;
        signature.ShouldBe("val value: <Error>");
    }

    [Fact]
    public void ParameterHover_WithErrorType_UsesAngleBracketPlaceholder()
    {
        const string code = """
class Broken {
    func Test(value: MissingType) -> unit {}
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var parameterSyntax = root.DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameterSymbol = semanticModel.GetDeclaredSymbol(parameterSyntax).ShouldBeAssignableTo<IParameterSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [parameterSymbol, parameterSyntax, semanticModel])!;
        signature.ShouldBe("value: MissingType");
    }

    [Fact]
    public async Task LocalHover_UsesCompactUnionTypeSignatureAsync()
    {
        const string code = """
func Main() -> unit {
    val invoiceTotal: Either<int, string> = 42
}

union Either<T1, T2>(T1 | T2)
""";

        var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-hover-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempRoot);

        try
        {
            var projectPath = Path.Combine(tempRoot, "App.rvnproj");
            File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

            var documentPath = Path.Combine(tempRoot, "src", "main.rvn");
            Directory.CreateDirectory(Path.GetDirectoryName(documentPath)!);
            File.WriteAllText(documentPath, code);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var uri = DocumentUri.FromFileSystemPath(documentPath);
            await store.UpsertDocumentAsync(uri, code);

            var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
            context.ShouldNotBeNull();

            var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
            var root = context.Value.SyntaxTree.GetRoot();
            var localDeclarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "invoiceTotal");
            var localSymbol = semanticModel.GetDeclaredSymbol(localDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

            var buildSignature = typeof(HoverHandler)
                .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

            var signature = (string)buildSignature.Invoke(null, [localSymbol, localDeclarator, semanticModel])!;
            signature.ShouldBe("val invoiceTotal: Either<int, string>");
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public void LocalHover_WithGenericConstructorInitializer_UsesExplicitTypeArguments()
    {
        const string code = """
import System.Collections.Generic.*

class JsonValue {
}

class Reader {
    func Read() -> unit {
        val values = List<JsonValue>()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var localDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.Text == "values");
        var localSymbol = semanticModel.GetDeclaredSymbol(localDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [localSymbol, localDeclarator, semanticModel])!;
        signature.ShouldBe("val values: List<JsonValue>");
    }

    [Fact]
    public void ProtectedMethodHover_UsesProtectedKeyword()
    {
        const string code = """
class Base {
    protected func Run() -> unit { }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var method = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = semanticModel.GetDeclaredSymbol(method).ShouldBeAssignableTo<IMethodSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [symbol, method, semanticModel])!;
        signature.ShouldBe("protected func Run() -> ()");
        signature.ShouldNotContain("protectedandprotected");
    }

    [Fact]
    public void MethodHover_FormatsAllAccessibilityModifiers()
    {
        const string code = """
class Base {
    private func PrivateRun() -> unit { }
    internal func InternalRun() -> unit { }
    protected func ProtectedRun() -> unit { }
    protected internal func ProtectedInternalRun() -> unit { }
    private protected func PrivateProtectedRun() -> unit { }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var methods = root.DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .ToDictionary(
                static declaration => declaration.Identifier.ValueText,
                declaration => semanticModel.GetDeclaredSymbol(declaration).ShouldBeAssignableTo<IMethodSymbol>());
        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        ((string)buildSignature.Invoke(null, [methods["PrivateRun"], methods["PrivateRun"].DeclaringSyntaxReferences[0].GetSyntax(), semanticModel])!)
            .ShouldBe("private func PrivateRun() -> ()");
        ((string)buildSignature.Invoke(null, [methods["InternalRun"], methods["InternalRun"].DeclaringSyntaxReferences[0].GetSyntax(), semanticModel])!)
            .ShouldBe("internal func InternalRun() -> ()");
        ((string)buildSignature.Invoke(null, [methods["ProtectedRun"], methods["ProtectedRun"].DeclaringSyntaxReferences[0].GetSyntax(), semanticModel])!)
            .ShouldBe("protected func ProtectedRun() -> ()");
        ((string)buildSignature.Invoke(null, [methods["ProtectedInternalRun"], methods["ProtectedInternalRun"].DeclaringSyntaxReferences[0].GetSyntax(), semanticModel])!)
            .ShouldBe("protected internal func ProtectedInternalRun() -> ()");
        ((string)buildSignature.Invoke(null, [methods["PrivateProtectedRun"], methods["PrivateProtectedRun"].DeclaringSyntaxReferences[0].GetSyntax(), semanticModel])!)
            .ShouldBe("private protected func PrivateProtectedRun() -> ()");
    }

    [Fact]
    public void FunctionStatementHover_DoesNotShowAccessibilityModifier()
    {
        const string code = """
class C {
    func Run() -> int {
        func Parse() -> int => 42
        Parse()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var functionStatement = root.DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var methodSymbol = semanticModel.GetDeclaredSymbol(functionStatement).ShouldBeAssignableTo<IMethodSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [methodSymbol, functionStatement, semanticModel])!;
        signature.ShouldStartWith("func Parse()");
        signature.ShouldNotContain("private ");
        signature.ShouldNotContain("internal ");
    }

    [Fact]
    public void GenericMethodHover_IncludesOutParameterModifier()
    {
        const string code = """
class MacroArgument {
    func TryParseValue<T>(out value: int) -> bool { false }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var method = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = semanticModel.GetDeclaredSymbol(method).ShouldBeAssignableTo<IMethodSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [symbol, method, semanticModel])!;
        signature.ShouldStartWith("func TryParseValue<T>(");
        signature.ShouldContain("out value:");
    }

    [Fact]
    public void XmlDocumentationHover_IsRenderedAsMarkdownInsteadOfRawXml()
    {
        const string code = """
/// <summary>
/// Creates a stored property declaration with an initializer.
/// </summary>
/// <remarks>
/// Alias for <c>PropertyDeclaration</c>. Prefer the canonical factory name unless the alias is clearer at the call site.
/// </remarks>
class Widget
""";

        var syntaxTree = SyntaxTree.ParseText(
            code,
            new ParseOptions { DocumentationFormat = DocumentationFormat.Xml },
            path: "/workspace/test.rav");

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var typeDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var documentation = semanticModel.GetDeclaredSymbol(typeDeclaration)!.GetDocumentationComment();

        var formatDocumentation = typeof(HoverHandler)
            .GetMethod("FormatDocumentation", BindingFlags.NonPublic | BindingFlags.Static)!;

        var formatted = (string?)formatDocumentation.Invoke(null, [documentation]);

        formatted.ShouldNotBeNull();
        formatted.ShouldContain("Creates a stored property declaration with an initializer.");
        formatted.ShouldContain("**Remarks**");
        formatted.ShouldContain("`PropertyDeclaration`");
        formatted.ShouldNotContain("<summary>");
        formatted.ShouldNotContain("<remarks>");
    }

    [Fact]
    public void MarkdownDocumentationHover_RewritesXrefsToOpenDocumentationCommands()
    {
        const string code = """
/// See [Widget](xref:T:Samples.Docs.Widget).
///
/// @see xref:M:Samples.Docs.Widget.GetTitle
class Widget
""";

        var syntaxTree = SyntaxTree.ParseText(
            code,
            new ParseOptions { DocumentationFormat = DocumentationFormat.Markdown },
            path: "/workspace/test.rav");

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var typeDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var documentation = semanticModel.GetDeclaredSymbol(typeDeclaration)!.GetDocumentationComment();

        var formatDocumentation = typeof(HoverHandler)
            .GetMethod("FormatDocumentation", BindingFlags.NonPublic | BindingFlags.Static)!;

        var formatted = (string?)formatDocumentation.Invoke(null, [documentation]);

        formatted.ShouldNotBeNull();
        formatted.ShouldContain("[Widget](command:raven.openDocumentation?");
        formatted.ShouldContain("raven-doc%3A%2F%2F%2Fxref.md%3Ftarget%3DT%253ASamples.Docs.Widget");
        formatted.ShouldContain("raven-doc%3A%2F%2F%2Fxref.md%3Ftarget%3DM%253ASamples.Docs.Widget.GetTitle");
    }

    [Fact]
    public void MarkdownDocumentationHover_DoesNotDuplicateRecognizedSections()
    {
        const string code = """
/// Prints information about [Widget](xref:T:Samples.Docs.Widget) values.
///
/// ### Remarks
///
/// This consumer project exists to exercise cross-project documentation links
/// and metadata loading scenarios.
class WidgetPrinter
""";

        var syntaxTree = SyntaxTree.ParseText(
            code,
            new ParseOptions { DocumentationFormat = DocumentationFormat.Markdown },
            path: "/workspace/test.rav");

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var typeDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var documentation = semanticModel.GetDeclaredSymbol(typeDeclaration)!.GetDocumentationComment();

        var formatDocumentation = typeof(HoverHandler)
            .GetMethod("FormatDocumentation", BindingFlags.NonPublic | BindingFlags.Static)!;

        var formatted = (string?)formatDocumentation.Invoke(null, [documentation]);

        formatted.ShouldNotBeNull();
        formatted.ShouldContain("Prints information about [Widget](command:raven.openDocumentation?");
        formatted.ShouldContain("**Remarks**");
        formatted.ShouldContain("This consumer project exists to exercise cross-project documentation links");
        formatted.ShouldNotContain("### Remarks");
        formatted.Split("**Remarks**", StringSplitOptions.None).Length.ShouldBe(2);
    }

    [Fact]
    public void ContinueWithBody_ResultHover_ResolvesTaskResultProperty()
    {
        const string code = """
class Payload()

class ContinuationContext(var Result: Payload)

class Computation(var Value: ContinuationContext) {
    func ContinueWith(continuationFunction: (ContinuationContext) -> Payload) -> Payload {
        continuationFunction(Value)
    }
}

class C {
    func Run(value: ContinuationContext) -> Payload {
        val f = func (x: ContinuationContext) {
            Computation(x)
        }

        val x = f(value).ContinueWith(x => {
            return x.Result
        })

        x
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var resultIdentifier = root
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "Result");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            resultIdentifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        var property = resolution!.Value.Symbol.ShouldBeAssignableTo<IPropertySymbol>();
        property.Name.ShouldBe("Result");
        property.Type.Name.ShouldBe("Payload");
    }

    [Fact]
    public void ContinueWithBody_ReceiverHover_ResolvesLambdaParameter()
    {
        const string code = """
class Payload()

class ContinuationContext(var Result: Payload)

class Computation(var Value: ContinuationContext) {
    func ContinueWith(continuationFunction: (ContinuationContext) -> Payload) -> Payload {
        continuationFunction(Value)
    }
}

class C {
    func Run(value: ContinuationContext) -> Payload {
        val f = func (x: ContinuationContext) {
            Computation(x)
        }

        val x = f(value).ContinueWith(x => {
            return x.Result
        })

        x
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();

        var receiverIdentifier = root
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Where(member => member.Name.Identifier.ValueText == "Result")
            .Select(member => member.Expression)
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "x");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            receiverIdentifier.Identifier.SpanStart);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.Name.ShouldBe("x");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, receiverIdentifier.Identifier.SpanStart])!;
        signature.ShouldContain("x: ContinuationContext");
    }

    [Fact]
    public void NamedFunctionExpressionIdentifier_HoverUsesLambdaSignature()
    {
        const string code = """
class C {
    func Run() -> int {
        val seed = 1
        val compute = func Step(n: int) -> int {
            if n < 1
                n
            else
                Step(n - 1)
        }

        compute(3)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var lambda = root.DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            lambda.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        var resolvedMethod = resolution!.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        resolvedMethod.MethodKind.ShouldBe(MethodKind.LambdaMethod);

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, lambda.Identifier.SpanStart + 1])!;

        signature.ShouldStartWith("(");
        signature.ShouldContain("->");
        signature.ShouldNotContain("Func(");
    }

    [Fact]
    public void FunctionExpressionParameterDeclaration_HoverResolvesParameterSymbol()
    {
        const string code = """
class C {
    func Run() -> int {
        val project: (int) -> int = (x: int) => x + 1
        project(5)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var lambdaParameter = root
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single()
            .ParameterList
            .Parameters
            .Single();

        var hoverOffset = lambdaParameter.Identifier.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var parameterSymbol = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
        parameterSymbol.Name.ShouldBe("x");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [parameterSymbol, resolution.Value.Node, semanticModel, root, hoverOffset])!;
        signature.ShouldContain("x:");
    }

    [Fact]
    public async Task EventSubscriptionLambdaParameter_HoverUsesInferredDelegateParameterTypeAsync()
    {
        const string code = """
import System.*

class ChangedArgs(var PropertyName: string)

delegate PropertyChangedHandler(sender: object?, e: ChangedArgs) -> unit

class App {
    static func Log(value: string) -> unit { }

    static func Main() -> unit {
        val handler: PropertyChangedHandler = (sender, args) => {
            Log(args.PropertyName ?? "")
        }
    }
}
""";

        var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-hover-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempRoot);

        try
        {
            var projectPath = Path.Combine(tempRoot, "App.rvnproj");
            File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

            var documentPath = Path.Combine(tempRoot, "src", "main.rvn");
            Directory.CreateDirectory(Path.GetDirectoryName(documentPath)!);
            File.WriteAllText(documentPath, code);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var uri = DocumentUri.FromFileSystemPath(documentPath);
            await store.UpsertDocumentAsync(uri, code);

            var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
            context.ShouldNotBeNull();

            var compilation = context.Value.Compilation;
            var syntaxTree = context.Value.SyntaxTree;
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            compilation.GetDiagnostics().Where(d => d.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error).ShouldBeEmpty();
            var root = syntaxTree.GetRoot();
            var lambdaParameters = root
                .DescendantNodes()
                .OfType<ParenthesizedFunctionExpressionSyntax>()
                .Single()
                .ParameterList
                .Parameters;

            var buildSignatureForHover = typeof(HoverHandler)
                .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

            var lambdaOperation = semanticModel.GetOperation(root
                .DescendantNodes()
                .OfType<ParenthesizedFunctionExpressionSyntax>()
                .Single()).ShouldBeAssignableTo<ILambdaOperation>();
            lambdaOperation.Parameters[1].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("ChangedArgs");

            var argsParameter = semanticModel.GetFunctionExpressionParameterSymbol(lambdaParameters[1]);
            argsParameter.ShouldNotBeNull();
            argsParameter!.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("ChangedArgs");

            foreach (var (parameterName, expectedType) in new[]
                     {
                         ("args", "ChangedArgs")
                     })
            {
                var lambdaParameter = lambdaParameters.Single(parameter => parameter.Identifier.ValueText == parameterName);
                var hoverOffset = lambdaParameter.Identifier.SpanStart + 1;
                var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

                resolution.ShouldNotBeNull();
                var parameterSymbol = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
                parameterSymbol.Name.ShouldBe(parameterName);
                parameterSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe(expectedType);

                var signature = (string)buildSignatureForHover.Invoke(
                    null,
                    [parameterSymbol, resolution.Value.Node, semanticModel, root, hoverOffset])!;

                signature.ShouldContain($"{parameterName}: {expectedType}");
            }
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public void GetLambdaParameterIndex_SkipsMissingParameterIdentifiers()
    {
        var functionExpression = SyntaxFactory.ParenthesizedFunctionExpression(
            SyntaxFactory.ParameterList(
                SyntaxFactory.SeparatedList<ParameterSyntax>([
                    SyntaxFactory.Parameter(SyntaxFactory.List<AttributeListSyntax>(), SyntaxFactory.Token(SyntaxKind.None)),
                    SyntaxFactory.CommaToken,
                    SyntaxFactory.Parameter(SyntaxFactory.List<AttributeListSyntax>(), SyntaxFactory.Identifier("value"))
                ])),
            SyntaxFactory.List<TypeParameterConstraintClauseSyntax>());

        var getLambdaParameterIndex = typeof(HoverHandler)
            .GetMethod("GetLambdaParameterIndex", BindingFlags.NonPublic | BindingFlags.Static)!;

        var index = (int)getLambdaParameterIndex.Invoke(null, [functionExpression, "value"])!;

        index.ShouldBe(1);
    }

    [Fact]
    public void SymbolResolver_DoesNotThrowInsideAssignmentStatement()
    {
        const string code = """
class C {
    func Run() -> unit {
        var value = 0
        value += 1
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single()
            .Left
            .DescendantTokens()
            .Single(token => token.ValueText == "value");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("value");
    }

    [Fact(Skip = "Stale pattern hover coverage: resolver behavior for sequence declarations needs redesign around compiler API results.")]
    public void DeconstructionPatternDeclaration_HoverResolvesBoundLocal()
    {
        const string code = """
class C {
    func Run() -> int {
        val [a, b] = [1, 2]
        a + b
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declarationDesignation = root
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .First(designation => designation.Identifier.ValueText == "a" &&
                                  designation.Ancestors().Any(static n => n is SequencePatternSyntax));
        var usageIdentifier = root
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.ValueText == "a" &&
                         !id.Ancestors().Any(static n => n is SequencePatternSyntax));

        var hoverOffset = declarationDesignation.Identifier.SpanStart + 1;
        var usageSymbol = semanticModel.GetSymbolInfo(usageIdentifier).Symbol.ShouldBeAssignableTo<ILocalSymbol>();
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        usageSymbol.DeclaringSyntaxReferences.Any(reference =>
            reference.SyntaxTree == declarationDesignation.SyntaxTree &&
            reference.Span.Contains(declarationDesignation.Identifier.SpanStart)).ShouldBeTrue();
        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("a");
        usageSymbol.Name.ShouldBe("a");
    }

    [Fact]
    public void PositionalDeconstructionPatternDeclaration_HoverResolvesBoundLocals()
    {
        const string code = """
class C {
    func Run() -> int {
        val obj = (3, "test")
        val (id, name) = obj
        id
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();

        var tokens = new[]
        {
            root.DescendantTokens().First(t =>
                t.Kind == SyntaxKind.IdentifierToken &&
                t.ValueText == "id" &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is PositionalPatternSyntax) == true),
            root.DescendantTokens().First(t =>
                t.Kind == SyntaxKind.IdentifierToken &&
                t.ValueText == "name" &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is PositionalPatternSyntax) == true)
        };

        foreach (var token in tokens)
        {
            var hoverOffset = token.SpanStart + Math.Min(1, token.Span.Length - 1);
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);
            if (resolution is not null)
            {
                resolution.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe(token.ValueText);
                continue;
            }

            var tryBuildPatternDeclarationHover = typeof(HoverHandler)
                .GetMethod("TryBuildPatternDeclarationHover", BindingFlags.NonPublic | BindingFlags.Static)!;
            var hover = tryBuildPatternDeclarationHover.Invoke(
                null,
                [syntaxTree.GetText(), semanticModel, root, hoverOffset]);
            hover.ShouldNotBeNull();
        }
    }

    [Fact]
    public void LambdaDeconstructionPatternDeclaration_HoverResolvesBoundLocal()
    {
        const string code = """
import System.*
import System.Linq.*

class C {
    func Run() -> () {
        val tuples = [(1, "x")]
        val rows = [[1, 2, 3]]

        val s = tuples.Select(((a, b)) => b)
        val t = rows.Select(([head, ..rest]) => rest)

        _ = [s, t]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();

        var tokens = new[]
        {
            root.DescendantTokens().First(t =>
                t.Kind == SyntaxKind.IdentifierToken &&
                t.ValueText == "rest" &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is FunctionExpressionSyntax) == true &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is SequencePatternSyntax) == true)
        };

        foreach (var token in tokens)
        {
            var hoverOffset = token.SpanStart + Math.Min(1, token.Span.Length - 1);
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);
            resolution.ShouldNotBeNull();
            resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe(token.ValueText);
        }
    }

    [Fact]
    public void NestedDeconstructionDeclarations_HoverResolvesBoundSymbols()
    {
        const string code = """
class C {
    func Run() -> int {
        val ((a, b), c) = ((1, 2), 3)
        val [head, [inner1, inner2]] = [1, [2, 3]]
        a + b + c + head + inner1 + inner2
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();

        var identifiers = new[] { "a", "b", "c", "head", "inner1", "inner2" };
        foreach (var name in identifiers)
        {
            var declarationToken = root.DescendantTokens().First(t =>
                t.Kind == SyntaxKind.IdentifierToken &&
                t.ValueText == name &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is PositionalPatternSyntax or SequencePatternSyntax) == true);

            var hoverOffset = declarationToken.SpanStart + Math.Min(1, declarationToken.Span.Length - 1);
            var resolution = SymbolResolver.ResolveSymbolAtPosition(
                semanticModel,
                root,
                hoverOffset);

            resolution.ShouldNotBeNull();
            resolution!.Value.Symbol.Name.ShouldBe(name);
        }
    }

    [Fact]
    public void IsPatternDeclaration_HoverResolvesBoundLocal()
    {
        const string code = """
class C {
    func Run(value: (int, int)) -> int {
        if value is (val x, 0) {
            return x
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declarationToken = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "x" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is IsPatternExpressionSyntax) == true);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, declarationToken.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("x");
    }

    [Fact]
    public void MatchArmPatternDeclaration_HoverResolvesBoundLocal()
    {
        const string code = """
class C {
    func Run(values: int[]) -> int {
        return values match {
            [val head, ..val rest] => head + rest.Length
            _ => 0
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var tokens = new[] { "head", "rest" };

        foreach (var name in tokens)
        {
            var declarationToken = root.DescendantTokens().First(t =>
                t.Kind == SyntaxKind.IdentifierToken &&
                t.ValueText == name &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is MatchArmSyntax) == true &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is PatternSyntax) == true);

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, declarationToken.SpanStart + 1);

            resolution.ShouldNotBeNull();
            resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe(name);
        }
    }

    [Fact]
    public async Task PatternCaseHover_ResolvesUnionCaseSymbolsInsteadOfCarrierUnionAsync()
    {
        const string code = """
import System.*

class C {
    func Render(status: Result<System.Int32, System.Int32>) -> unit {
        match status {
            .Ok(val okStatement) => okStatement
            .Error(val errorStatement) => errorStatement
        }

        _ = status match {
            .Ok(val okExpression) => okExpression
            .Error(val errorExpression) => errorExpression
        }

        if status is .Ok(val checkedValue) {
            _ = checkedValue
        }

        if status is Ok(val nominalValue) {
            _ = nominalValue
        }
    }
}
""";

        var tempRoot = Path.Combine(Path.GetTempPath(), "raven-pattern-hover-" + Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(tempRoot);

            var projectPath = Path.Combine(tempRoot, "PatternHover.rvnproj");
            File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>PatternHover</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

            var filePath = Path.Combine(tempRoot, "src", "main.rvn");
            Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);
            File.WriteAllText(filePath, code);

            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "pattern-hover",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var uri = DocumentUri.FromFileSystemPath(filePath);
            _ = await store.UpsertDocumentAsync(uri, code);

            var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
            context.ShouldNotBeNull();

            var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
            var root = context.Value.SyntaxTree.GetRoot();
            context.Value.Compilation.GetDiagnostics()
                .Where(static diagnostic => diagnostic.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error)
                .Select(static diagnostic => diagnostic.GetMessage())
                .ShouldBeEmpty();

            var memberPatternTokens = root.DescendantNodes()
                .OfType<MemberPatternPathSyntax>()
                .Select(path => path.Identifier)
                .Where(token => token.ValueText is "Ok" or "Error")
                .ToArray();

            memberPatternTokens.Length.ShouldBe(5);
            foreach (var token in memberPatternTokens)
            {
                var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

                resolution.ShouldNotBeNull();
                resolution!.Value.Symbol.Name.ShouldBe(token.ValueText);
                resolution.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>().IsUnionCase.ShouldBeTrue();
            }

            var nominalToken = root.DescendantNodes()
                .OfType<NominalDeconstructionPatternSyntax>()
                .SelectMany(pattern => pattern.Type.DescendantTokens())
                .Single(token => token.ValueText == "Ok");
            var nominalResolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, nominalToken.SpanStart + 1);

            nominalResolution.ShouldNotBeNull();
            nominalResolution!.Value.Symbol.Name.ShouldBe("Ok");
            nominalResolution.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>().IsUnionCase.ShouldBeTrue();
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public void ForPatternTargetDeclaration_HoverResolvesBoundLocal()
    {
        const string code = """
class C {
    func Run(points: (int, int)[]) -> int {
        var total = 0
        for (val x, 0) in points {
            total += x
        }

        return total
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declarationToken = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "x" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is ForStatementSyntax) == true &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is PatternSyntax) == true);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, declarationToken.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("x");
    }

    [Fact]
    public void DeclarationPatternHover_UsesDeclaredType()
    {
        const string code = """
class C {
    func Run(value: object) -> int {
        if value is string text {
            return text.Length
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "text" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is DeclarationPatternSyntax) == true);
        var tryBuildPatternDeclarationHover = typeof(HoverHandler)
            .GetMethod("TryBuildPatternDeclarationHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var hover = tryBuildPatternDeclarationHover.Invoke(
            null,
            [syntaxTree.GetText(), semanticModel, root, token.SpanStart + 1]);

        hover.ShouldNotBeNull();
        hover.ToString().ShouldContain("val text: string");
    }

    [Fact]
    public void ForPatternDeclarationHover_UsesOuterVarMutability()
    {
        const string code = """
class C {
    func Run() -> unit {
        val points: int[][] = [[2, 3], [2, 0], [5, 1]]
        for var [x, 0] in points {
            x
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "x" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is SequencePatternSyntax) == true &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is ForStatementSyntax) == true);
        var hoverOffset = token.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);
        resolution.ShouldNotBeNull();

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignatureForHover.Invoke(null, [resolution!.Value.Symbol, resolution.Value.Node, semanticModel, root, hoverOffset])!;
        signature.ShouldContain("var x:");
    }

    [Fact(Skip = "Stale pattern hover coverage: for-deconstruction pattern type inference needs redesign around compiler API results.")]
    public void ForDeconstructionPatternDeclarationHover_UsesDeconstructParameterTypes()
    {
        const string code = """
import System.*
import System.Collections.Immutable.*

record Person(val Name: string, val Age: int, val Items: string[])

class C {
    func Run() -> unit {
        val people = [Person("Ada", 42, ["tea", "cake"])]
        for val (name, age when > 18, [item1, item2]) in people {
            name.Length + age + item1.Length + item2.Length
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var tryBuildPatternDeclarationHover = typeof(HoverHandler)
            .GetMethod("TryBuildPatternDeclarationHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        foreach (var (name, expectedSignature) in new[]
                 {
                     ("name", "val name: string"),
                     ("age", "val age: int"),
                     ("item1", "val item1: string"),
                     ("item2", "val item2: string")
                 })
        {
            var token = root.DescendantTokens().First(t =>
                t.Kind == SyntaxKind.IdentifierToken &&
                t.ValueText == name &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is ForStatementSyntax) == true &&
                t.Parent?.AncestorsAndSelf().Any(static n => n is PatternSyntax) == true);

            var hover = tryBuildPatternDeclarationHover.Invoke(
                null,
                [syntaxTree.GetText(), semanticModel, root, token.SpanStart + 1]);

            hover.ShouldNotBeNull();
            hover.ToString().ShouldContain(expectedSignature);
        }
    }

    [Fact]
    public void SymbolResolver_PatternHover_DoesNotThrowWhenSiblingStatementContainsSpreadElement()
    {
        const string code = """
import System.*

class C {
    func Run() -> unit {
        val arr1 = [1, 2, 3]
        val arr2 = [1, ...arr1, 3]

        for val [..2, ..2 x, ...] in [[2, 1..4]] {
            x
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "x" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is SequencePatternSyntax) == true);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("x");
    }

    [Fact]
    public void SymbolResolver_UseDeclarationHover_DoesNotThrow()
    {
        const string code = """
import System.IO.*

class C {
    func Run() -> unit {
        use stream = MemoryStream()
        stream
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "stream" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is UseDeclarationStatementSyntax) == true);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("stream");
    }

    [Fact]
    public void SymbolResolver_MemberReceiverHover_ResolvesParameterFromNodeInterestPath()
    {
        const string code = """
class Counter {
    func Value() -> int => 0
}

class C {
    func Run(counter: Counter) -> int {
        counter.Value()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var memberAccess = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single();
        var token = memberAccess.Expression
            .DescendantTokens()
            .Single(t => t.Kind == SyntaxKind.IdentifierToken && t.ValueText == "counter");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.MemberReceiver);
        resolution.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>().Name.ShouldBe("counter");
    }

    [Fact]
    public void SymbolResolver_MemberReceiverHover_ResolvesLocalFromNodeInterestPath()
    {
        const string code = """
class Counter {
    func Value() -> int => 0
}

class C {
    func Run() -> int {
        val counter = Counter()
        counter.Value()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var memberAccess = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single();
        var token = memberAccess.Expression
            .DescendantTokens()
            .Single(t => t.Kind == SyntaxKind.IdentifierToken && t.ValueText == "counter");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.MemberReceiver);
        resolution.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("counter");
    }

    [Fact]
    public void SymbolResolver_IfPatternStatementHover_DoesNotThrow()
    {
        const string code = """
class C {
    func Run(person: (string, int)) -> string {
        if val (name, >= 18) = person {
            return name
        }

        return ""
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "name" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is IfPatternStatementSyntax) == true);

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("name");
    }

    [Fact]
    public void SymbolResolver_ObjectInitializerHover_DoesNotThrow()
    {
        const string code = """
class Person {
    var Name: string = ""
}

class C {
    func Run() -> unit {
        val person = Person {
            Name = ""
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "Name" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is AssignmentStatementSyntax) == true);

        Should.NotThrow(() => SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1));
    }

    [Fact]
    public void SymbolResolver_WithInitializerPropertyNameHover_ResolvesProperty()
    {
        const string code = """
record Options(val WriteIndented: bool)

class C {
    func Run() -> unit {
        val options = Options(false) with {
            WriteIndented = true
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes()
            .OfType<WithAssignmentSyntax>()
            .Single()
            .Name;
        var hoverOffset = identifier.Identifier.SpanStart + 1;

        var symbolInfo = semanticModel.GetSymbolInfo(identifier);
        symbolInfo.Symbol.ShouldBeAssignableTo<IPropertySymbol>().Name.ShouldBe("WriteIndented");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var property = resolution!.Value.Symbol.ShouldBeAssignableTo<IPropertySymbol>();
        property.Name.ShouldBe("WriteIndented");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [property, resolution.Value.Node, semanticModel, root, hoverOffset])!;

        signature.ShouldContain("WriteIndented:");
        signature.ShouldNotContain("<Error>");
    }

    [Fact]
    public void SymbolResolver_NamedConstructorArgumentHover_ResolvesParameter()
    {
        const string code = """
union class Option<T> {
    case Some(value: T)
    case None
}

record Foo(
    val Name: string
    val Item: Option<string>
    val Test: int | bool
)

class C {
    func Run() -> unit {
        val foo = Foo(
            Name: "Foo",
            Item: .Some("Foo"),
            Test: true
        )
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        foreach (var expectedName in new[] { "Name", "Item", "Test" })
        {
            var identifier = root.DescendantNodes()
                .OfType<ArgumentSyntax>()
                .Single(argument => argument.NameColon?.Name.Identifier.ValueText == expectedName)
                .NameColon!.Name;
            var hoverOffset = identifier.Identifier.SpanStart + 1;

            var symbolInfo = semanticModel.GetSymbolInfo(identifier);
            symbolInfo.Symbol.ShouldBeAssignableTo<IParameterSymbol>().Name.ShouldBe(expectedName);

            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

            resolution.ShouldNotBeNull();
            var parameter = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
            parameter.Name.ShouldBe(expectedName);

            var signature = (string)buildSignatureForHover.Invoke(
                null,
                [parameter, resolution.Value.Node, semanticModel, root, hoverOffset])!;

            signature.ShouldContain($"{expectedName}:");
            signature.ShouldNotContain("<Error>");
        }
    }

    [Fact]
    public void SymbolResolver_NamedFunctionArgumentHover_ResolvesParameter()
    {
        const string code = """
func StackPanel(spacing: double = 0, content: (() -> ())? = null) -> () {
}

class C {
    func Run() -> unit {
        StackPanel(spacing: 8.0) {
            Console.WriteLine("Hello")
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes()
            .OfType<ArgumentSyntax>()
            .Single(argument => argument.NameColon?.Name.Identifier.ValueText == "spacing")
            .NameColon!.Name;
        var hoverOffset = identifier.Identifier.SpanStart + 1;

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var parameter = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
        parameter.Name.ShouldBe("spacing");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;
        var signature = (string)buildSignatureForHover.Invoke(
            null,
            [parameter, resolution.Value.Node, semanticModel, root, hoverOffset])!;

        signature.ShouldContain("spacing:");
        signature.ShouldContain("double");
        signature.ShouldNotContain("<Error>");
    }

    [Fact]
    public void SymbolResolver_MemberBindingHover_ResolvesActualMember()
    {
        const string code = """
class Bar {
    init() {}
    static val Instance: Bar {
        get { return Bar() }
    }
}

class C {
    func Run() -> unit {
        val value: Bar = .Instance
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes()
            .OfType<MemberBindingExpressionSyntax>()
            .Single()
            .Name;
        var hoverOffset = identifier.Identifier.SpanStart + 1;

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<IPropertySymbol>().Name.ShouldBe("Instance");
        resolution.Value.Symbol.ShouldNotBeAssignableTo<ITypeSymbol>();
    }

    [Fact]
    public void SymbolResolver_InvocationTarget_ReusesCachedSymbolInfoWithoutBinding()
    {
        const string code = """
class C {
    func Test() {
        Print()
    }

    func Print() {
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var instrumentation = new PerformanceInstrumentation();
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                performanceInstrumentation: instrumentation));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var invocation = root.DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var identifier = (IdentifierNameSyntax)invocation.Expression;

        semanticModel.GetSymbolInfo(invocation).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        instrumentation.BinderReentry.Reset();

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, identifier.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        instrumentation.BinderReentry.TotalBindExecutions.ShouldBe(0);
    }

    [Fact]
    public void SymbolResolver_MemberSegment_ReusesCachedSymbolInfoWithoutBinding()
    {
        const string code = """
class Counter {
    private var count: int = 0

    func Test() {
        val current = self.count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var instrumentation = new PerformanceInstrumentation();
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            [.. LanguageServerTestReferences.Default],
            new CompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                performanceInstrumentation: instrumentation));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var memberAccess = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(static memberAccess => memberAccess.Name.Identifier.ValueText == "count");
        var memberName = (IdentifierNameSyntax)memberAccess.Name;

        semanticModel.GetSymbolInfo(memberName).Symbol.ShouldBeAssignableTo<IPropertySymbol>();

        instrumentation.BinderReentry.Reset();

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, memberName.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        resolution.Value.Symbol.ShouldBeAssignableTo<IPropertySymbol>();
        instrumentation.BinderReentry.TotalBindExecutions.ShouldBe(0);
    }

    [Fact]
    public void PipeInvocationTargetHover_UsesResolvedMethodSignature()
    {
        const string code = """
import System.*

class Runner {
    static func Where(value: Int32, predicate: (Int32) -> bool) -> Int32 {
        return value
    }

    static func Main() -> unit {
        val query = 5
            |> Where(x => x > 1)

        query
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var diagnostics = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var whereIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "Where");
        var buildDisplaySignatureForResolvedHover = typeof(HoverHandler)
            .GetMethod("BuildDisplaySignatureForResolvedHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        foreach (var hoverOffset in Enumerable.Range(whereIdentifier.Identifier.SpanStart, whereIdentifier.Identifier.Span.Length + 1))
        {
            var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

            resolution.ShouldNotBeNull();
            resolution!.Value.Kind.ShouldBe(SymbolResolutionKind.InvocationTarget);
            resolution!.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>().Name.ShouldBe("Where");

            var signature = (string)buildDisplaySignatureForResolvedHover.Invoke(null, [resolution.Value, semanticModel, root, hoverOffset])!;
            signature.ShouldContain("Where");
            signature.ShouldNotContain("val query");
            signature.ShouldNotContain("Error");
        }
    }

    [Fact]
    public void ConstructorInvocationHover_UsesConstructorSignatureInsteadOfType()
    {
        const string code = """
import System.*

class PingResult(val Message: int)

class Api {
    func ping(value: int) -> PingResult {
        PingResult(value)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "PingResult" &&
            t.Parent?.Parent is InvocationExpressionSyntax);
        var hoverOffset = token.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        var constructor = resolution!.Value.Symbol.ShouldBeAssignableTo<IMethodSymbol>();
        constructor.MethodKind.ShouldBe(MethodKind.Constructor);
        constructor.ContainingType.Name.ShouldBe("PingResult");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignatureForHover.Invoke(null, [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, hoverOffset])!;
        signature.ShouldContain("PingResult(");
        signature.ShouldNotBe("()");
    }

    [Fact]
    public void ReturnTypeIdentifierHover_UsesNamedTypeSignature()
    {
        const string code = """
class Api {
    func ping(name: string) -> PingResult {
        PingResult("pong $name")
    }
}

record PingResult(val Message: string)
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "PingResult" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is ArrowTypeClauseSyntax) == true);
        var hoverOffset = token.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>().Name.ShouldBe("PingResult");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignatureForHover.Invoke(null, [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, hoverOffset])!;
        signature.ShouldContain("PingResult");
        signature.ShouldNotBe("()");
    }

    [Fact]
    public void GenericTypeArgumentHover_UsesNamedTypeSignature()
    {
        const string code = """
record PingResult(val Message: string)
record CustomError(val Message: string)

class Api {
    func ping(name: string) -> Result<PingResult, CustomError> {
        Error(CustomError("bad"))
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var token = root.DescendantTokens().First(t =>
            t.Kind == SyntaxKind.IdentifierToken &&
            t.ValueText == "CustomError" &&
            t.Parent?.AncestorsAndSelf().Any(static n => n is ArrowTypeClauseSyntax) == true);
        var hoverOffset = token.SpanStart + 1;
        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, hoverOffset);

        resolution.ShouldNotBeNull();
        resolution!.Value.Symbol.ShouldBeAssignableTo<ITypeSymbol>().Name.ShouldBe("CustomError");

        var buildSignatureForHover = typeof(HoverHandler)
            .GetMethod("BuildSignatureForHover", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignatureForHover.Invoke(null, [resolution.Value.Symbol, resolution.Value.Node, semanticModel, root, hoverOffset])!;
        signature.ShouldContain("CustomError");
        signature.ShouldNotBe("()");
    }

    private static (SymbolResolutionResult? Resolution, SemanticModel SemanticModel) ResolveTypeAnnotationAtIdentifier(
        string code,
        string declaratorName,
        string tokenText,
        bool includeRavenCore = false)
    {
        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        if (includeRavenCore)
        {
            compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
            compilation = compilation.AddReferences(MetadataReference.CreateFromFile(GetRavenCoreReferencePath()));
        }
        else
        {
            foreach (var reference in LanguageServerTestReferences.Default)
                compilation = compilation.AddReferences(reference);

            compilation = compilation.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));
        }

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == declaratorName);
        var typeSyntax = declarator.TypeAnnotation!.Type;
        var token = typeSyntax.DescendantTokens().First(token => token.ValueText == tokenText);

        return (
            SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1),
            semanticModel);
    }

    private static string GetRavenCoreReferencePath()
    {
        var outputPath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (File.Exists(outputPath))
            return outputPath;

        return Path.GetFullPath(Path.Combine(
            AppContext.BaseDirectory,
            "..",
            "..",
            "..",
            "..",
            "..",
            "src",
            "Raven.Core",
            "bin",
            "Debug",
            "net10.0",
            "Raven.Core.dll"));
    }

    private static (SymbolResolutionResult? Resolution, SemanticModel SemanticModel) ResolveParameterTypeAtIdentifier(
        string code,
        string parameterName,
        string tokenText)
    {
        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        _ = compilation.GetDiagnostics();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var parameter = root.DescendantNodes()
            .OfType<ParameterSyntax>()
            .Single(parameter => parameter.Identifier.ValueText == parameterName);
        var typeSyntax = parameter.TypeAnnotation!.Type;
        var token = typeSyntax.DescendantTokens().First(token => token.ValueText == tokenText);

        return (
            SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1),
            semanticModel);
    }

}
