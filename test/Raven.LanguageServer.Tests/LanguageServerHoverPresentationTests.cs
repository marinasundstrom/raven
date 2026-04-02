using System.IO;
using System.Linq;
using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public class LanguageServerHoverPresentationTests
{
    [Fact]
    public void PromotedPrimaryConstructorParameter_ShowsPropertyKindAndContainingType()
    {
        const string code = """
class Foo(private var name: string) {
    func Test() -> string => name
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
record ApplicationError(val Message: string)
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
        var symbol = semanticModel.GetDeclaredSymbol(parameterSyntax).ShouldBeAssignableTo<IParameterSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [symbol, parameterSyntax, semanticModel])!;
        signature.ShouldStartWith("val Message:");
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

    [Fact]
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
    public async Task LocalHover_UsesCompactUnionTypeSignatureAsync()
    {
        const string code = """
func Main() -> unit {
    val invoiceTotal: Either<int, string> = 42
}

union Either<T1, T2>(T1, T2)
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
            store.UpsertDocument(uri, code);

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
                seed
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

        var hoverOffset = lambdaParameter.TypeAnnotation!.ColonToken.SpanStart + 1;
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
            store.UpsertDocument(uri, code);

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

    [Fact]
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
            t.Parent?.AncestorsAndSelf().Any(static n => n is ObjectInitializerAssignmentEntrySyntax) == true);

        Should.NotThrow(() => SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, token.SpanStart + 1));
    }
}
