using System.Reflection;

using Raven.CodeAnalysis;
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
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

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
    public void EventSubscriptionLambdaParameter_HoverUsesInferredDelegateParameterType()
    {
        const string code = """
import System.*

class ChangedArgs(var PropertyName: string)

delegate PropertyChangedHandler(sender: object?, e: ChangedArgs) -> unit

class Program {
    static func Log(value: string) -> unit { }

    static func Main() -> unit {
        val handler: PropertyChangedHandler = (sender, args) => {
            Log(args.PropertyName ?? "")
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
        compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ShouldBeEmpty();
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

}
