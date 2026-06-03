using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class InvocationOperatorTests : CompilationTestBase
{
    [Fact]
    public void InvocationOperator_BindsToInvokeMethod()
    {
        var source = """
class Test {
    public func self(no: int) -> int {
        return no + 1;
    }
}
val t = Test()
val x = t(2)
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Invoke", symbol.Name);
    }

    [Fact]
    public void InvocationOperator_InterfaceDeclaration_BindsInvokeMethod()
    {
        var source = """
interface ICallable {
    func self(value: int) -> int;
}

class Callable : ICallable {
    public func self(value: int) -> int {
        return value + 1;
    }
}

val callable: ICallable = Callable()
val result = callable(2)
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        Assert.Equal("Invoke", symbol.Name);
        Assert.Equal(TypeKind.Interface, symbol.ContainingType.TypeKind);
        Assert.Equal("ICallable", symbol.ContainingType.Name);
    }

    [Fact]
    public void GetSymbolInfo_UnqualifiedCallableInstanceMemberInvocation_ReturnsInstanceMember()
    {
        var source = """
class Handler {
    private val callback: () -> ()

    init(callback: () -> ()) {
        self.callback = callback
    }

    func Handle() -> () {
        callback()
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var callbackIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier =>
                identifier.Identifier.ValueText == "callback" &&
                identifier.Parent is InvocationExpressionSyntax);

        var symbol = model.GetSymbolInfo(callbackIdentifier).Symbol;

        Assert.NotNull(symbol);
        Assert.False(symbol.IsStatic);
        Assert.Equal("callback", symbol.Name);
        Assert.Equal("Handler", symbol.ContainingType?.Name);
    }

    [Fact]
    public void InvocationOperator_Modifiers_AreTrackedOnSymbols()
    {
        var source = """
abstract class Base {
    public abstract func self(value: int) -> int;
}

class Derived : Base {
    public override func self(value: int) -> int {
        return value;
    }
}

open class VirtualBase {
    public virtual func self(value: int) -> int {
        return value;
    }
}

class VirtualDerived : VirtualBase {
    public override func self(value: int) -> int {
        return value + 2;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var baseType = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Base");
        var baseMethod = baseType.Members.OfType<MethodDeclarationSyntax>().Single();
        var baseSymbol = (IMethodSymbol)model.GetDeclaredSymbol(baseMethod)!;
        Assert.True(baseSymbol.IsAbstract);
        Assert.True(baseSymbol.IsVirtual);

        var derivedType = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Derived");
        var derivedMethod = derivedType.Members.OfType<MethodDeclarationSyntax>().Single();
        var derivedSymbol = (IMethodSymbol)model.GetDeclaredSymbol(derivedMethod)!;
        Assert.True(derivedSymbol.IsOverride);

        var virtualBaseType = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "VirtualBase");
        var virtualBaseMethod = virtualBaseType.Members.OfType<MethodDeclarationSyntax>().Single();
        var virtualBaseSymbol = (IMethodSymbol)model.GetDeclaredSymbol(virtualBaseMethod)!;
        Assert.True(virtualBaseSymbol.IsVirtual);
        Assert.False(virtualBaseSymbol.IsOverride);
    }

    [Fact]
    public void InvocationOperator_AfterNotNullCheck_BindsToInvokeMethod()
    {
        var source = """
class Foo {
    public func self(value: int) -> int {
        return value + 1;
    }
}

func test(foo: Foo?) -> int {
    if foo is not null {
        return foo(2)
    }

    return 0
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("Invoke", symbol.Name);
        Assert.DoesNotContain(
            diagnostics,
            d => d.Severity == DiagnosticSeverity.Error &&
                 d.Descriptor != CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }

    [Fact]
    public void InvocationOperator_AwaitedAfterNotNullCheck_BindsInvokeAndAwaitedResult()
    {
        var source = """
import System.Threading.Tasks.*

union Option<T> {
    case Some(value: T)
    case None
}

class Foo(value: int) {
    async func Test(flag: bool) -> Task<Option<int>> {
        await Task.Yield()

        if flag {
            return .Some(value)
        }

        return .None
    }

    func self(flag: bool) -> Task<Option<int>> {
        return Test(flag)
    }
}

async func Run(foo: Foo?) -> Task<Option<int>> {
    if foo is not null {
        val result = await foo(true)
        return result
    }

    return .None
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var invocation = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is IdentifierNameSyntax { Identifier.ValueText: "foo" });
        var invokeSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("Invoke", invokeSymbol.Name);
        Assert.Equal("Foo", invokeSymbol.ContainingType.Name);
        Assert.Equal("Task<Option<int>>", model.GetTypeInfo(invocation).Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var awaitExpression = root.DescendantNodes()
            .OfType<PrefixOperatorExpressionSyntax>()
            .Single(node => node.Kind == SyntaxKind.AwaitExpression && node.Expression == invocation);
        Assert.Equal("Option<int>", model.GetTypeInfo(awaitExpression).Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var resultDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(node => node.Identifier.ValueText == "result");
        var result = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        Assert.Equal("Option<int>", result.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void InvocationOperator_DiagnosticsOnly_BindsToInvokeMethod()
    {
        var source = """
class Foo {
    func self(value: int) -> int {
        value + 1
    }
}

func test(foo: Foo) -> int {
    foo(2)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            d => d.Severity == DiagnosticSeverity.Error &&
                 d.Descriptor == CompilerDiagnostics.NonInvocableMember);
    }
}
