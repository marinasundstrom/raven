using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionTests : CompilationTestBase
{
    [Fact]
    public void Function_WithoutReturnType_DefaultsToVoid()
    {
        var source = """
func outer() {
    func inner() { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        Assert.Equal(SpecialType.System_Unit, symbol.ReturnType.SpecialType);
    }

    [Fact]
    public void AsyncFunction_WithoutReturnType_DefaultsToTask()
    {
        var source = """
func outer() {
    async func inner() { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        Assert.True(symbol.IsAsync);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task, symbol.ReturnType.SpecialType);
    }

    [Fact]
    public void AsyncFunction_WithoutReturnType_WithReturnExpression_DefaultsToTask()
    {
        var source = """
func outer() {
    async func inner() {
        return 1
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;

        Assert.True(symbol.IsAsync);
        Assert.Equal(
            "class System.Threading.Tasks.Task",
            symbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.AsyncLacksAwait.Id);
    }

    [Fact]
    public void Function_WithTupleAndFunctionSignature_BindsWithBindTypeSyntaxPath()
    {
        var source = """
func outer() {
    func inner(callback: (int, string) -> bool) -> (left: int, right: string) {
        return (1, "ok")
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(inner));

        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var parameter = Assert.Single(symbol.Parameters);
        var callbackType = Assert.IsAssignableFrom<INamedTypeSymbol>(parameter.Type);
        Assert.Equal(TypeKind.Delegate, callbackType.TypeKind);
        var invoke = callbackType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);
        Assert.Equal(2, invoke.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);

        var tupleReturn = Assert.IsAssignableFrom<ITupleTypeSymbol>(symbol.ReturnType);
        Assert.Equal(2, tupleReturn.TupleElements.Length);
        Assert.Equal("left", tupleReturn.TupleElements[0].Name);
        Assert.Equal(SpecialType.System_Int32, tupleReturn.TupleElements[0].Type.SpecialType);
        Assert.Equal("right", tupleReturn.TupleElements[1].Name);
        Assert.Equal(SpecialType.System_String, tupleReturn.TupleElements[1].Type.SpecialType);
    }

    [Fact]
    public void Function_LocalStructDeclaration_IsVisibleToAdjacentLocalFunction_AndEmits()
    {
        const string source = """
func outer() -> int {
    func makeBox() -> CounterBox {
        return CounterBox(42)
    }

    struct CounterBox(value: int) { }

    val box = makeBox()
    return box.value
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var localStruct = tree.GetRoot().DescendantNodes().OfType<StructDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(localStruct));
        Assert.Equal(TypeKind.Struct, symbol.TypeKind);

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);

        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void GenericFunction_CanInferTypeArgumentsForInvocation()
    {
        var source = """
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

async func Outer() -> Task<int> {
    val result = await Test(42)
    return result
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var functions = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().ToArray();
        var testFunction = functions.Single(f => f.Identifier.Text == "Test");
        var testSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(testFunction));
        Assert.True(testSymbol.IsGenericMethod);
        Assert.Single(testSymbol.TypeParameters);
        Assert.Equal("T", testSymbol.TypeParameters[0].Name);
        Assert.Equal(
            "class System.Threading.Tasks.Task<T>",
            testSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var invocation = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax id && id.Identifier.Text == "Test");
        var symbolInfo = model.GetSymbolInfo(invocation);
        var invokedSymbol = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.Equal(testSymbol, invokedSymbol.OriginalDefinition);
        Assert.Single(invokedSymbol.TypeArguments);
        Assert.Equal(
            compilation.GetSpecialType(SpecialType.System_Int32),
            invokedSymbol.TypeArguments[0]);

        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.Id != CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id);
    }

    [Fact]
    public void DuplicateFunction_DiagnosticReported()
    {
        var source = """
func test() {}
func test() {}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var funcs = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(funcs[0]);
        _ = model.GetDeclaredSymbol(funcs[1]);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.FunctionAlreadyDefined);
    }

    [Fact]
    public void Function_WithAttribute_BindsDeclaredAttributes()
    {
        var source = """
[System.Obsolete]
func test() {}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var symbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(function));
        var attributes = symbol.GetAttributes();

        Assert.Contains(attributes, static a => a.AttributeClass?.Name is "ObsoleteAttribute");
    }

    [Fact]
    public void FunctionStatement_CapturesOuterLocal_ByDefault()
    {
        const string source = """
func Main() {
    val x: int = 2

    func Foo() {
        val v = x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "Foo");

        var captures = model.GetCapturedVariables(function);
        Assert.Contains(captures, static s => s is ILocalSymbol { Name: "x" });
    }

    [Fact]
    public void StaticFunctionStatement_DoesNotCaptureOuterLocal()
    {
        const string source = """
func Main() {
    val x: int = 2

    static func Foo() {
        val v = x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext &&
                        d.GetMessage().Contains("'x' is not in scope."));
    }

    [Fact]
    public void StaticMember_SelfExpression_ProducesDiagnostic()
    {
        const string source = """
class Program {
    static func Main() -> int {
        return self.value
    }

    val value: int {
        get => 1
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.SelfNotAvailableInStaticContext);
    }

    [Fact]
    public void StaticFunctionStatement_SelfExpression_ProducesDiagnostic()
    {
        const string source = """
class Program {
    func Main() -> int {
        static func Inner() -> int {
            return self.value
        }

        return Inner()
    }

    val value: int {
        get => 1
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.SelfNotAvailableInStaticContext);
    }

    [Fact]
    public void FunctionStatement_InMethodBody_CapturesOuterLocal()
    {
        const string source = """
class Program {
    public static func Run() -> int {
        val x: int = 2

        func Foo() -> int {
            return x
        }

        return Foo()
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "Foo");

        var captures = model.GetCapturedVariables(function);
        Assert.Contains(captures, static s => s is ILocalSymbol { Name: "x" });
    }

    [Fact]
    public void FunctionStatement_DoesNotCaptureLocalsDeclaredInNestedFunctionExpression()
    {
        const string source = """
class Program {
    static func Run() -> () {
        val f = func Fib(n: int) -> int {
            val x = if n < 2
                n
            else
                Fib(n - 1) + Fib(n - 2)
            x
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var run = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(f => f.Identifier.ValueText == "Run");
        var runSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(run));

        var captures = model.GetCapturedVariables(runSymbol);
        Assert.DoesNotContain(captures, static s => s is ILocalSymbol { Name: "x" });
    }

    [Fact]
    public void CapturedLocal_InMethodBody_IsReportedAsCapturedVariable()
    {
        const string source = """
class Program {
    public static func Run() -> int {
        val x: int = 2

        func Foo() -> int {
            return x
        }

        return Foo()
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var localDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.ValueText == "x");
        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(localDeclarator));

        Assert.True(model.IsCapturedVariable(localSymbol));
    }

    [Fact]
    public void SemanticModel_ReportsCapturedVariable_ForIdentifier()
    {
        const string source = """
func Main() {
    val x: int = 2

    func Foo() {
        val v = x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var xIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "x");

        var symbol = model.GetSymbolInfo(xIdentifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(model.IsCapturedVariable(symbol!));
    }
}
