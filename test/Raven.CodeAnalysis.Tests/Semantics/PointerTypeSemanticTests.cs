using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PointerTypeSemanticTests : CompilationTestBase
{
    [Fact]
    public void PointerTypeSyntax_BindsToPointerTypeSymbol()
    {
        const string source = """
class C {
    unsafe static Test() {
        val value = 0
        val pointer: *int = &value
    }
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "pointer");

        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);
        var pointerType = Assert.IsAssignableFrom<IPointerTypeSymbol>(typeInfo.Type);
        Assert.Equal(SpecialType.System_Int32, pointerType.PointedAtType.SpecialType);
    }

    [Fact]
    public void AddressOfInitializer_DefaultsToByRefType()
    {
        const string source = """
class C {
    static Test() {
        var value = 0
        val alias = &value
    }
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "alias");

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var byRef = Assert.IsType<ByRefTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, byRef.ElementType.SpecialType);
    }

    [Fact]
    public void AddressOfInitializer_WithPointerAnnotation_RemainsPointer()
    {
        const string source = """
class C {
    unsafe static Test() {
        var value = 0
        val alias: *int = &value
    }
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "alias");

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var pointer = Assert.IsAssignableFrom<IPointerTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, pointer.PointedAtType.SpecialType);
    }

    [Fact]
    public void AddressOfInstanceField_ProducesByRefLocal()
    {
        const string source = """
class Buffer {
    var head: int = 0

    static Pin(storage: &Buffer) {
        var alias = &storage.head
        alias = 5
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "alias");
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var byRef = Assert.IsType<ByRefTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, byRef.ElementType.SpecialType);
    }

    [Fact]
    public void AddressOfStaticField_ProducesByRefLocal()
    {
        const string source = """
class Counter {
    static var total: int = 0

    static Pin() {
        var alias = &Counter.total
        alias = 1
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "alias");
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var byRef = Assert.IsType<ByRefTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, byRef.ElementType.SpecialType);
    }

    [Fact]
    public void AddressOfArrayElement_ProducesByRefLocal()
    {
        const string source = """
class Data {
    static Pin() {
        var numbers: int[] = [1, 2, 3]
        var slot = &numbers[0]
        slot = 42
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "slot");
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var byRef = Assert.IsType<ByRefTypeSymbol>(local.Type);
        Assert.Equal(SpecialType.System_Int32, byRef.ElementType.SpecialType);
    }

    [Fact]
    public void PointerTypeSyntax_WithoutUnsafe_ReportsDiagnostic()
    {
        const string source = """
val value = 0
val pointer: *int = &value
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(false);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error),
            d => d.Id == CompilerDiagnostics.PointerTypeRequiresUnsafe.Id);
    }

    [Fact]
    public void GlobalUnsafeMode_ReportsWarningDiagnostic()
    {
        const string source = "val value = 0";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(true);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d =>
            d.Id == CompilerDiagnostics.UnsafeModeEnabled.Id &&
            d.Severity == DiagnosticSeverity.Warning);
    }

    [Fact]
    public void PointerDereference_ReadAndWrite_BindsWithoutErrors()
    {
        const string source = """
class Test {
    unsafe static Run() -> int {
        var value = 41
        val pointer: *int = &value
        *pointer = 42
        return *pointer
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var dereference = tree.GetRoot()
            .DescendantNodes()
            .OfType<UnaryExpressionSyntax>()
            .First(u => u.Kind == SyntaxKind.DereferenceExpression);

        var typeInfo = model.GetTypeInfo(dereference);
        Assert.Equal(SpecialType.System_Int32, typeInfo.Type?.SpecialType);
    }

    [Fact]
    public void PointerArrowMemberAccess_ResolvesMemberOnPointedType()
    {
        const string source = """
struct Holder {
    public var Value: int = 42
}

class Test {
    unsafe static Run() -> int {
        var holder = Holder()
        val pointer: *Holder = &holder
        return pointer->Value
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));
    }

    [Fact]
    public void PointerDereference_WithoutUnsafe_ReportsDiagnostic()
    {
        const string source = """
class Test {
    static Run() -> int {
        var value = 0
        val pointer: *int = &value
        return *pointer
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(false);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error),
            d => d.Id == CompilerDiagnostics.PointerOperationRequiresUnsafe.Id);
    }

    [Fact]
    public void PointerArrowMemberAccess_WithNonPointerReceiver_ReportsDiagnostic()
    {
        const string source = """
class Holder {
    var Value: int = 42
}

class Test {
    unsafe static Run() -> int {
        var holder = Holder()
        return holder->Value
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error),
            d => d.Id == CompilerDiagnostics.PointerMemberAccessRequiresPointer.Id);
    }

    [Fact]
    public void PointerOperations_InsideUnsafeBlock_WorkWithoutGlobalUnsafeFlag()
    {
        const string source = """
class Test {
    static Run() -> int {
        var value = 0
        unsafe {
            val pointer: *int = &value
            *pointer = 7
        }
        return value
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(false);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));
    }

    [Fact]
    public void PointerOperations_InsideUnsafeMethod_WorkWithoutGlobalUnsafeFlag()
    {
        const string source = """
class Test {
    unsafe static Run() -> int {
        var value = 0
        val pointer: *int = &value
        *pointer = 9
        return value
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(false);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));
    }

    [Fact]
    public void PointerArithmetic_AdditionAndSubtraction_BindsWithoutErrors()
    {
        const string source = """
class Test {
    unsafe static Run() -> nint {
        var value = 0
        val pointer: *int = &value;
        val p1 = pointer + 1
        val p2 = 2 + pointer
        val p3 = p1 - 1
        p3 - pointer
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));

        var model = compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToDictionary(x => x.Identifier.Text, x => x);

        var p1 = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["p1"]));
        var p2 = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["p2"]));
        var p3 = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["p3"]));

        Assert.IsAssignableFrom<IPointerTypeSymbol>(p1.Type);
        Assert.IsAssignableFrom<IPointerTypeSymbol>(p2.Type);
        Assert.IsAssignableFrom<IPointerTypeSymbol>(p3.Type);

        var returnExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<BinaryExpressionSyntax>()
            .Single(x => x.OperatorToken.Kind == SyntaxKind.MinusToken && x.Left.ToString() == "p3" && x.Right.ToString() == "pointer");

        var typeInfo = model.GetTypeInfo(returnExpression);
        Assert.Equal(SpecialType.System_IntPtr, typeInfo.Type?.SpecialType);
    }

    [Fact]
    public void PointerArithmetic_WithoutUnsafe_ReportsDiagnostic()
    {
        const string source = """
class Test {
    static Run() -> *int {
        var value = 0
        val pointer: *int = &value
        pointer + 1
    }
}
""";

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(false);
        var (compilation, _) = CreateCompilation(source, options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error),
            d => d.Id == CompilerDiagnostics.PointerTypeRequiresUnsafe.Id ||
                 d.Id == CompilerDiagnostics.PointerOperationRequiresUnsafe.Id);
    }
}
