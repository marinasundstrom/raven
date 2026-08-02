using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ByRefParameterTests : CompilationTestBase
{
    [Fact]
    public void Parameter_WithRefKeyword_HasRefKindRef()
    {
        var source = """
class C {
    func test(ref x: int) -> unit { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        var symbol = methodSymbol.Parameters.Single();
        Assert.Equal(RefKind.Ref, symbol.RefKind);
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
        Assert.True(symbol.IsMutable);
    }

    [Fact]
    public void OutParameter_HasRefKindOut()
    {
        var source = """
class C {
    func test(out x: int) -> unit { x = 1 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        var symbol = methodSymbol.Parameters.Single();
        Assert.Equal(RefKind.Out, symbol.RefKind);
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
        Assert.True(symbol.IsMutable);
    }

    [Fact]
    public void OutParameter_UnassignedOnNormalExit_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Assign(out value: int) {
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_AssignedOnBothIfBranches_DoesNotReportDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        if flag {
            value = 1
        }
        else {
            value = 0
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void OutParameter_EarlyNullExitAlignsAssignmentAndNullState(bool diagnosticsFirst)
    {
        const string source = """
class C {
    func TryGetLength(value: string?, out length: int) -> bool {
        if value is null {
            length = 0
            return false
        }

        length = value.Length
        return true
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single()
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(NullableAnnotation.Annotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void OutParameter_AssignedOnEveryExhaustiveMatchArm_DoesNotReportDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        match flag {
            true => {
                value = 1
            }
            false => {
                value = 0
            }
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_AssignedOnOnlyOneMatchArm_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        match flag {
            true => {
                value = 1
            }
            false => ()
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_AssignedInEveryPresentNonExhaustiveMatchArm_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        match flag {
            true => {
                value = 1
            }
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void OutParameter_NonExhaustiveMatchAssignmentAgreesWithExhaustivenessQuery(bool diagnosticsFirst)
    {
        const string source = """
            class C {
                func Assign(flag: bool, out value: int) {
                    match flag {
                        true => {
                            value = 1
                        }
                    }
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        else
            _ = model.GetMatchExhaustiveness(match);

        var diagnostics = compilation.GetDiagnostics();
        var info = model.GetMatchExhaustiveness(match);

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);
        Assert.False(info.IsExhaustive);
        Assert.Equal("false", Assert.Single(info.MissingCases));
    }

    [Fact]
    public void OutParameter_ExhaustiveMatchReturnBeforeAssignment_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        match flag {
            true => return
            false => {
                value = 0
            }
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_ExhaustiveMatchThrowOrAssignment_DoesNotReportDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        match flag {
            true => throw System.Exception()
            false => {
                value = 0
            }
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_NonTerminatingLoop_DoesNotReportDiagnostic()
    {
        const string source = """
class C {
    func Wait(out value: int) {
        loop {
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_BreakableLoop_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Wait(out value: int) {
        loop {
            break
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_AssignedBeforeEveryLoopExit_DoesNotReportDiagnostic()
    {
        const string source = """
class C {
    func Assign(out value: int) {
        loop {
            value = 1
            break
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_UnassignedOnOneConditionalLoopExit_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        loop {
            if flag {
                break
            }

            value = 1
            break
        }
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void OutParameter_ReturnBeforeAssignment_ReportsDiagnostic()
    {
        const string source = """
class C {
    func Assign(flag: bool, out value: int) {
        if flag {
            return
        }

        value = 1
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnassignedOutParameter);
    }

    [Fact]
    public void ConstructorParameter_WithRefKeyword_HasRefKindRef()
    {
        var source = """
class C {
    init(ref x: int) { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var ctorSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ctor)!;
        var parameter = ctorSymbol.Parameters.Single();
        Assert.Equal(RefKind.Ref, parameter.RefKind);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
        Assert.True(parameter.IsMutable);
    }

    [Fact]
    public void Constructor_OutParameter_HasRefKindOut()
    {
        var source = """
class C {
    init(out x: int) { x = 0 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var ctorSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ctor)!;
        var parameter = ctorSymbol.Parameters.Single();
        Assert.Equal(RefKind.Out, parameter.RefKind);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
        Assert.True(parameter.IsMutable);
    }

    [Fact]
    public void FunctionParameter_WithRefKeyword_HasRefKindRef()
    {
        var source = """
func outer() {
    func inner(ref x: int) { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        var parameter = symbol.Parameters.Single();
        Assert.Equal(RefKind.Ref, parameter.RefKind);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
        Assert.True(parameter.IsMutable);
    }

    [Fact]
    public void Function_OutParameter_HasRefKindOut()
    {
        var source = """
func outer() {
    func inner(out x: int) { x = 1 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        var parameter = symbol.Parameters.Single();
        Assert.Equal(RefKind.Out, parameter.RefKind);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
        Assert.True(parameter.IsMutable);
    }

    [Fact]
    public void Method_ByRefParameter_Passed_To_Other_ByRefMethod_Compiles()
    {
        var source = """
class C {
    static func Set(ref x: int) { x = 1 }
    static func Pass(ref x: int) { Set(ref x) }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_ByRefParameter_WithAddressOfLocal_Compiles()
    {
        var source = """
class C {
    static func Set(ref value: int) -> unit { value = 42 }

    static func Run() -> unit {
        var data = 0
        Set(ref data)
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_OutParameter_WithAddressOfLocal_Compiles()
    {
        var source = """
class C {
    static func TryParse(text: string, out result: int) -> bool {
        result = 1
        return true
    }

    static func Consume(arg: string) -> bool {
        var total = 0
        if !TryParse(arg, out total) {
            return false
        }

        total = total + 1
        return true
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_OutParameter_WithDeclaredVarLocal_Compiles()
    {
        var source = """
class C {
    static func TryParse(text: string, out result: int) -> bool {
        result = 1
        return true
    }

    static func Consume(arg: string) -> int {
        if !TryParse(arg, out var total) {
            return -1
        }

        total = total + 1
        return total
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_OutParameter_WithDeclaredValLocal_IsReadOnlyAfterCall()
    {
        var source = """
class C {
    static func TryParse(text: string, out result: int) -> bool {
        result = 1
        return true
    }

    static func Consume(arg: string) -> int {
        if !TryParse(arg, out val total) {
            return -1
        }

        total = total + 1
        return total
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == CompilerDiagnostics.ThisValueIsNotMutable.Id);
    }

    [Fact]
    public void Method_OutParameter_WithBindingKeyword_ReportsDiagnostic()
    {
        var source = """
class C {
    static func TryParse(text: string, out var result: int) -> bool {
        result = 0
        return true
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id);
    }

    [Theory]
    [InlineData("ref")]
    [InlineData("out")]
    [InlineData("in")]
    public void Method_ParameterModifier_CannotBeCombinedWithByRefType(string modifier)
    {
        var source = $$"""
class C {
    static func M({{modifier}} value: &int) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.ParameterModifierCannotBeCombinedWithByRefType.Id);
    }

    [Fact]
    public void Method_ValueParameter_WithVar_ReportsDiagnostic()
    {
        var source = """
class C {
    static func Increment(var value: int) -> int {
        value = value + 1
        return value
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == CompilerDiagnostics.ParameterBindingKeywordNotAllowed.Id);
        Assert.Contains(diagnostics, d => d.Id == CompilerDiagnostics.ThisValueIsNotMutable.Id);
    }

    [Fact]
    public void Method_ByRefReturn_AddressOfLocal_ReportsDiagnostic()
    {
        var source = """
class C {
    static func MakeRef() -> &int {
        let x = 2
        return &x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.ByRefReturnCannotReferenceLocal.Id, diagnostic.Id);
    }

    [Fact]
    public void Method_ByRefReturn_AddressOfValueParameter_ReportsDiagnostic()
    {
        var source = """
class C {
    static func Forward(x: int) -> &int {
        return &x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.ByRefReturnCannotReferenceValueParameter.Id, diagnostic.Id);
    }

}
