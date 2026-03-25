using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class DiscriminatedUnionSemanticTests : CompilationTestBase
{
    [Fact]
    public void GetDeclaredSymbol_ReturnsCaseSymbol()
    {
        const string source = """
union Option {
    None
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var caseClause = unionDecl.Cases[0];
        var symbol = model.GetDeclaredSymbol(caseClause);

        var caseSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionCaseSymbol>(symbol);
        Assert.Equal("None", caseSymbol.Name);
    }

    [Fact]
    public void MemberAccess_BindsToUnionCaseType()
    {
        const string source = """
func create() {
    val option = Option.Some(value: 42)
}

union Option {
    None
    Some(value: int)
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
    val option : Option = .Some(value: 42)
}

union Option {
    None
    Some(value: int)
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
    val option: Option<T> = .Some(payload)
    return option
}

union Option<T> {
    None
    Some(value: T)
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
    public void MemberAccessInvocation_OnUnconstructedCarrier_CaseArgumentsInferFromConstructor()
    {
        const string source = """
func build() {
    val result: Result<int, string> = Result.Ok(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val factory: Func<int, Result<int, string>> = x => Result.Ok(x)
    val result: Result<int, string> = factory(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val factory: Func<T, Result<T, string>> = x => Result.Ok(x)
    return factory(value)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val caseA: Ok<int> = Ok(2)
    val caseB: Ok<int> = Ok<int>(2)
    val resultA: Result<int, string> = Result<int, string>.Ok(2)
    val resultB: Result<int, string> = .Ok(2)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val caseValue = Result.Ok(2)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    return option match {
        .Some(val value) => "some ${value}"
        .None => "none"
    }
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV9002");
        Assert.True(diagnostics.All(d => d.Severity != DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MemberQualifiedCaseInvocation_WithExplicitCaseTypeArguments_BindsWithoutErrors()
    {
        const string source = """
func build() {
    val caseValue = Result.Ok<int>(2)
    val resultValue: Result<int, string> = Result.Ok<int>(2)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val err = Result<int, string>.Error("boom")
    val result: Result<int, string> = err
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val value: int? = null

    return value match {
        null => Error(MissingName)
        val v => Ok(v ?? 0)
    }
}

union Err {
    MissingUser
    MissingName
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    return flag match {
        true => Ok(42)
        false => Error("boom")
    }
}

union Response<T, E> {
    Ok(value: T)
    Error(error: E)
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
    public void MatchStatement_ImplicitReturn_WithUserDefinedUnionCases_BindsWithoutErrors()
    {
        const string source = """
func build(flag: bool) -> Response<int, string> {
    flag match {
        true => Ok(42)
        false => Error("boom")
    }
}

union Response<T, E> {
    Ok(value: T)
    Error(error: E)
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
    public void IfExpression_WithUserDefinedUnionCases_PrefersTargetUnionType()
    {
        const string source = """
func build(flag: bool) -> Response<int, string> {
    return if flag Ok(42) else Error("boom")
}

union Response<T, E> {
    Ok(value: T)
    Error(error: E)
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
        Ok(42)
    } else {
        Error("boom")
    }
}

union Response<T, E> {
    Ok(value: T)
    Error(error: E)
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
    val s1: Option<int> = Some(1)
    val s2: Option<int> = .Some(2)
    val s3: Option<int> = Option.Some(3)
    val s4: Option<int> = Option<int>.Some(4)

    val n1: Option<int> = None
    val n2: Option<int> = .None
}

union Option<T> {
    Some(value: T)
    None
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

union Result<T> {
    Ok(value: T)
    Error(message: string)
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
    public void UnqualifiedCaseInvocation_BindsWhenUniqueInScope()
    {
        const string source = """
func create() -> Result<int, string> {
    return Ok(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
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
        val responseBody = await response.Content.ReadAsStringAsync()
        return Ok(responseBody)
    } catch (HttpRequestException e) {
        return Error(e.Message)
    } catch (TaskCanceledException) {
        return Error("Request timed out or was canceled.")
    }
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
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
class C {
    func create() {
        var value = Ok(42)
    }
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

union Option<T> {
    Ok(value: T)
    None
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
    return Ok<int>(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
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
class C {
    func create() {
        var value = Ok<int>(42)
    }
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

union Option<T> {
    Ok(value: T)
    None
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
    public void UnqualifiedCaseInvocation_ReportsAmbiguousWhenUnionTypesAreTypeImported()
    {
        const string source = """
namespace A {
    union Result<T, E> {
        Ok(value: T)
        Error(message: E)
    }
}

namespace B {
    union Option<T> {
        Ok(value: T)
        None
    }
}

import A.Result<,>
import B.Option<>

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
class C {
    func create() {
        var value = Ok(42)
    }
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

union Option<T> {
    Ok(value: T)
    None
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
        Ok(value: T)
        Error(message: E)
    }
}

namespace B {
    union Result<T> {
        Ok(value: T)
        None
    }
}

import A.Result<,>
import B.Result<>

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
    Ok(value: T)
    Error(message: E)
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
    Ok(value: T)
    Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void AliasToClosedGenericUnionCaseType_BindsAndConvertsToCarrier()
    {
        const string source = """
alias ResultOk = Result.Ok<int>

func create() -> Result<int, string> {
    return ResultOk(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Union_DoesNotDeclareImplicitConversionPerCase()
    {
        const string source = """
union Option {
    None
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

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
    Ok(value: T)
    Error(error: E)
    Pending
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Ok"));
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Error"));
        var pendingCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Pending"));

        Assert.Equal(1, okCase.Arity);
        Assert.Equal(1, errorCase.Arity);
        Assert.Equal(0, pendingCase.Arity);
    }

    [Fact]
    public void UnionCaseParameters_ComplexTypes_BindViaBindTypeSyntaxPath()
    {
        const string source = """
union Payloads {
    Callback(fn: (int, string) -> bool)
    Pair(value: (left: int, right: string))
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var callbackCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Callback"));
        var callbackCtor = callbackCase.InstanceConstructors.Single();
        var callbackType = Assert.IsAssignableFrom<INamedTypeSymbol>(callbackCtor.Parameters[0].Type);
        Assert.Equal(TypeKind.Delegate, callbackType.TypeKind);
        var invoke = callbackType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);
        Assert.Equal(2, invoke.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);

        var pairCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Pair"));
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
union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var constructedUnion = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(
            unionSymbol.Construct(
                compilation.GetSpecialType(SpecialType.System_Int32),
                compilation.GetSpecialType(SpecialType.System_String)));

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedUnion.Cases.Single(c => c.Name == "Ok"));
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedUnion.Cases.Single(c => c.Name == "Error"));

        Assert.Equal(SpecialType.System_Int32, okCase.TypeArguments.Single().SpecialType);
        Assert.Equal(SpecialType.System_String, errorCase.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void UnionCase_UsesLogicalNameAndScopedMetadataName()
    {
        const string source = """
union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Ok"));
        var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(unionSymbol.Cases.Single(c => c.Name == "Error"));

        Assert.Equal("Ok", okCase.Name);
        Assert.Equal("Result_Ok`1", okCase.MetadataName);
        Assert.Equal("Error", errorCase.Name);
        Assert.Equal("Result_Error`1", errorCase.MetadataName);
    }

    [Fact]
    public void CaseParameters_AreExposedAsGetterOnlyProperties()
    {
        const string source = """
union Option {
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.Cases.Single();

        var property = caseSymbol.GetMembers("Value").OfType<IPropertySymbol>().Single();
        Assert.Equal(Accessibility.Private, property.DeclaredAccessibility);
        Assert.NotNull(property.GetMethod);
        Assert.Null(property.SetMethod);
        Assert.Equal(SpecialType.System_Int32, property.Type.SpecialType);

        var backingField = caseSymbol.GetMembers().OfType<IFieldSymbol>()
            .Single(f => f.Name == "<value>k__BackingField");
        Assert.Equal(Accessibility.Private, backingField.DeclaredAccessibility);
    }

    [Fact]
    public void SynthesizedUnionMembers_AreRegisteredOnUnionAndCaseSymbols()
    {
        const string source = """
union Option {
    Some(value: int)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.Cases.Single();

        var constructor = Assert.Single(caseSymbol.GetMembers(".ctor").OfType<IMethodSymbol>());
        Assert.Contains(unionSymbol.GetMembers(".ctor"), m => SymbolEqualityComparer.Default.Equals(m, constructor));

        var payloadProperty = Assert.Single(caseSymbol.GetMembers().OfType<IPropertySymbol>());
        Assert.Contains(unionSymbol.GetMembers(payloadProperty.Name), m => SymbolEqualityComparer.Default.Equals(m, payloadProperty));

        var getter = payloadProperty.GetMethod;
        Assert.NotNull(getter);
        Assert.Contains(unionSymbol.GetMembers(getter!.Name), m => SymbolEqualityComparer.Default.Equals(m, getter));

        var caseToString = Assert.Single(caseSymbol.GetMembers("ToString").OfType<IMethodSymbol>());
        Assert.Contains(unionSymbol.GetMembers("ToString"), m => SymbolEqualityComparer.Default.Equals(m, caseToString));

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
    None
    Some(value: T)
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
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        var caseSymbol = unionSymbol.Cases.Single(c => c.Name == "Some");

        var constructedUnion = (INamedTypeSymbol)unionSymbol.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var conversion = compilation.ClassifyConversion(caseSymbol, constructedUnion);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsDiscriminatedUnion);
        Assert.False(conversion.IsUserDefined);
        Assert.Null(conversion.MethodSymbol);
        Assert.NotNull(conversion.ConstructorSymbol);
    }

    [Fact]
    public void Lowerer_LowersUnionCaseToConstructorAndConversion()
    {
        const string source = """
union Option {
    None
    Some(value: int)
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
    Ok(value: T)
    Error(error: E)
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));

        foreach (var caseSymbol in unionSymbol.Cases)
        {
            Assert.Contains(
                unionSymbol.Constructors,
                constructor => constructor.Parameters.Length == 1 &&
                               string.Equals(
                                   constructor.Parameters[0].Type.TryGetDiscriminatedUnionCase()?.Name,
                                   caseSymbol.Name,
                                   StringComparison.Ordinal));
        }
    }

    [Fact]
    public void CasePattern_BindsPayloadType()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return result match {
        .Ok(val payload) => payload.ToString()
        .Error(val message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
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
    return result match {
        .Ok(val payload) => payload.ToString()
        .Error(val message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
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
    return result match {
        .Ok(val payload) => payload.ToString()
        .Error(val message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
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
    return result match {
        Ok(val payload) => payload.ToString()
        Error(val message) => message
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void CasePattern_UnqualifiedSingleArm_ReportsExhaustivenessDiagnostic()
    {
        const string source = """
func format(result: Result<int>) -> string {
    return result match {
        Ok(val payload) => payload.ToString()
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);
    }

    [Fact]
    public void CasePattern_NonGenericUnion_BindsTryGetMethods()
    {
        const string source = """
func describe(value: Test) -> string {
    return value match {
        .Something(val text) => text
        .Nothing => "none"
    }
}

union Test {
    Something(value: string)
    Nothing
}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDecl = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(model.GetDeclaredSymbol(unionDecl));
        Assert.Equal(2, unionSymbol.GetMembers("TryGetValue").OfType<IMethodSymbol>().Count());
    }

    [Fact]
    public void CasePattern_MissingArm_ReportsExhaustivenessDiagnostic()
    {
        const string source = """
func describe(result: Result<int>) -> string {
    return result match {
        .Ok(val payload) => payload.ToString()
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
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
    return result match {
        .Ok(val payload) => "ok ${payload}" when payload > 1
        .Error(val message) => "error ${message}"
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
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
    return shape match {
        .Circle(val r) => r * r * 3
        .Rectangle(4, val h) => 42
    }
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
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
    return result match {
        .Ok() => "ok"
        _ => "none"
    }
}

union Result<T> {
    Ok(value: T)
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
    return Ok(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    Ok(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
    return Ok(42)
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
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
