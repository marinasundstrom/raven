using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class LambdaInferenceTests : CompilationTestBase
{
    [Fact]
    public void Lambda_WithoutParameterTypes_UsesTargetDelegateSignature()
    {
        const string code = """
import System.*
class Calculator {
    Transform(value: int, projector: Func<int, int>) -> int {
        return projector(value)
    }

    Apply() -> int {
        return Transform(5, delta => delta + 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.False(boundLambda.CandidateDelegates.IsDefaultOrEmpty);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, parameter.Type),
            parameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.NotNull(boundLambda.Body.Type);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, boundLambda.Body.Type),
            $"{boundLambda.Body.GetType().FullName} - {boundLambda.Body.Type?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType),
            boundLambda.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.True(
            SymbolEqualityComparer.Default.Equals(
                compilation.GetTypeByMetadataName("System.Func`2")?.Construct(intType, intType),
                boundLambda.DelegateType),
            boundLambda.DelegateType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var transformSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .First(f => f.Identifier.Text == "Transform");
        var transformSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(transformSyntax));
        var projectorParameter = Assert.Single(transformSymbol.Parameters, p => p.Name == "projector");
        Assert.True(SymbolEqualityComparer.Default.Equals(projectorParameter.Type, boundLambda.DelegateType));
    }

    [Fact]
    public void Lambda_WithConflictingDelegateCandidates_SuppressesParameterInferenceDiagnostic()
    {
        const string code = """
import System.*
class Container {
    Overloaded(pred: Func<int, bool>) -> unit { }
    Overloaded(pred: Func<string, bool>) -> unit { }

    Invoke() -> unit {
        Overloaded(value => true)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.LambdaParameterTypeCannotBeInferred));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.NotNull(boundLambda.Unbound);
    }

    [Fact]
    public void Lambda_WithCandidateMissingParameters_StillInfersTypeFromValidDelegate()
    {
        const string code = """
import System.*
class Container {
    Overloaded(projector: Func<int, int>) -> int {
        return projector(1)
    }

    Overloaded(callback: Action) -> int {
        callback()
        return 0
    }

    Invoke() -> int {
        return Overloaded(value => value + 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type));
    }

    [Fact]
    public void Lambda_WithoutReturnType_InferredFromBody()
    {
        const string code = """
import System.*
class Container {
    Provide() -> unit {
        val lambda = (value: int) => value + 1
        lambda(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<IMethodSymbol>(boundLambda.Symbol);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaSymbol.ReturnType));
    }

    [Fact]
    public void Lambda_ParameterWithDefaultValue_BindsAsOptionalParameter()
    {
        const string code = """
import System.*
class Container {
    Provide() -> unit {
        val f = (name: string, age: int = 1) => age
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var parameters = boundLambda.Parameters.ToArray();
        Assert.Equal(2, parameters.Length);
        Assert.False(parameters[0].HasExplicitDefaultValue);
        Assert.True(parameters[1].HasExplicitDefaultValue);
        Assert.Equal(1, parameters[1].ExplicitDefaultValue);
    }

    [Fact]
    public void Lambda_LeadingAttribute_BindsToSingleParameter()
    {
        const string code = """
import System.*

class Container {
    Provide() -> unit {
        val f = [Obsolete](content: string) => content
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var syntaxParameter = Assert.Single(lambdaSyntax.ParameterList.Parameters);
        Assert.Single(syntaxParameter.AttributeLists);

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.Single(boundLambda.Parameters);
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_DoesNotBindAgainstOpenTypeParameter()
    {
        const string code = """
import System.Linq.*

class Container {
    Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 { 0 } else { n * 2 })
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_InGenericLinqWhere_AllowsMemberAccessOnConcreteRecordType()
    {
        const string code = """
import System.Linq.*
import System.Collections.Generic.*

record class RatePlan(Carrier: string)

class Container {
    HasCarrier(carrier: string) -> bool {
        val plans = List<RatePlan> { RatePlan("NorthStar") }
        val selected = plans.Where(p => p.Carrier == carrier)
        return selected.Any()
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_LocalDeclaration_InfersFuncDelegate()
    {
        const string code = """
import System.*
class Container {
    Provide() -> unit {
        val projector = (value: int) => value + 2
        val result = projector(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(d => d.Identifier.Text == "projector");
        var lambdaSyntax = declarator
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var funcDefinition = compilation.GetTypeByMetadataName("System.Func`2");
        Assert.NotNull(funcDefinition);
        INamedTypeSymbol expectedDelegateDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition);
        INamedTypeSymbol expectedDelegate = (INamedTypeSymbol)expectedDelegateDefinition.Construct(intType, intType);

        INamedTypeSymbol actualDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(localSymbol.Type);
        var actualArgs = string.Join(", ", actualDelegate.TypeArguments.Select(arg => arg.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)));
        var expectedArgs = string.Join(", ", expectedDelegate.TypeArguments.Select(arg => arg.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)));
        var actualDisplay = actualDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var expectedDisplay = expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

        Assert.Equal(expectedDisplay, actualDisplay);
        Assert.Equal(expectedArgs, actualArgs);

        var lambdaTypeInfo = model.GetTypeInfo(lambdaSyntax);
        INamedTypeSymbol lambdaType = Assert.IsAssignableFrom<INamedTypeSymbol>(lambdaTypeInfo.Type);
        Assert.Equal(expectedDisplay, lambdaType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        INamedTypeSymbol boundDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.DelegateType);
        Assert.Equal(expectedDisplay, boundDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void Lambda_GlobalDeclaration_UsesFuncDelegate()
    {
        const string code = """
import System.*

val projector = (value: int) => value + 2
val result = projector(1)
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(d => d.Identifier.Text == "projector");
        var lambdaSyntax = declarator
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var funcDefinition = compilation.GetTypeByMetadataName("System.Func`2");
        Assert.NotNull(funcDefinition);
        INamedTypeSymbol expectedDelegateDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition);
        INamedTypeSymbol expectedDelegate = (INamedTypeSymbol)expectedDelegateDefinition.Construct(intType, intType);

        INamedTypeSymbol actualDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(localSymbol.Type);
        var actualDisplay = actualDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var expectedDisplay = expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal(expectedDisplay, actualDisplay);

        var lambdaTypeInfo = model.GetTypeInfo(lambdaSyntax);
        INamedTypeSymbol lambdaType = Assert.IsAssignableFrom<INamedTypeSymbol>(lambdaTypeInfo.Type);
        Assert.Equal(expectedDisplay, lambdaType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        INamedTypeSymbol boundDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.DelegateType);
        Assert.Equal(expectedDisplay, boundDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void Lambda_WithOverloadSpecificDelegate_RebindsToSelectedMethod()
    {
        const string code = """
import System.*
class Container {
    Overloaded(projector: Func<int, int>, value: int) -> int {
        return projector(value)
    }

    Overloaded(projector: Func<string, string>, value: string) -> string {
        return projector(value)
    }

    Invoke() -> int {
        return Overloaded(value => value + 1, 5)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.ValueText: "Overloaded" });

        var invocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        var method = Assert.IsAssignableFrom<IMethodSymbol>(invocation.Method);

        Assert.Equal("Overloaded", method.Name);

        var funcDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Func`2"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expectedDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition.Construct(intType, intType));

        var projectorParameter = Assert.Single(method.Parameters, p => p.Name == "projector");
        Assert.Equal(
            expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            projectorParameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var lambdaArgument = Assert.IsAssignableFrom<BoundLambdaExpression>(invocation.Arguments.First());
        Assert.NotNull(lambdaArgument.Unbound);
        Assert.False(lambdaArgument.CandidateDelegates.IsDefaultOrEmpty);
        var lambdaParameter = Assert.Single(lambdaArgument.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaParameter.Type));
    }

    [Fact]
    public void Lambda_AssignedToLocal_InvocationBindsToDelegateInvoke()
    {
        const string code = """
import System.*
class Calculator {
    Compute() -> int {
        val add = (left: int, right: int) -> int => left + right
        return add(2, 3)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("Invoke", boundInvocation.Method.Name);

        Assert.NotNull(boundInvocation.Receiver);
        var receiver = boundInvocation.Receiver!;
        var delegateType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiver.Type);
        Assert.Equal(TypeKind.Delegate, delegateType.TypeKind);
        Assert.Same(delegateType.GetDelegateInvokeMethod(), boundInvocation.Method);
    }

    [Fact]
    public void Lambda_ReturningLambda_WithExplicitReturnType_ComposesSuccessfully()
    {
        const string code = """
import System.*

val makeAdder = (x: int) -> (int -> int) => (a: int) => x + a
""";

        var (compilation, _) = CreateCompilation(code, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_WithUnionCaseConstructor_BindsWithoutErrors()
    {
        const string code = """
import System.*

public union Option<T> {
    Some(value: T)
    None
}

class Container {
    Map(value: int, selector: Func<int, Option<int>>) -> Option<int> {
        return selector(value)
    }

    Test() -> Option<int> {
        return Map(1, n => if n < 0 { Option<int>.None } else { Option<int>.Some(n * 2) })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaBody = boundLambda.Body;
        if (lambdaBody is BoundConversionExpression lambdaConversion)
            lambdaBody = lambdaConversion.Expression;

        var ifExpression = Assert.IsType<BoundIfExpression>(lambdaBody);
        var elseBranch = ifExpression.ElseBranch;
        if (elseBranch is BoundConversionExpression conversion)
            elseBranch = conversion.Expression;

        var elseBlock = Assert.IsType<BoundBlockExpression>(elseBranch);
        var elseExpression = elseBlock.Statements
            .OfType<BoundExpressionStatement>()
            .Single()
            .Expression;

        var expectedReturn = boundLambda.ReturnType;
        Assert.NotNull(expectedReturn);
        var ifType = ifExpression.Type;
        Assert.NotNull(ifType);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedReturn, ifType) ||
            compilation.ClassifyConversion(ifType!, expectedReturn!).Exists);

        var thenType = ifExpression.ThenBranch.Type;
        Assert.NotNull(thenType);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedReturn, thenType) ||
            compilation.ClassifyConversion(thenType!, expectedReturn!).Exists);

        var elseType = ifExpression.ElseBranch?.Type;
        Assert.NotNull(elseType);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedReturn, elseType) ||
            compilation.ClassifyConversion(elseType!, expectedReturn!).Exists);

        Assert.False(elseExpression is BoundErrorExpression, elseExpression.ToString());
    }

    [Fact]
    public void MetadataDelegate_PreservesDelegateTypeKind_WhenConstructed()
    {
        const string code = "class Container { }";

        var (compilation, _) = CreateCompilation(code);

        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Func`2"));
        Assert.Equal(TypeKind.Delegate, definition.TypeKind);

        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(
            compilation.GetSpecialType(SpecialType.System_Int32),
            compilation.GetSpecialType(SpecialType.System_Int32)));
        Assert.Equal(TypeKind.Delegate, constructed.TypeKind);
    }
}

public class LambdaInferenceDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void Lambda_WithErrorTypeArgument_DoesNotReportConversionError()
    {
        const string code = """
func apply(value: int, transform: int -> int) -> int {
    transform(value)
}

val doubled = x => x * 2

val result = apply(5, doubled)
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2200").WithSpan(5, 15, 5, 16).WithArguments("x"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Lambda_WithBodyError_ReportsBodyDiagnostics()
    {
        const string code = """
import System.*

class Container {
    Map(projector: Func<string, string>) -> unit { }

    Test() -> unit {
        Map((name: string) => missingValue)
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("missingValue"),
            ]);

        verifier.Verify();
    }
}
