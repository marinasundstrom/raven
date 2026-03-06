using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionExpressionInferenceTests : CompilationTestBase
{
    [Fact]
    public void Lambda_WithoutParameterTypes_UsesTargetDelegateSignature()
    {
        const string code = """
import System.*
class Calculator {
    func Transform(value: int, projector: Func<int, int>) -> int {
        return projector(value)
    }

    func Apply() -> int {
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
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
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
    func Overloaded(pred: Func<int, bool>) -> unit { }
    func Overloaded(pred: Func<string, bool>) -> unit { }

    func Invoke() -> unit {
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
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.NotNull(boundLambda.Unbound);
    }

    [Fact]
    public void Lambda_WithCandidateMissingParameters_StillInfersTypeFromValidDelegate()
    {
        const string code = """
import System.*
class Container {
    func Overloaded(projector: Func<int, int>) -> int {
        return projector(1)
    }

    func Overloaded(callback: Action) -> int {
        callback()
        return 0
    }

    func Invoke() -> int {
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
            .OfType<SimpleFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

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
    func Provide() -> unit {
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
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<IMethodSymbol>(boundLambda.Symbol);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaSymbol.ReturnType));
    }

    [Fact]
    public void FuncLambda_WithoutReturnType_InferredFromExpressionBody()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f = func (a: int, b: int) => a + b
        f(1, 2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
    }

    [Fact]
    public void FuncLambda_WithoutReturnType_InferredFromBlockBody()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f = func (a: int, b: int) => {
            a + b
        }
        f(1, 2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
    }

    [Fact]
    public void FuncLambda_BlockBodyWithoutArrow_CapturesOuterVariable()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> int {
        val offset = 2
        val f = func (value: int) {
            value + offset
        }
        f(3)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var captures = model.GetCapturedVariables(lambdaSyntax);
        Assert.Contains(captures, symbol => symbol.Name == "offset");
    }

    [Fact]
    public void FuncLambda_AssignedToFunctionTypeSignature_Binds()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
        val f: func (int, int) -> int = func (a: int, b: int) => a + b
        f(1, 2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        Assert.Equal("func (int, int) -> int", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void Lambda_ParameterWithDefaultValue_BindsAsOptionalParameter()
    {
        const string code = """
import System.*
class Container {
    func Provide() -> unit {
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
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
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
    func Provide() -> unit {
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
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var syntaxParameter = Assert.Single(lambdaSyntax.ParameterList.Parameters);
        Assert.Single(syntaxParameter.AttributeLists);

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.Single(boundLambda.Parameters);
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_DoesNotBindAgainstOpenTypeParameter()
    {
        const string code = """
import System.Linq.*

class Container {
    func Run() -> unit {
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
    public void Lambda_InGenericLinqSelect_QualifiedUnionCases_ReportsConversionDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

public union Option<T> {
    Some(value: T)
    None
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            Option<int>.None
        } else {
            Option<int>.Some(n * 2)
        })
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_QualifiedResultCases_InfersResultType()
    {
        const string code = """
import System.*
import System.Linq.*

public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            Result<int, string>.Error("neg")
        } else {
            Result<int, string>.Ok(n * 2)
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);

        Assert.IsNotType<ITypeUnionSymbol>(elementType);

        var expectedResult = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Result`2"))
            .Construct(
                compilation.GetSpecialType(SpecialType.System_Int32),
                compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedResult, elementType),
            $"Expected: {expectedResult.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}, Actual: {elementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_QualifiedCustomUnionCases_InfersCarrierType()
    {
        const string code = """
import System.*
import System.Linq.*

public union Response<T, E> {
    Success(value: T)
    Failure(error: E)
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            Response<int, string>.Failure("neg")
        } else {
            Response<int, string>.Success(n * 2)
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);

        Assert.IsNotType<ITypeUnionSymbol>(elementType);

        var expectedResponse = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Response`2"))
            .Construct(
                compilation.GetSpecialType(SpecialType.System_Int32),
                compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedResponse, elementType),
            $"Expected: {expectedResponse.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}, Actual: {elementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_SiblingClasses_InfersCommonBaseType()
    {
        const string code = """
import System.*
import System.Linq.*

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 {
            InvalidOperationException()
        } else {
            ApplicationException()
        })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);

        var expectedException = compilation.GetTypeByMetadataName("System.Exception");
        Assert.NotNull(expectedException);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(expectedException, elementType),
            $"Expected: {expectedException!.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}, Actual: {elementType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
    }

    [Fact]
    public void Lambda_InGenericLinqSelect_ImplicitOperatorTargets_InfersBestCommonType()
    {
        const string code = """
import System.*
import System.Linq.*

class A {
    public static implicit operator(value: A) -> string { return "a" }
}

class B {
    public static implicit operator(value: B) -> string { return "b" }
}

class Container {
    func Run() -> unit {
        val arr = [1, -1, 3]
        val result = arr.Select(n => if n < 0 { A() } else { B() })
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var resultDeclarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(d => d.Identifier.Text == "result");

        var resultLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));
        var resultType = Assert.IsAssignableFrom<INamedTypeSymbol>(resultLocal.Type);
        var elementType = Assert.Single(resultType.TypeArguments);
        Assert.Equal("Object", elementType.Name);
    }

    [Fact]
    public void Lambda_InGenericLinqWhere_AllowsMemberAccessOnConcreteRecordType()
    {
        const string code = """
import System.Linq.*
import System.Collections.Generic.*

record class RatePlan(Carrier: string)

class Container {
    func HasCarrier(carrier: string) -> bool {
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
    func Provide() -> unit {
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
            .OfType<ParenthesizedFunctionExpressionSyntax>()
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

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
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
            .OfType<ParenthesizedFunctionExpressionSyntax>()
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

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        INamedTypeSymbol boundDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(boundLambda.DelegateType);
        Assert.Equal(expectedDisplay, boundDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void Lambda_WithOverloadSpecificDelegate_RebindsToSelectedMethod()
    {
        const string code = """
import System.*
class Container {
    func Overloaded(projector: Func<int, int>, value: int) -> int {
        return projector(value)
    }

    func Overloaded(projector: Func<string, string>, value: string) -> string {
        return projector(value)
    }

    func Invoke() -> int {
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

        var lambdaArgument = Assert.IsAssignableFrom<BoundFunctionExpression>(invocation.Arguments.First());
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
    func Compute() -> int {
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

val makeAdder = (x: int) -> (func int -> int) => (a: int) => x + a
""";

        var (compilation, _) = CreateCompilation(code, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void Lambda_WithUnionCaseConstructor_ReportsConversionDiagnostic()
    {
        const string code = """
import System.*

public union Option<T> {
    Some(value: T)
    None
}

class Container {
    func Map(value: int, selector: Func<int, Option<int>>) -> Option<int> {
        return selector(value)
    }

    func Test() -> Option<int> {
        return Map(1, n => if n < 0 { Option<int>.None } else { Option<int>.Some(n * 2) })
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV1503");
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

    [Fact]
    public void GenericFunction_WithCollectionLiteralAndLambda_InfersTypeParameterFromCollection()
    {
        const string source = """
import System.Collections.Generic.*

ForEach([1, 2], no => {})

func ForEach<T>(source: IEnumerable<T>, callback: func T -> ()) -> () { }
""";
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithMethodGroupArgument_InfersTypeParameterAndResolvesOverload()
    {
        const string source = """
import System.*
import System.Console.*
import System.Collections.Generic.*

val items: List<int> = [1, 2, 3]
ForEach(items, WriteLine)

func ForEach<T>(source: IEnumerable<T>, callback: func T -> ()) -> () { }
""";
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(n => n.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "ForEach");
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsGenericMethod);
        Assert.Equal(SpecialType.System_Int32, boundInvocation.Method.TypeArguments[0].SpecialType);

        var writeLineNode = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(n => n.Identifier.ValueText == "WriteLine");
        var symbolInfo = model.GetSymbolInfo(writeLineNode);
        var selectedMethod = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.Equal("WriteLine", selectedMethod.Name);
        Assert.Equal(SpecialType.System_Int32, selectedMethod.Parameters[0].Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithTypedSourceAndLambda_InfersTypeParameter()
    {
        const string source = """
import System.Collections.Generic.*

val items: IEnumerable<int> = [1, 2, 3]
ForEach(items, x => {})

func ForEach<T>(source: IEnumerable<T>, callback: func T -> ()) -> () { }
""";
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<SimpleFunctionExpressionSyntax>().Single();
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_ToDictionary_WithTwoLambdas_InfersTypeParameters()
    {
        const string source = """
import System.*
import System.Linq.*

func Main() {
    val o = [1, 2, 3].ToDictionary(x => x, y => y)
}
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, tree) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(n => n.Expression is MemberAccessExpressionSyntax m && m.Name.Identifier.Text == "ToDictionary");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(boundInvocation.Type);
        Assert.Equal("Dictionary", returnType.Name);
        Assert.Equal(2, returnType.TypeArguments.Length);
        Assert.Equal(SpecialType.System_Int32, returnType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, returnType.TypeArguments[1].SpecialType);

        var lambdas = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .ToDictionary(lambda => lambda.Parameter.Identifier.ValueText);

        var xLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdas["x"]));
        var xParam = Assert.Single(xLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, xParam.Type.SpecialType);

        var yLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdas["y"]));
        var yParam = Assert.Single(yLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, yParam.Type.SpecialType);
    }

    [Fact]
    public void GenericFunction_WithOpenDelegateInputType_ReportsInferenceDiagnostic()
    {
        const string code = """
class Container {
    static func Build<T>(projector: func T -> int) -> int {
        return 0
    }

    func Run() -> int {
        return Build(x => x)
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.LambdaParameterTypeCannotBeInferred));
    }

    [Fact]
    public void StaticFuncExpression_CapturingOuterLocal_ReportsDiagnostic()
    {
        const string code = """
class C {
    func M() -> int {
        val offset = 2
        val f = static func (x: int) {
            x + offset
        }

        return f(1)
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
                diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.StaticFunctionExpressionCannotCapture));
    }

    [Fact]
    public void ContinueWith_FunctionExpressionBody_ResolvesReceiverAndResultMember()
    {
        const string code = """
import System.Console.*
import System.Threading.Tasks.*

async func Main() -> Task {
    val f = async func (a: int, b: int) {
        await Task.FromResult(a + b)
    }

    val x = f(2, 3).ContinueWith(x => {
        x.Result
    })

    WriteLine(await f(2, 3))
}
""";

        var (compilation, tree) = CreateCompilation(code, options: new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Name is IdentifierNameSyntax { Identifier.ValueText: "Result" });

        var resultSymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetSymbolInfo(memberAccess).Symbol);
        Assert.Equal("Result", resultSymbol.Name);
        Assert.Equal(SpecialType.System_Int32, resultSymbol.Type.SpecialType);

        var receiverIdentifier = Assert.IsType<IdentifierNameSyntax>(memberAccess.Expression);
        var receiverSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(receiverIdentifier).Symbol);
        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiverSymbol.Type);
        Assert.Equal("Task", receiverType.Name);
        Assert.Equal(1, receiverType.TypeArguments.Length);
        Assert.Equal(SpecialType.System_Int32, receiverType.TypeArguments[0].SpecialType);
    }
}

public class FunctionExpressionInferenceDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void Lambda_WithErrorTypeArgument_DoesNotReportConversionError()
    {
        const string code = """
func apply(value: int, transform: func int -> int) -> int {
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
    func Map(projector: Func<string, string>) -> unit { }

    func Test() -> unit {
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
