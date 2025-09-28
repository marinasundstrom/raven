using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MetadataExtensionMethodSemanticTests : CompilationTestBase
{
    protected override MetadataReference[] GetMetadataReferences()
        => TestMetadataReferences.DefaultWithExtensionMethods;

    [Fact]
    public void MemberAccess_OnIEnumerableReceiver_UsesFixtureExtensions()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let projection = numbers.Select(func (value) => value)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Select");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        var extensionCandidate = Assert.Single(methodGroup.Methods);

        Assert.True(extensionCandidate.IsExtensionMethod);
        Assert.Equal("Select", extensionCandidate.Name);
        Assert.Equal("RavenEnumerableExtensions", extensionCandidate.ContainingType?.Name);

        var symbolInfo = model.GetSymbolInfo(memberAccess);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.True(selected.IsExtensionMethod);
        Assert.True(SymbolEqualityComparer.Default.Equals(extensionCandidate, selected));

        var receiverParameter = selected.Parameters[0];
        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiverParameter.Type);
        Assert.Equal("IEnumerable", receiverType.Name);
        Assert.Equal("Generic", receiverType.ContainingNamespace?.Name);
        Assert.Equal("Collections", receiverType.ContainingNamespace?.ContainingNamespace?.Name);
        Assert.Equal("System", receiverType.ContainingNamespace?.ContainingNamespace?.ContainingNamespace?.Name);
        var elementType = Assert.Single(receiverType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, elementType.SpecialType);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void MemberAccess_OnArrayReceiver_UsesFixtureExtensions()
    {
        const string source = """
import Raven.MetadataFixtures.Linq.*

let values: int[] = [1, 2, 3]
let enumerable = values.AsEnumerable()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "AsEnumerable");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        var extensionCandidate = Assert.Single(methodGroup.Methods);

        Assert.True(extensionCandidate.IsExtensionMethod);
        Assert.Equal("AsEnumerable", extensionCandidate.Name);
        Assert.Equal("RavenArrayExtensions", extensionCandidate.ContainingType?.Name);

        var symbolInfo = model.GetSymbolInfo(memberAccess);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.True(selected.IsExtensionMethod);
        Assert.True(SymbolEqualityComparer.Default.Equals(extensionCandidate, selected));

        var receiverParameter = selected.Parameters[0];
        var receiverType = Assert.IsAssignableFrom<IArrayTypeSymbol>(receiverParameter.Type);
        Assert.Equal(SpecialType.System_Int32, receiverType.ElementType.SpecialType);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void MemberAccess_OnNullableReceiver_UsesFixtureExtensions()
    {
        const string source = """
import Raven.MetadataFixtures.Linq.*

let value: int? = 5
let isPresent = value.IsPresent()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "IsPresent");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        var extensionCandidate = Assert.Single(methodGroup.Methods);

        Assert.True(extensionCandidate.IsExtensionMethod);
        Assert.Equal("IsPresent", extensionCandidate.Name);
        Assert.Equal("RavenNullableExtensions", extensionCandidate.ContainingType?.Name);

        var symbolInfo = model.GetSymbolInfo(memberAccess);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.True(selected.IsExtensionMethod);
        Assert.True(SymbolEqualityComparer.Default.Equals(extensionCandidate, selected));

        var receiverParameter = selected.Parameters[0];
        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiverParameter.Type);
        Assert.Equal(SpecialType.System_Nullable_T, receiverType.SpecialType);
        var elementType = Assert.Single(receiverType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, elementType.SpecialType);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_WithLambdaArgument_ResolvesPredicateOverload()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let anyPositive = numbers.Any(func (value) => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Any");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        Assert.Equal(2, methodGroup.Methods.Length);
        Assert.All(methodGroup.Methods, method => Assert.True(method.IsExtensionMethod));
        Assert.Contains(methodGroup.Methods, method => method.Parameters.Length == 2);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal(1, boundInvocation.Arguments.Count());

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Any", selected.Name);
        Assert.Equal(2, selected.Parameters.Length);
        Assert.Single(selected.TypeArguments, type => type.SpecialType == SpecialType.System_Int32);

        var predicateParameter = selected.Parameters[1];
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicateParameter.Type);
        Assert.Equal("Func", predicateType.Name);
        Assert.Equal(2, predicateType.Arity);
        Assert.Collection(
            predicateType.TypeArguments,
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_Boolean, argument.SpecialType));

        var convertedArgument = Assert.Single(boundInvocation.Arguments);
        var cast = Assert.IsType<BoundCastExpression>(convertedArgument);
        Assert.Equal(predicateType, cast.Type);

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_WithoutLambdaArgument_ResolvesParameterlessOverload()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let anyItems = numbers.Any()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Any");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        Assert.Equal(2, methodGroup.Methods.Length);
        Assert.Contains(methodGroup.Methods, method => method.Parameters.Length == 1);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Empty(boundInvocation.Arguments);

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Any", selected.Name);
        Assert.Equal(1, selected.Parameters.Length);
        Assert.Single(selected.TypeArguments, type => type.SpecialType == SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_WithProjectionLambda_InfersSourceAndResultTypes()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let projection = numbers.Select(func (value) => value.ToString())
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Select");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        Assert.Single(methodGroup.Methods);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.NotNull(boundInvocation.ExtensionReceiver);

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Select", selected.Name);
        Assert.Equal(2, selected.TypeArguments.Length);
        Assert.Equal(SpecialType.System_Int32, selected.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_String, selected.TypeArguments[1].SpecialType);

        var projectionParameter = selected.Parameters[1];
        var delegateType = Assert.IsAssignableFrom<INamedTypeSymbol>(projectionParameter.Type);
        Assert.Equal("Func", delegateType.Name);
        Assert.Equal(2, delegateType.Arity);

        var convertedArgument = Assert.Single(boundInvocation.Arguments);
        var cast = Assert.IsType<BoundCastExpression>(convertedArgument);
        Assert.Equal(delegateType, cast.Type);

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_String, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_WithWhereLambda_ResolvesSingleParameterOverload()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let positives = numbers.Where(func (value) => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Where");

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Where", selected.Name);
        Assert.Equal(2, selected.Parameters.Length);

        var predicateParameter = selected.Parameters[1];
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicateParameter.Type);
        Assert.Equal("Func", predicateType.Name);
        Assert.Equal(2, predicateType.Arity);
        Assert.Collection(
            predicateType.TypeArguments,
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_Boolean, argument.SpecialType));

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_SystemLinqWhereWithLambda_Binds()
    {
        const string source = """
import System.*
import System.Linq.*

let numbers: int[] = [1, 2, 3]
let result = numbers.Where(func (value) => value == 2)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Where");

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Where", selected.Name);
        Assert.Equal("Enumerable", selected.ContainingType?.Name);
        Assert.Equal("Linq", selected.ContainingNamespace?.Name);
        Assert.Equal("System", selected.ContainingNamespace?.ContainingNamespace?.Name);

        var predicateParameter = selected.Parameters[1];
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicateParameter.Type);
        Assert.Equal("Func", predicateType.Name);
        Assert.Equal(2, predicateType.Arity);
        Assert.Collection(
            predicateType.TypeArguments,
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_Boolean, argument.SpecialType));

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_WithImplicitLambda_CapturesExtensionDelegateCandidates()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let positives = numbers.Where(value => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Where");

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Equal("Where", boundInvocation.Method.Name);
        Assert.Equal("RavenEnumerableExtensions", boundInvocation.Method.ContainingType?.Name);

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));

        Assert.False(boundLambda.CandidateDelegates.IsDefaultOrEmpty);
        var unbound = Assert.IsType<BoundUnboundLambda>(boundLambda.Unbound);
        Assert.False(unbound.CandidateDelegates.IsDefaultOrEmpty);
        Assert.Empty(unbound.SuppressedDiagnostics);
        Assert.Contains(
            boundLambda.CandidateDelegates,
            candidate => candidate.Name == "Func" &&
                candidate.Arity == 2 &&
                candidate.TypeArguments[0] is { SpecialType: SpecialType.System_Int32 } &&
                candidate.TypeArguments[1] is { SpecialType: SpecialType.System_Boolean });

        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
    }

    [Fact]
    public void Invocation_WithIndexedWhereLambda_ResolvesIndexedOverload()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

let numbers = List<int>()
let positives = numbers.Where(func (value, index) => value > index)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Where");

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        var symbolInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Where", selected.Name);
        Assert.Equal(2, selected.Parameters.Length);

        var predicateParameter = selected.Parameters[1];
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicateParameter.Type);
        Assert.Equal("Func", predicateType.Name);
        Assert.Equal(3, predicateType.Arity);
        Assert.Collection(
            predicateType.TypeArguments,
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_Boolean, argument.SpecialType));

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.Equal(2, boundLambda.Parameters.Count());
        Assert.All(boundLambda.Parameters.Take(2), parameter => Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType));
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    private static MemberAccessExpressionSyntax GetMemberAccess(SyntaxTree tree, string methodName)
    {
        return tree
            .GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.Text == methodName);
    }
}
