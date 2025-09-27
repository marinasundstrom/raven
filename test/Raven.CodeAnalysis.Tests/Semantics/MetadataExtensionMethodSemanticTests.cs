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

    private static MemberAccessExpressionSyntax GetMemberAccess(SyntaxTree tree, string methodName)
    {
        return tree
            .GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.Text == methodName);
    }
}
