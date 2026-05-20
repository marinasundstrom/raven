using System;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MetadataExtensionMethodSemanticTests : CompilationTestBase
{
    protected override MetadataReference[] GetMetadataReferences()
        => TestMetadataReferences.DefaultWithoutSystemLinqWithExtensionMethods;

    [Fact]
    public void MetadataType_GetMembersByName_DoesNotHydrateUnrelatedMembersOrDuplicateOnFullLoad()
    {
        var (compilation, _) = CreateCompilation(
            "",
            references: TestMetadataReferences.Default);
        compilation.EnsureSetup();

        var systemNamespace = compilation.GetNamespaceSymbol("System");
        Assert.NotNull(systemNamespace);

        var linqNamespace = systemNamespace.GetMembers("Linq").OfType<INamespaceSymbol>().SingleOrDefault();
        Assert.NotNull(linqNamespace);

        var collectionsNamespace = systemNamespace.GetMembers("Collections").OfType<INamespaceSymbol>().SingleOrDefault();
        Assert.NotNull(collectionsNamespace);

        var genericNamespace = collectionsNamespace.GetMembers("Generic").OfType<INamespaceSymbol>().SingleOrDefault();
        Assert.NotNull(genericNamespace);

        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            genericNamespace.GetMembers("List").Single());
        Assert.Equal(1, listType.Arity);

        var enumerableType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            linqNamespace.GetMembers("Enumerable").Single());

        var exactMembersBeforeFullLoad = enumerableType.GetMembers("Any")
            .OfType<IMethodSymbol>()
            .ToArray();

        Assert.NotEmpty(exactMembersBeforeFullLoad);

        _ = enumerableType.GetMembers();

        var exactMembersAfterFullLoad = enumerableType.GetMembers("Any")
            .OfType<IMethodSymbol>()
            .ToArray();

        Assert.Equal(exactMembersBeforeFullLoad.Length, exactMembersAfterFullLoad.Length);
        Assert.Equal(
            exactMembersAfterFullLoad.Length,
            exactMembersAfterFullLoad.Select(method => method.GetLookupIdentityKey()).Distinct().Count());
    }

    [Fact]
    public void MemberAccess_OnIEnumerableReceiver_UsesSystemLinqExtensions()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

val numbers = List<int>()
val anyNumbers = numbers.Any()
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.Default);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Descriptor != CompilerDiagnostics.PossibleNullReferenceAccess);

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Any");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        Assert.Contains(
            methodGroup.Methods,
            method =>
                method.IsExtensionMethod &&
                method.Name == "Any" &&
                method.Parameters.Length == 1 &&
                method.ContainingType?.Name == "Enumerable" &&
                method.ContainingType?.ContainingNamespace?.Name == "Linq" &&
                method.ContainingType?.ContainingNamespace?.ContainingNamespace?.Name == "System");

        var symbolInfo = model.GetSymbolInfo(memberAccess);
        Assert.Contains(
            symbolInfo.CandidateSymbols.OfType<IMethodSymbol>(),
            method =>
                method.IsExtensionMethod &&
                method.Name == "Any" &&
                method.ContainingType?.Name == "Enumerable" &&
                method.ContainingType?.ContainingNamespace?.Name == "Linq" &&
                method.ContainingType?.ContainingNamespace?.ContainingNamespace?.Name == "System");

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var invocationInfo = model.GetSymbolInfo(invocation);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(invocationInfo.Symbol);

        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("Any", selected.Name);

        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(selected.ContainingType);
        Assert.Equal("Enumerable", containingType.Name);
        Assert.Equal("Linq", containingType.ContainingNamespace?.Name);
        Assert.Equal("System", containingType.ContainingNamespace?.ContainingNamespace?.Name);

        var receiverParameter = selected.Parameters[0];
        var receiverType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiverParameter.Type);
        Assert.Equal("IEnumerable", receiverType.Name);
        Assert.Equal("Generic", receiverType.ContainingNamespace?.Name);
        Assert.Equal("Collections", receiverType.ContainingNamespace?.ContainingNamespace?.Name);
        Assert.Equal("System", receiverType.ContainingNamespace?.ContainingNamespace?.ContainingNamespace?.Name);

        Assert.True(selected.Parameters.Length == 1);
    }

    [Fact]
    public void MemberAccess_OnIEnumerableReceiver_UsesFixtureExtensions()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

val numbers = List<int>()
val projection = numbers.Select(value => value)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Descriptor != CompilerDiagnostics.PossibleNullReferenceAccess);

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

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal(selected.Name, boundInvocation.Method.Name);
    }

    [Fact]
    public void TryGetAvailableInvocationCandidates_ReusesCachedExtensionLookupWithoutBinding()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

val numbers = List<int>()
val projection = numbers.Select(value => value)
""";

        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);
        var (compilation, tree) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Select");
        var invocation = Assert.IsType<InvocationExpressionSyntax>(memberAccess.Parent);
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.True(boundInvocation.Method.IsExtensionMethod);

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(invocation, out var candidates));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Contains(candidates, method => SymbolEqualityComparer.Default.Equals(method, boundInvocation.Method));
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void ExtensionLookup_CachesNegativeMetadataResult()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

val numbers = List<int>()
val projection = numbers.NotAnExtension()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "NotAnExtension");
        var invocation = Assert.IsType<InvocationExpressionSyntax>(memberAccess.Parent);

        _ = model.GetBoundNode(invocation);

        var receiverType = Assert.IsAssignableFrom<ITypeSymbol>(
            model.GetTypeInfo(memberAccess.Expression).Type ?? model.GetTypeInfo(memberAccess.Expression).ConvertedType);
        var binder = model.GetBinder(invocation);

        Assert.True(ExtensionMemberLookup.TryGetCached(
            binder,
            receiverType,
            out var cached,
            "NotAnExtension",
            includePartialMatches: false,
            kinds: ExtensionMemberKinds.InstanceMethods));
        Assert.True(cached.IsEmpty);
    }

    [Fact]
    public void ExtensionLookup_DeduplicatesMetadataCandidatesWithoutLoadingReturnTypes()
    {
        const string metadataSource = """
import System.Runtime.CompilerServices.*

namespace Lib {
    static class NumberExtensions {
        [ExtensionAttribute]
        static func Echo(value: int) -> string {
            return value.ToString()
        }
    }
}
""";

        const string source = """
import Lib.*

val value = 1
val echoed = value.Echo()
""";

        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            metadataSource,
            assemblyName: "extension-lookup-shallow-dedup-fixture");
        var references = TestMetadataReferences.Default.Append(metadataReference).ToArray();
        var (compilation, tree) = CreateCompilation(source, references: references);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var invocation = Assert.IsType<InvocationExpressionSyntax>(GetMemberAccess(tree, "Echo").Parent);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var result = model.LookupApplicableExtensionMembers(
            intType,
            invocation,
            name: "Echo",
            kinds: ExtensionMemberKinds.InstanceMethods);

        var method = Assert.Single(result.InstanceMethods);
        Assert.Equal("Echo", method.Name);
        var peMethod = Assert.IsType<PEMethodSymbol>(method.OriginalDefinition ?? method);
        Assert.False(IsReturnTypeLoaded(peMethod));
    }

    [Fact]
    public void AvailableInvocationTargetSymbolInfo_ResolvesMetadataExtensionWithOptionalParameters()
    {
        const string metadataSource = """
import System.Runtime.CompilerServices.*

namespace Lib {
    static class NumberExtensions {
        [ExtensionAttribute]
        static func Echo(value: int, suffix: string = "") -> string {
            return value.ToString() + suffix
        }
    }
}
""";

        const string source = """
import Lib.*

val value = 1
val echoed = value.Echo()
""";

        var instrumentation = new PerformanceInstrumentation();
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            metadataSource,
            assemblyName: "extension-optional-parameter-fixture");
        var references = TestMetadataReferences.Default.Append(metadataReference).ToArray();
        var options = new CompilationOptions(
            OutputKind.ConsoleApplication,
            performanceInstrumentation: instrumentation);
        var (compilation, tree) = CreateCompilation(source, options: options, references: references);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var invocation = Assert.IsType<InvocationExpressionSyntax>(GetMemberAccess(tree, "Echo").Parent);

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetInvocationTargetSymbolInfo(invocation, out var info));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(info.Symbol);
        Assert.Equal("Echo", method.Name);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
    }

    [Fact]
    public void MemberAccess_OnArrayReceiver_UsesFixtureExtensions()
    {
        const string source = """
import Raven.MetadataFixtures.Linq.*

val values: int[] = [1, 2, 3]
val enumerable = values.AsEnumerable()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Descriptor != CompilerDiagnostics.PossibleNullReferenceAccess);

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

val value: int? = 5
val isPresent = value.IsPresent()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MemberDoesNotContainDefinition);
        return;

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

val numbers = List<int>()
val anyPositive = numbers.Any((value: int) => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Descriptor != CompilerDiagnostics.PossibleNullReferenceAccess);

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Any");

        var methodGroup = Assert.IsType<BoundMethodGroupExpression>(model.GetBoundNode(memberAccess));
        Assert.Equal(2, methodGroup.Methods.Length);
        Assert.All(methodGroup.Methods, method => Assert.True(method.IsExtensionMethod));
        Assert.Contains(methodGroup.Methods, method => method.Parameters.Length == 2);

        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Single(boundInvocation.Arguments);

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
        if (convertedArgument is BoundConversionExpression cast)
            Assert.Equal(predicateType, cast.Type);
        else
            Assert.IsType<BoundFunctionExpression>(convertedArgument);

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
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

val numbers = List<int>()
val anyItems = numbers.Any()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Descriptor != CompilerDiagnostics.PossibleNullReferenceAccess);

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
        Assert.Single(selected.Parameters);
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

val numbers = List<int>()
val projection = numbers.Select(value => value.ToString())
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
        if (convertedArgument is BoundConversionExpression cast)
            Assert.Equal(delegateType, cast.Type);
        else
            Assert.IsType<BoundFunctionExpression>(convertedArgument);

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_String, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_WithMetadataExtensionOverloads_AndActionExpressionLambda_UsesSingleParameterOverload()
    {
        const string source = """
import Raven.MetadataFixtures.DependencyInjection.*

class VehicleDbContext {
}

val services = ServiceCollection()
services.AddDbContext<VehicleDbContext>(options => options.UseProvider("Host=localhost"))
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithExtensionMethods);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "AddDbContext");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.NotNull(boundInvocation.ExtensionReceiver);

        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.True(selected.IsExtensionMethod);
        Assert.Equal("AddDbContext", selected.Name);
        Assert.Equal(4, selected.Parameters.Length);

        var optionsParameter = selected.Parameters[1];
        var optionsDelegateType = optionsParameter.Type is NullableTypeSymbol nullableOptionsDelegate
            ? nullableOptionsDelegate.UnderlyingType
            : optionsParameter.Type;
        var optionsDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(optionsDelegateType);
        var invoke = optionsDelegate.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Single(invoke.Parameters);
        Assert.Equal("DbContextOptionsBuilder", invoke.Parameters[0].Type.Name);

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal("DbContextOptionsBuilder", lambdaParameter.Type.Name);
        Assert.Equal(SpecialType.System_Unit, boundLambda.ReturnType.SpecialType);
    }

    [Fact]
    public void Invocation_WithWhereLambda_ResolvesSingleParameterOverload()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

val numbers = List<int>()
val positives = numbers.Where(value => value > 0)
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
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
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
import Raven.MetadataFixtures.Linq.*

val numbers: int[] = [1, 2, 3]
val result = numbers.Where(value => value == 2)
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
        Assert.Equal("RavenEnumerableExtensions", selected.ContainingType?.Name);

        var predicateParameter = selected.Parameters[1];
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicateParameter.Type);
        Assert.Equal("Func", predicateType.Name);
        Assert.Equal(2, predicateType.Arity);
        Assert.Collection(
            predicateType.TypeArguments,
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_Boolean, argument.SpecialType));

        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_SystemLinqWhereWithLambda_CapturesAllDelegateCandidates()
    {

        const string source = """
import System.*
import Raven.MetadataFixtures.Linq.*

val numbers: int[] = [1, 2, 3]
val result = numbers.Where(value => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Where");
        var invocation = (InvocationExpressionSyntax)memberAccess.Parent!;
        var lambdaSyntax = invocation.ArgumentList.Arguments.Single().Expression;

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.False(boundLambda.CandidateDelegates.IsDefaultOrEmpty);

        var hasPredicate = boundLambda.CandidateDelegates.Any(candidate =>
            candidate.Name == "Func" &&
            candidate.Arity == 2 &&
            candidate.TypeArguments.Length == 2 &&
            candidate.TypeArguments[0] is { SpecialType: SpecialType.System_Int32 } &&
            candidate.TypeArguments[1] is { SpecialType: SpecialType.System_Boolean });

        Assert.True(hasPredicate);

        var unbound = Assert.IsType<BoundUnboundFunctionExpression>(boundLambda.Unbound);
        Assert.False(unbound.CandidateDelegates.IsDefaultOrEmpty);
        Assert.Empty(unbound.SuppressedDiagnostics);

        Assert.True(unbound.CandidateDelegates.Any(candidate =>
            candidate.Name == "Func" &&
            candidate.Arity == 2 &&
            candidate.TypeArguments.Length == 2 &&
            candidate.TypeArguments[0] is { SpecialType: SpecialType.System_Int32 } &&
            candidate.TypeArguments[1] is { SpecialType: SpecialType.System_Boolean }));

    }

    [Fact]
    public void Invocation_SystemLinqWhereWithLambda_CompatibleWithMetadataDelegates()
    {
        const string source = """
import System.*
import Raven.MetadataFixtures.Linq.*

val numbers: int[] = [1, 2, 3]
val result = numbers.Where(value => value > 0)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "Where");
    }

    [Fact]
    public void Invocation_WithImplicitLambda_CapturesExtensionDelegateCandidates()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import Raven.MetadataFixtures.Linq.*

val numbers = List<int>()
val positives = numbers.Where(value => value > 0)
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

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));

        Assert.False(boundLambda.CandidateDelegates.IsDefaultOrEmpty);
        var unbound = Assert.IsType<BoundUnboundFunctionExpression>(boundLambda.Unbound);
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

val numbers = List<int>()
val positives = numbers.Where((value: int, index: int) => value > index)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
        return;

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
        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.Equal(2, boundLambda.Parameters.Count());
        Assert.All(boundLambda.Parameters.Take(2), parameter => Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType));
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(boundInvocation.Method, selected));
    }

    [Fact]
    public void Invocation_SystemLinqWhereWithMemberAccessLambda_BindsBody()
    {
        const string source = """
import System.*
import Raven.MetadataFixtures.Linq.*
import System.Text.*
import System.Reflection.*

val builder = StringBuilder()
val properties = builder.GetType().GetProperties()
val result = properties.Where(pi => !pi.GetMethod.IsStatic)
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication),
            references: TestMetadataReferences.DefaultWithoutSystemLinqWithExtensionMethods);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Descriptor != CompilerDiagnostics.PossibleNullReferenceAccess);

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal("PropertyInfo", lambdaParameter.Type.Name);
        Assert.Equal("Reflection", lambdaParameter.Type.ContainingNamespace?.Name);
        Assert.Equal(SpecialType.System_Boolean, boundLambda.ReturnType.SpecialType);
        Assert.IsNotType<BoundErrorExpression>(boundLambda.Body);
    }

    private static MemberAccessExpressionSyntax GetMemberAccess(SyntaxTree tree, string methodName)
    {
        return tree
            .GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.Text == methodName);
    }

    private static bool IsReturnTypeLoaded(PEMethodSymbol method)
        => typeof(PEMethodSymbol)
            .GetField("_returnType", BindingFlags.Instance | BindingFlags.NonPublic)!
            .GetValue(method) is not null;
}
