using System.Linq;

using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NullableFlowAttributeBindingTests : CompilationTestBase
{
    [Fact]
    public void NotNullWhenTrue_NarrowsEveryAnnotatedArgumentInTrueBranch()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length(first: string?, second: string?) -> int {
    if NullableFlowFixture.ArePresent(first, second) {
        return first.Length + second.Length
    }

    return 0
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NotNullWhenFalse_NarrowsAnnotatedArgumentInFalseBranch()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length(value: string?) -> int {
    if NullableFlowFixture.IsMissing(value) {
        return 0
    }

    return value.Length
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NotNullWhen_DoesNotNarrowTheOppositeResultBranch()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length(value: string?) -> int {
    if NullableFlowFixture.IsMissing(value) {
        return value.Length
    }

    return 0
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Single(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MaybeNullWhenTrue_InvalidatesArgumentInTrueBranch(bool diagnosticsFirst)
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int {
    var value: string? = "raven"
    if value is null {
        return 0
    }

    if NullableFlowFixture.MaybeClear(true, ref value) {
        return value.Length
    }

    return 0
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(memberAccess => memberAccess.Name.Identifier.ValueText == "Length")
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MaybeNullWhenTrue_PreservesArgumentInFalseBranch(bool diagnosticsFirst)
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int {
    var value: string? = "raven"
    if value is null {
        return 0
    }

    if NullableFlowFixture.MaybeClear(false, ref value) {
        return 0
    }

    return value.Length
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(memberAccess => memberAccess.Name.Identifier.ValueText == "Length")
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void AllowNullParameter_AcceptsNullInputWithoutChangingDeclaredType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.AcceptNull(null)
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);

        Assert.False(method.Parameters[0].Type.IsNullable);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void DisallowNullParameter_RejectsNullInputWithoutChangingDeclaredType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.RejectNull(null)
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);

        Assert.True(method.Parameters[0].Type.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void NonNullableParameter_RejectsNullInput()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.RejectOrdinaryNull(null)
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void NonNullableParameter_RejectsMaybeNullInput()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Pass(value: string?) -> unit {
    NullableFlowFixture.RejectOrdinaryNull(value)
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NonNullableParameter_AcceptsFlowNarrowedInput(bool diagnosticsFirst)
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Pass(value: string?) -> unit {
    if value is null {
        return
    }

    NullableFlowFixture.RejectOrdinaryNull(value)
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData("RequiredName", false)]
    [InlineData("OptionalName", true)]
    [InlineData("OrdinaryName", true)]
    public void PropertyInputNullabilityContract_ControlsNullAssignment(
        string propertyName,
        bool expectDiagnostic)
    {
        var source = $$"""
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.{{propertyName}} = null
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal(
            expectDiagnostic,
            diagnostics.Any(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType));
    }

    [Fact]
    public void MaybeNullReturn_WarnsOnDirectDereference()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int => NullableFlowFixture.FindName().Length
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Single(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void MaybeNullReturn_InfersNullableLocalFlow()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int {
    let value = NullableFlowFixture.FindName()
    return value.Length
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Single(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void MaybeNullReturn_RejectsExplicitNonNullableLocalType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value: string = NullableFlowFixture.FindName()
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Single(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void MaybeNullReturn_PreservesDeclaredAnnotationAndReportsMaybeNullFlow()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.FindName()
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(invocation);

        Assert.Equal(NullableAnnotation.NotAnnotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
    }

    [Fact]
    public void MaybeNullReturn_UnconstrainedGenericReflectsConstructedReferenceTypeFlow()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.FindOrDefault<string>()
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(invocation);

        Assert.Equal(NullableAnnotation.NotAnnotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
    }

    [Fact]
    public void MaybeNullReturn_UnconstrainedGenericReflectsConstructedValueTypeFlow()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.FindOrDefault<int>()
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(invocation);

        Assert.Equal(NullableAnnotation.NotAnnotated, typeInfo.Nullability.Annotation);
        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
    }

    [Fact]
    public void NotNullIfNotNullReturn_IsNotNullForNonNullArgument()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int => NullableFlowFixture.Echo("raven").Length
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NotNullIfNotNullReturn_RemainsMaybeNullForNullableArgument()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length(value: string?) -> int => NullableFlowFixture.Echo(value).Length
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Single(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Fact]
    public void NotNullIfNotNullReturn_UsesNarrowedArgumentFlow()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length(value: string?) -> int {
    if value is null {
        return 0
    }

    return NullableFlowFixture.Echo(value).Length
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NotNullOutParameter_UpdatesArgumentFlowAfterCall(bool diagnosticsFirst)
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int {
    var value: string? = null
    NullableFlowFixture.SetName(out value)
    return value.Length
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(memberAccess => memberAccess.Name.Identifier.ValueText == "Length")
            .Expression;
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableFlowState.NotNull, typeInfo.Nullability.FlowState);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MaybeNullGenericOutParameter_InvalidatesArgumentFlowAfterCall(bool diagnosticsFirst)
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Length() -> int {
    var value: string? = "raven"
    NullableFlowFixture.SetDefault<string>(out value)
    return value.Length
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var receiver = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(memberAccess => memberAccess.Name.Identifier.ValueText == "Length")
            .Expression;
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
        var typeInfo = compilation.GetSemanticModel(tree).GetTypeInfo(receiver);

        Assert.Equal(NullableFlowState.MaybeNull, typeInfo.Nullability.FlowState);
    }
}
