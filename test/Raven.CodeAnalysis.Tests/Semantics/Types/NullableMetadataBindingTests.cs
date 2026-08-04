using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NullableMetadataBindingTests : CompilationTestBase
{
    [Fact]
    public void NotNullWhen_DoesNotRefineNullableArgument()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Inspect(value: string?) -> unit {
    if NullableFlowFixture.ArePresent(value, value) {
        let length = value.Length
    }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void MemberNotNull_DoesNotRefineNullableProperty()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Inspect(holder: MemberNullabilityFixture) -> unit {
    holder.Initialize()
    let length = holder.Value.Length
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void AllowNullParameter_AcceptsNullInput()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.AcceptNull(null)
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void DisallowNullParameter_RejectsNullInput()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.RejectNull(null)
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Theory]
    [InlineData("RequiredName", false)]
    [InlineData("OptionalName", true)]
    [InlineData("OrdinaryName", true)]
    public void PropertyInputContract_ControlsNullAssignment(string propertyName, bool expectDiagnostic)
    {
        var source = $$"""
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.{{propertyName}} = null
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Equal(
            expectDiagnostic,
            compilation.GetDiagnostics().Any(
                diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType));
    }

    [Fact]
    public void MaybeNullReturn_HasStaticNullableType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.FindName()
let length = value.Length
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();
        var invocationType = compilation.GetSemanticModel(tree).GetTypeInfo(invocation).Type;

        Assert.True(invocationType?.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void MaybeNullReturn_CannotInitializeNonNullableBinding()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value: string = NullableFlowFixture.FindName()
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic =>
                diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType ||
                diagnostic.Descriptor == CompilerDiagnostics.CannotAssignFromTypeToType);
    }

    [Fact]
    public void NotNullIfNotNull_DoesNotChangeDeclaredNullableReturnType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.Echo("raven")
let length = value.Length
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();

        Assert.True(compilation.GetSemanticModel(tree).GetTypeInfo(invocation).Type?.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }
}
