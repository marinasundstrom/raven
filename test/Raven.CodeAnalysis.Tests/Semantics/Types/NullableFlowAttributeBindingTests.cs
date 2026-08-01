using System.Linq;

using Raven.CodeAnalysis.Tests;

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
}
