using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Functions.Async;

public sealed class AsyncIteratorCancellationTests : CompilationTestBase
{
    [Fact]
    public void AsyncIterator_WithCancellationTokenParameterWithoutEnumeratorCancellation_ReportsWarning()
    {
        const string source = """
import System.Collections.Generic.*
import System.Threading.*

class C {
    async func Stream(cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
        yield return 1
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics, static d => d.Descriptor == CompilerDiagnostics.EnumeratorCancellationAttributeMissing);
        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
    }

    [Fact]
    public void AsyncIterator_WithEnumeratorCancellation_DoesNotReportMissingAttributeWarning()
    {
        const string source = """
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*

class C {
    async func Stream([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
        yield return 1
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static d => d.Descriptor == CompilerDiagnostics.EnumeratorCancellationAttributeMissing);
    }
}
