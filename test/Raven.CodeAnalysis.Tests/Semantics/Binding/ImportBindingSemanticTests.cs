using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ImportBindingSemanticTests : CompilationTestBase
{
    [Fact]
    public void WildcardTypeImport_DoesNotBringInstanceMethodsIntoUnqualifiedLookup()
    {
        const string source = """
import Utility.*

val built = Build()
val tapped = Tap()

class Utility {
    static func Build() -> int {
        return 1
    }

    func Tap() -> int {
        return 1
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        var errors = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();

        Assert.DoesNotContain(errors, static diagnostic =>
            diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id &&
            diagnostic.GetMessage().Contains("'Build'"));
        Assert.Contains(errors, static diagnostic =>
            diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id &&
            diagnostic.GetMessage().Contains("'Tap'"));
    }
}
