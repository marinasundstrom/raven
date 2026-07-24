namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SourceRefStructRestrictionTests : CompilationTestBase
{
    [Fact]
    public void SourceRefStruct_CannotBeStoredInClassField()
    {
        const string source = """
            ref struct Buffer {}

            class Container {
                field Value: Buffer
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefLikeTypeCannotBeUsedAsField);
    }

    [Fact]
    public void SourceRefStruct_CanBeStoredInRefStructField()
    {
        const string source = """
            ref struct Buffer {}

            ref struct Container {
                field Value: Buffer
            }
            """;

        AssertNoErrors(source);
    }

    [Fact]
    public void SourceRefStruct_CannotBeArrayElement()
    {
        const string source = """
            ref struct Buffer {}

            func Consume(values: Buffer[]) {}
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefLikeTypeCannotBeUsedAsArrayElement);
    }

    [Fact]
    public void SourceRefStruct_CannotBeBoxed()
    {
        const string source = """
            ref struct Buffer {}

            func Create() -> object {
                Buffer()
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void SourceRefStruct_CannotBeCaptured()
    {
        const string source = """
            ref struct Buffer {}

            func Run() {
                val buffer = Buffer()
                val capture = () => buffer
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefLikeVariableCannotBeCaptured);
    }

    [Fact]
    public void SourceRefStruct_CannotCrossAwait()
    {
        const string source = """
            import System.Threading.Tasks.*

            ref struct Buffer {}

            async func Run() -> Task {
                val buffer = Buffer()
                await Task.CompletedTask
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefLikeVariableCannotCrossAwait);
    }

    [Fact]
    public void SourceRefStruct_CannotBeStoredInIterator()
    {
        const string source = """
            import System.Collections.Generic.*

            ref struct Buffer {}

            func Values(buffer: Buffer) -> IEnumerable<int> {
                yield return 1
            }
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.RefLikeVariableCannotBeStoredInIterator);
    }

    [Fact]
    public void SourceRefStruct_RequiresAllowByRefLikeGenericParameter()
    {
        const string source = """
            ref struct Buffer {}

            func Run() {
                Reject<Buffer>()
            }

            func Reject<T>() {}
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
    }

    private void AssertHasDiagnostic(string source, DiagnosticDescriptor descriptor)
    {
        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == descriptor);
    }

    private void AssertNoErrors(string source)
    {
        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
