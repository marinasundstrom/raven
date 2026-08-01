using System.Linq;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class TypeParameterConstraintDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData("""
        func Convert<T>(value: T) -> T
            where U: struct {
            value
        }
        """)]
    [InlineData("""
        let convert = func<T>(value: T) where U: struct {
            value
        }
        """)]
    [InlineData("""
        class Converter {
            func Convert<T>(value: T) -> T
                where U: struct {
                value
            }
        }
        """)]
    [InlineData("""
        macro func Convert<T>(value: T)
            where U: struct {
            expand value
        }
        """)]
    public void UnknownConstraintClauseTypeParameter_IsDiagnosed(string source)
    {
        var (compilation, _) = CreateCompilation(source);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnknownTypeParameterInConstraintClause);

        Assert.Contains("'U'", diagnostic.GetMessage());
    }

    [Fact]
    public void ConstraintClauseWithoutTypeParameterList_IsDiagnosed()
    {
        const string source = """
            func Run() where T: struct {
            }
            """;
        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnknownTypeParameterInConstraintClause);
    }
}
