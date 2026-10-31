using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class StructuralUnionExhaustivenessTests
{
    [Theory]
    [InlineData("A a => 1\n B b => 2", true)]
    [InlineData("A a => 1", false)]
    [InlineData("A a when flag => 1\n B b => 2", false)]
    [InlineData("A a when true => 1\n B b => 2", true)]
    [InlineData("A a => 1\n _ => 2", true)]
    public void NativeUnionMatch_ReportsAlternativeCoverage(string arms, bool exhaustive)
    {
        var source = $$"""
            class A {}
            class B {}
            func Check(value: A | B, flag: bool) -> int {
                return match value { {{arms}} }
            }
            """;
        Check(source, exhaustive);
    }

    [Fact]
    public void NativeUnionMatch_InsideGenericCasePayload_IsExhaustive()
    {
        Check("""
            union A { case Failed(code: int) }
            union B { case Failed(message: string) }
            union Outcome<T, E> {
                case Ok(value: T)
                case Error(error: E)
            }
            func Adapt<T>(result: Outcome<T, A | B>) -> int {
                return match result {
                    .Ok(let value) => 0
                    .Error(let error) => match error {
                        A reason => 1
                        B reason => 2
                    }
                }
            }
            """, true);
    }

    private static void Check(string source, bool exhaustive)
    {
        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetPlatform(TargetPlatform.NeoCLR)
            .WithMetadataImportOptions(new MetadataImportOptions());
        var compilation = Compilation.Create("union-coverage", [tree], TestMetadataReferences.DefaultWithRavenCore, options);
        var model = compilation.GetSemanticModel(tree);
        var errors = model.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.DoesNotContain(errors, d => d.Id != "RAV2100");
        Assert.Equal(!exhaustive, errors.Any(d => d.Id == "RAV2100"));
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Last();
        Assert.Equal(exhaustive, model.GetMatchExhaustiveness(match).IsExhaustive);
    }
}
