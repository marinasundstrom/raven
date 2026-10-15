using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ClosedGenericPatternIdentityTests : DiagnosticTestBase
{
    [Theory]
    [InlineData("class Box<T> {}", "Box<int>", "Box<long>")]
    [InlineData("struct Box<T> {}", "Box<int>", "Box<long>")]
    [InlineData("class Box<T> {}", "Box<Box<int>>", "Box<Box<long>>")]
    [InlineData("import System.Collections.Generic.*", "List<int>", "List<long>")]
    public void DistinctClosedTypes_RemainReachable(string declaration, string first, string second)
    {
        var code = $$"""
{{declaration}}
func Inspect(value: object) -> int {
    return match value {
        let first: {{first}} => 1
        let second: {{second}} => 2
        _ => 0
    }
}
""";
        CreateVerifier(code).Verify();
    }

    [Fact]
    public void RepeatedClosedType_RemainsUnreachable()
    {
        const string code = """
class Box<T> {}
func Inspect(value: object) -> int {
    return match value {
        let first: Box<int> => 1
        let second: Box<int> => 2
        _ => 0
    }
}
""";
        CreateVerifier(code,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan()])
            .Verify();
    }
}
