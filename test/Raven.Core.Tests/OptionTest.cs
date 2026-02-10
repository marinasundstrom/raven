using Raven.CodeAnalysis.Testing;
using System.Text.Json;

namespace Raven.Core.Tests;

public sealed class OptionTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_Some()
    {
        Option<int> option = new Option<int>.Some(42);

        var json = JsonSerializer.Serialize(option);
        var parsed = JsonSerializer.Deserialize<Option<int>>(json);

        Assert.NotNull(parsed);
        Assert.True(parsed.Value.TryGetSome(out var some));
        Assert.Equal(42, some.Value);
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_None()
    {
        Option<int> option = new Option<int>.None();

        var json = JsonSerializer.Serialize(option);
        var parsed = JsonSerializer.Deserialize<Option<int>>(json);

        Assert.NotNull(parsed);
        Assert.True(parsed.Value.TryGetNone(out _));
    }

    [Fact]
    public void MapThenWhere_BindsFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val input: Option<int> = .Some(20)
val mapped = input.Map(v => v + 1)
val filtered = mapped.Where(v => v > 10)
val output = filtered.UnwrapOr(0)
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void Map_WithWrongLambdaType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val input: Option<int> = .Some(20)
val _ = input.Map((v: string) => v)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501")
                    .WithAnySpan()
                    .WithArguments("method", "Map", 1)
            ]);

        verifier.Verify();
    }
}
