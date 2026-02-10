using Raven.CodeAnalysis.Testing;
using System.Text.Json;

namespace Raven.Core.Tests;

public sealed class ResultTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_Ok()
    {
        Result<int, string> result = new Result<int, string>.Ok(123);

        var json = JsonSerializer.Serialize(result);
        var parsed = JsonSerializer.Deserialize<Result<int, string>>(json);

        Assert.NotNull(parsed);
        Assert.True(parsed.Value.TryGetOk(out var ok));
        Assert.Equal(123, ok.Value);
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_Error()
    {
        Result<int, string> result = new Result<int, string>.Error("boom");

        var json = JsonSerializer.Serialize(result);
        var parsed = JsonSerializer.Deserialize<Result<int, string>>(json);

        Assert.NotNull(parsed);
        Assert.True(parsed.Value.TryGetError(out var error));
        Assert.Equal("boom", error.Data);
    }

    [Fact]
    public void MapErrorAndUnwrapError_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val result: Result<int, string> = .Error("boom")
val hasError = result.HasError
val text = result.MapError(message => message + "!").UnwrapError()
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void MapError_WithWrongLambdaType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val result: Result<int, string> = .Error("boom")
val _ = result.MapError((v: int) => v)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501")
                    .WithAnySpan()
                    .WithArguments("method", "MapError", 1)
            ]);

        verifier.Verify();
    }
}
