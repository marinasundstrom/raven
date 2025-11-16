using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class UnionCodeGenTests
{
    [Fact]
    public void UnionConversionAndTryGet_EmitsStructCases()
    {
        const string source = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Program {
    static Main() -> unit {
        Print(Result<int>.Ok(value: 42));
        Print(Result<int>.Error(message: "boom"));
    }

    static Print(result: Result<int>) -> unit {
        var ok: Result<int>.Ok;
        if (result.TryGetOk(ref ok)) {
            System.Console.WriteLine(ok.value);
            return;
        }

        var error: Result<int>.Error;
        if (result.TryGetError(ref error)) {
            System.Console.WriteLine(error.message);
        }
    }
}
""";

        var output = CodeGenTestUtilities.EmitAndRun(source, "union_ok_error");
        if (output is null)
            return;

        Assert.Equal("42\nboom", output);
    }

    [Fact]
    public void UnionTryGet_RefParameterForParameterlessCase()
    {
        const string source = """
union Maybe<T> {
    None
    Some(value: T)
}

class Program {
    static Main() -> unit {
        Print(Maybe<int>.None());
        Print(Maybe<int>.Some(value: 5));
    }

    static Print(value: Maybe<int>) -> unit {
        var payload: Maybe<int>.Some;
        let found = value.TryGetSome(ref payload);
        System.Console.WriteLine(found);
        System.Console.WriteLine(payload.value);
    }
}
""";

        var output = CodeGenTestUtilities.EmitAndRun(source, "union_none_some");
        if (output is null)
            return;

        Assert.Equal("false\n0\ntrue\n5", output);
    }

    [Fact]
    public void UnionCaseConstructor_WithMultiplePayloadFields_PersistsValues()
    {
        const string source = """
union Pair<T> {
    Point(x: T, y: T)
}

class Program {
    static Main() -> unit {
        let pair: Pair<int> = Pair<int>.Point(x: 2, y: 3);
        var payload: Pair<int>.Point;
        if (pair.TryGetPoint(ref payload)) {
            System.Console.WriteLine(payload.x + payload.y);
        }
    }
}
""";

        var output = CodeGenTestUtilities.EmitAndRun(source, "union_pair_point");
        if (output is null)
            return;

        Assert.Equal("5", output);
    }
}
