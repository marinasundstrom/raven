using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

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

    [Fact]
    public void UnionCasePattern_MatchExpression_EmitsTryGet()
    {
        const string source = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Program {
    static Main() -> unit {
        Print(Result<int>.Ok(value: 5));
        Print(Result<int>.Error(message: "boom"));
    }

    static Print(value: Result<int>) -> unit {
        let text = value match {
            .Ok(let payload) => $"ok {payload}",
            .Error(let message) => $"error {message}",
        };

        System.Console.WriteLine(text);
    }
}
""";

        var output = CodeGenTestUtilities.EmitAndRun(source, "union_case_pattern_match");
        if (output is null)
            return;

        Assert.Equal("ok 5\nerror boom", output);
    }

    [Fact]
    public void TargetTypedMemberBinding_ConstructsUnionCasesInReturnAndAssignment()
    {
        const string source = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Program {
    static Main() -> unit {
        System.Console.WriteLine(Describe(Divide(9, 3)));
        System.Console.WriteLine(Describe(Divide(1, 0)));
    }

    static Divide(numerator: int, denominator: int) -> Result<int> {
        if denominator == 0 {
            return .Error(message: "Cannot divide by zero");
        }

        let result: Result<int> = .Ok(value: numerator / denominator);
        return result;
    }

    static Describe(value: Result<int>) -> string {
        var ok: Result<int>.Ok;
        if (value.TryGetOk(ref ok)) {
            return ok.value.ToString();
        }

        var error: Result<int>.Error;
        if (value.TryGetError(ref error)) {
            return error.message;
        }

        return "unknown";
    }
}
""";

        var output = CodeGenTestUtilities.EmitAndRun(source, "union_target_typed_case_binding");
        if (output is null)
            return;

        Assert.Equal("3\nCannot divide by zero", output);
    }

    [Fact]
    public void UnionCaseStructs_WithGenericPayloads_EmitInvalidMetadata()
    {
        const string source = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Program {
    static Main() -> unit { }
}
""";

        var image = CodeGenTestUtilities.EmitAssembly(source, "union_generic_case_metadata");
        var assembly = Assembly.Load(image);

        var exception = Assert.Throws<ReflectionTypeLoadException>(() => assembly.GetTypes());
        Assert.Contains(
            exception.LoaderExceptions.OfType<TypeLoadException>(),
            loader => loader.Message.Contains("field of an illegal type", StringComparison.Ordinal));
    }

    [Fact]
    public void UnionCaseStructs_GenericUnion_DefineCasesWithoutForwardingTypeParameters()
    {
        const string source = """
union Result<T> {
    Ok(value: T)
}

class Program {
    static Main() -> unit { }
}
""";

        var image = CodeGenTestUtilities.EmitLibrary(source, "union_generic_case_signature");
        using var peReader = new PEReader(new MemoryStream(image));
        var reader = peReader.GetMetadataReader();

        var unionHandle = reader.TypeDefinitions.Single(handle =>
        {
            var definition = reader.GetTypeDefinition(handle);
            return reader.GetString(definition.Name) == "Result`1";
        });

        var unionDefinition = reader.GetTypeDefinition(unionHandle);
        var okHandle = unionDefinition.GetNestedTypes().Single(handle =>
        {
            var nested = reader.GetTypeDefinition(handle);
            return reader.GetString(nested.Name) == "Ok";
        });

        var okDefinition = reader.GetTypeDefinition(okHandle);
        Assert.Equal(0, okDefinition.GetGenericParameters().Count);

        var payloadFieldHandle = okDefinition.GetFields().Single();
        var payloadField = reader.GetFieldDefinition(payloadFieldHandle);
        var signatureReader = reader.GetBlobReader(payloadField.Signature);
        var signature = signatureReader.ReadBytes(signatureReader.Length);

        Assert.Contains((byte)SignatureTypeCode.GenericTypeParameter, signature);
    }
}
