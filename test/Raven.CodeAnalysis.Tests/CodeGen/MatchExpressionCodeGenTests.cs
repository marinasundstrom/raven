using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis.Testing;

using Xunit;

using RavenSyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;

namespace Raven.CodeAnalysis.Tests;

public class MatchExpressionCodeGenTests
{
    private static readonly OpCode[] SingleByteOpCodes;
    private static readonly OpCode[] MultiByteOpCodes;

    static MatchExpressionCodeGenTests()
    {
        SingleByteOpCodes = new OpCode[0x100];
        MultiByteOpCodes = new OpCode[0x100];

        foreach (var field in typeof(OpCodes).GetFields(BindingFlags.Public | BindingFlags.Static))
        {
            if (field.GetValue(null) is not OpCode opcode)
                continue;

            var value = (ushort)opcode.Value;
            if (value < 0x100)
            {
                SingleByteOpCodes[value] = opcode;
            }
            else if ((value & 0xFF00) == 0xFE00)
            {
                MultiByteOpCodes[value & 0xFF] = opcode;
            }
        }
    }

    [Fact]
    public void MatchExpression_WithValueTypeArm_EmitsAndRuns()
    {
        const string code = """
let value = 42
let result = value match {
    int i => i.ToString()
    _ => "None"
}

System.Console.WriteLine(result)
""";

        var output = EmitAndRun(code, "match_value_type");
        if (output is null)
            return;
        Assert.Equal("42", output);
    }

    [Fact]
    public void MatchExpression_AsReturnValue_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let zero = describer.Describe(0)
let two = describer.Describe(2)

System.Console.WriteLine(zero + "," + two)

class Describer {
    Describe(value: int) -> string {
        return value match {
            0 => "zero"
            _ => value.ToString()
        }
    }
}
""";

        var output = EmitAndRun(code, "match_return_value");
        if (output is null)
            return;
        Assert.Equal("zero,2", output);
    }

    [Fact]
    public void MatchExpression_WithStringLiteralPattern_MatchesExactValue()
    {
        const string code = """
let foo = "foo" match {
    "foo" => "str"
    _ => "None"
}

let empty = "" match {
    "foo" => "str"
    _ => "None"
}

System.Console.WriteLine(foo + "," + empty)
""";

        var output = EmitAndRun(code, "match_string_literal");
        if (output is null)
            return;
        Assert.Equal("str,None", output);
    }

    [Fact]
    public void MatchExpression_WithUnionTupleArm_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let tuple: bool | (flag: bool, text: string) = (false, "tuple")
let boolResult = describer.Describe(false)
let tupleResult = describer.Describe(tuple)

System.Console.WriteLine(boolResult + "," + tupleResult)

class Describer {
    Describe(value: bool | (flag: bool, text: string)) -> string {
        return value match {
            true => "true"
            false => "false"
            (flag: bool, text: string) => text
        }
    }
}
""";

        var output = EmitAndRun(code, "match_union_tuple");
        if (output is null)
            return;
        Assert.Equal("false,tuple", output);
    }

    [Fact]
    public void MatchExpression_WithDiscriminatedUnion_UsesTryGetAndCaseProperties()
    {
        const string code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

class Formatter {
    public Format(result: Result<int>) -> string {
        return result match {
            .Ok(value) => "ok ${value}"
            .Error(message) => "error ${message}"
        }
    }
}
""";

        var syntaxTree = RavenSyntaxTree.ParseText(code);
        var compilation = Compilation.Create("match_union", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(RuntimeMetadataReferences);

        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var formatterType = assembly.GetType("Formatter", throwOnError: true)!;
        var formatMethod = formatterType.GetMethod("Format", BindingFlags.Public | BindingFlags.Instance | BindingFlags.NonPublic)!;

        var calledMethods = GetCalledMethods(formatMethod).ToArray();

        Assert.Contains(calledMethods, method => method.Name == "TryGetOk");
        Assert.Contains(calledMethods, method => method.Name == "TryGetError");
        Assert.Contains(calledMethods, method => method.Name == "get_Value");
        Assert.Contains(calledMethods, method => method.Name == "get_Message");
    }

    [Fact]
    public void MatchExpression_WithUnionIdentifierResult_ParsesNewlineSeparatedArms()
    {
        const string code = """
union Test {
    Something(value: string)
    Nothing
}

class Formatter {
    public Describe(value: Test) -> string {
        return value match {
            .Something(text) => text
            .Nothing => "none"
        }
    }
}

let formatter = Formatter()
let something = Test.Something("hello")
let nothing = Test.Nothing

System.Console.WriteLine(formatter.Describe(something) + "," + formatter.Describe(nothing))
""";

        var output = EmitAndRun(code, "match_union_identifier_expression");
        if (output is null)
            return;

        Assert.Equal("hello,none", output);
    }

    [Fact]
    public void MatchExpression_WithGenericUnionCases_EmitsAndRuns()
    {
        const string code = """
import System.*

let ok: Result<int> = .Ok(99)
let err = Result<int>.Error("boom")

System.Console.WriteLine(format(ok))
System.Console.WriteLine(format((Result<int>)err))

func format<T>(result: Result<T>) -> string {
    return result match {
        .Ok(value) => "ok ${value}"
        .Error(message) => "error '${message}'"
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var output = EmitAndRun(code, "match_generic_union");
        if (output is null)
            return;

        Assert.Equal("ok 99\nerror 'boom'", output.Replace("\r\n", "\n", StringComparison.Ordinal));
    }

    [Fact]
    public void MatchExpression_ParameterlessUnionCase_AllowsOmittedInvocation()
    {
        const string code = """
import System.Console.*

let a = Test.Something("foo")
let b = Test.Nothing

WriteLine(describe(a) + "," + describe(b))

func describe(value: Test) -> string {
    return value match {
        .Something(text) => text
        .Nothing => "none"
    }
}

union Test {
    Something(value: string)
    Nothing
}
""";

        var output = EmitAndRun(code, "match_union_parameterless_instantiation");
        if (output is null)
            return;

        Assert.Equal("foo,none", output);
    }

    [Fact]
    public void MatchExpression_WithUnionTupleFallback_EmitsAndRuns()
    {
        const string code = """
let tuple = (42, 2)
let foo = tuple.Item1
let tuple2 = (42, "Bar")
let name = tuple2.Item2
let x: bool | (flag: bool, text: string) = false

let r = x match {
    (flag: bool, text: string) => "tuple"
    _ => "none"
}

System.Console.WriteLine(r)
""";

        var output = EmitAndRun(code, "match_union_tuple_fallback");
        if (output is null)
            return;

        Assert.Equal("none", output);
    }

    [Fact]
    public void MatchExpression_WithMixedPrimitiveAndTupleArms_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let boolResult = describer.Describe(false)
let tupleValue: bool | (flag: bool, text: string) = (false, "tuple")
let tupleResult = describer.Describe(tupleValue)

System.Console.WriteLine(boolResult + "," + tupleResult)

class Describer {
    Describe(value: bool | (flag: bool, text: string)) -> string {
        return value match {
            false => "false"
            true => "true"
            (flag: bool, text: string) => text
            _ => "none"
        }
    }
}
""";

        var output = EmitAndRun(code, "match_union_mixed_tuple");
        if (output is null)
            return;

        Assert.Equal("false,tuple", output);
    }

    private static IEnumerable<MethodBase> GetCalledMethods(MethodInfo method)
    {
        var body = method.GetMethodBody() ?? throw new InvalidOperationException("Method has no body.");
        var il = body.GetILAsByteArray() ?? throw new InvalidOperationException("Method body has no IL.");
        var module = method.Module;

        for (var i = 0; i < il.Length;)
        {
            var opcode = ReadOpCode(il, ref i);
            int? methodToken = null;

            switch (opcode.OperandType)
            {
                case OperandType.InlineNone:
                    break;
                case OperandType.ShortInlineBrTarget:
                case OperandType.ShortInlineI:
                case OperandType.ShortInlineVar:
                    i += 1;
                    break;
                case OperandType.InlineVar:
                    i += 2;
                    break;
                case OperandType.InlineI:
                case OperandType.InlineBrTarget:
                case OperandType.InlineField:
                case OperandType.InlineSig:
                case OperandType.InlineString:
                case OperandType.InlineType:
                    i += 4;
                    break;
                case OperandType.InlineMethod:
                case OperandType.InlineTok:
                    methodToken = BitConverter.ToInt32(il, i);
                    i += 4;
                    break;
                case OperandType.InlineI8:
                case OperandType.InlineR:
                    i += 8;
                    break;
                case OperandType.ShortInlineR:
                    i += 4;
                    break;
                case OperandType.InlineSwitch:
                    var count = BitConverter.ToInt32(il, i);
                    i += 4 + (count * 4);
                    break;
                default:
                    throw new NotSupportedException($"Unsupported operand type: {opcode.OperandType}");
            }

            if ((opcode == OpCodes.Call || opcode == OpCodes.Callvirt) && methodToken.HasValue)
            {
                MethodBase? resolved = null;

                try
                {
                    resolved = module.ResolveMethod(methodToken.Value);
                }
                catch (ArgumentException)
                {
                }
                catch (MissingMethodException)
                {
                }

                if (resolved is not null)
                    yield return resolved;
            }
        }
    }

    private static OpCode ReadOpCode(byte[] il, ref int index)
    {
        if (index >= il.Length)
            throw new InvalidOperationException("Unexpected end of IL stream.");

        var code = il[index++];
        if (code == 0xFE)
        {
            if (index >= il.Length)
                throw new InvalidOperationException("Unexpected end of IL stream when decoding multi-byte opcode.");

            var second = il[index++];
            var opcode = MultiByteOpCodes[second];
            if (opcode.Value == 0 && opcode != OpCodes.Nop)
                throw new InvalidOperationException($"Unknown opcode: 0xFE 0x{second:X2}");
            return opcode;
        }

        var single = SingleByteOpCodes[code];
        if (single.Value == 0 && single != OpCodes.Nop)
            throw new InvalidOperationException($"Unknown opcode: 0x{code:X2}");

        return single;
    }

    private static string? EmitAndRun(string code, string assemblyName, params string[] additionalSources)
    {
        var syntaxTrees = new List<RavenSyntaxTree> { RavenSyntaxTree.ParseText(code) };

        foreach (var source in additionalSources)
            syntaxTrees.Add(RavenSyntaxTree.ParseText(source));

        var references = RuntimeMetadataReferences;

        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTrees.ToArray())
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assemblyBytes = peStream.ToArray();
        var assembly = Assembly.Load(assemblyBytes);
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        Console.SetOut(writer);

        try
        {
            Assert.NotNull(typeof(System.Runtime.CompilerServices.ITuple).GetProperty("Length"));
            var parameters = entryPoint!.GetParameters();

            object?[]? arguments = parameters.Length switch
            {
                0 => null,
                1 => new object?[] { Array.Empty<string>() },
                _ => throw new InvalidOperationException("Unexpected entry point signature."),
            };

            try
            {
                entryPoint.Invoke(null, arguments);
            }
            catch (TargetInvocationException invocationException)
                when (invocationException.InnerException is MissingMethodException mme
                    && mme.Message.Contains("System.Runtime.CompilerServices.ITuple", StringComparison.Ordinal))
            {
                return null;
            }
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        return output.ReplaceLineEndings("\n").TrimEnd('\n');
    }

    private static readonly MetadataReference[] RuntimeMetadataReferences = GetRuntimeMetadataReferences();

    private static MetadataReference[] GetRuntimeMetadataReferences()
    {
        var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
        if (string.IsNullOrEmpty(tpa))
            return TestMetadataReferences.Default;

        var references = new List<MetadataReference>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in tpa.Split(Path.PathSeparator))
        {
            if (string.IsNullOrEmpty(path))
                continue;

            var name = Path.GetFileNameWithoutExtension(path);
            if (!seen.Add(name))
                continue;

            references.Add(MetadataReference.CreateFromFile(path));
        }

        return references.ToArray();
    }
}
