using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class NullLiteralEmissionTests
{
    private static readonly OpCode[] SingleByteOpCodes;
    private static readonly OpCode[] MultiByteOpCodes;

    static NullLiteralEmissionTests()
    {
        SingleByteOpCodes = new OpCode[0x100];
        MultiByteOpCodes = new OpCode[0x100];

        foreach (var field in typeof(OpCodes).GetFields(BindingFlags.Public | BindingFlags.Static))
        {
            if (field.GetValue(null) is OpCode opcode)
            {
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
    }

    private static IReadOnlyList<OpCode> GetOpCodes(MethodInfo method)
    {
        var body = method.GetMethodBody() ?? throw new InvalidOperationException("Method has no body.");
        var il = body.GetILAsByteArray() ?? throw new InvalidOperationException("Method body has no IL.");
        var opcodes = new List<OpCode>();

        for (var i = 0; i < il.Length;)
        {
            OpCode opcode;
            var code = il[i++];
            if (code == 0xFE)
            {
                if (i >= il.Length)
                    throw new InvalidOperationException("Unexpected end of IL stream when decoding multi-byte opcode.");

                var second = il[i++];
                opcode = MultiByteOpCodes[second];
            }
            else
            {
                opcode = SingleByteOpCodes[code];
            }

            if (opcode.Value == 0 && opcode != OpCodes.Nop)
                throw new InvalidOperationException($"Unknown opcode: 0x{code:X2}");

            opcodes.Add(opcode);

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
                case OperandType.InlineMethod:
                case OperandType.InlineSig:
                case OperandType.InlineString:
                case OperandType.InlineTok:
                case OperandType.InlineType:
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
        }

        return opcodes;
    }

    [Fact]
    public void NullableReference_ReturningNull_EmitsNull()
    {
        const string code = """
class C {
    Run() -> string? {
        return null
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;

        var value = method.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);

        var opcodes = GetOpCodes(method);
        Assert.Contains(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void NullableValue_ReturningNull_EmitsDefault()
    {
        const string code = """
class C {
    Run() -> int? {
        return null
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;

        var value = method.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);

        var opcodes = GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void NullableValue_LocalInitializedWithNull_ReturnsDefault()
    {
        const string code = """
class C {
    Run() -> int? {
        let value: int? = null
        return value
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;

        var value = method.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);

        var opcodes = GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }
}
