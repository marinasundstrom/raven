using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.Tests;

internal static class ILReader
{
    private static readonly OpCode[] SingleByteOpCodes;
    private static readonly OpCode[] MultiByteOpCodes;

    static ILReader()
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

    public static IReadOnlyList<OpCode> GetOpCodes(MethodInfo method)
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

    public static IReadOnlyList<string> GetCalledMembers(MethodInfo method)
    {
        var body = method.GetMethodBody() ?? throw new InvalidOperationException("Method has no body.");
        var il = body.GetILAsByteArray() ?? throw new InvalidOperationException("Method body has no IL.");
        var members = new List<string>();
        var module = method.Module;
        var declaringTypeArguments = method.DeclaringType?.GetGenericArguments() ?? Type.EmptyTypes;
        var methodTypeArguments = method.GetGenericArguments();

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
                case OperandType.InlineTok:
                case OperandType.InlineType:
                    i += 4;
                    break;
                case OperandType.InlineMethod:
                    {
                        var token = BitConverter.ToInt32(il, i);
                        i += 4;

                        if (opcode == OpCodes.Call || opcode == OpCodes.Callvirt)
                        {
                            try
                            {
                                if (module.ResolveMember(token, declaringTypeArguments, methodTypeArguments) is MemberInfo member)
                                {
                                    var declaringTypeName = member.DeclaringType?.FullName ?? "<unknown>";
                                    members.Add($"{declaringTypeName}::{member.Name}");
                                }
                                else
                                {
                                    members.Add($"<token:0x{token:X8}>");
                                }
                            }
                            catch
                            {
                                members.Add($"<token:0x{token:X8}>");
                            }
                        }

                        break;
                    }
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

        return members;
    }
}
