using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class IteratorILGenerationTests
{
    private const string IteratorCode = """
import System.Collections.Generic.*

class C {
    Values() -> IEnumerable<int> {
        yield return 42
        var i = 0
        while i < 2 {
            yield return i
            i = i + 1
        }
    }
}
""";

    [Fact]
    public void MoveNext_DoesNotEmitStackClearingPops()
    {
        var (_, instructions) = CaptureIteratorInstructions(static methodGenerator =>
            methodGenerator.MethodSymbol.Name == "MoveNext" &&
            methodGenerator.MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol);

        var formatted = instructions
            .Select(RecordedInstructionFormatter.Format)
            .ToArray();

        var expected = new[]
        {
            "ldc.i4.0",
            "stloc.1",
            "ldarg.0",
            "ldfld <>c__Iterator0._state",
            "ldc.i4 0",
            "ceq",
            "brtrue L0",
            "br L1",
            "ldarg.0",
            "ldc.i4 -1",
            "stfld <>c__Iterator0._state",
            "br L2",
            "ldarg.0",
            "ldfld <>c__Iterator0._state",
            "ldc.i4 1",
            "ceq",
            "brtrue L3",
            "br L4",
            "ldarg.0",
            "ldc.i4 -1",
            "stfld <>c__Iterator0._state",
            "br L5",
            "ldarg.0",
            "ldfld <>c__Iterator0._state",
            "ldc.i4 2",
            "ceq",
            "brtrue L6",
            "br L7",
            "ldarg.0",
            "ldc.i4 -1",
            "stfld <>c__Iterator0._state",
            "br L8",
            "ldc.i4.0",
            "stloc.1",
            "br L9",
            "ldarg.0",
            "ldc.i4 42",
            "stfld <>c__Iterator0._current",
            "ldarg.0",
            "ldc.i4 1",
            "stfld <>c__Iterator0._state",
            "ldc.i4.1",
            "stloc.1",
            "br L9",
            "ldarg.0",
            "ldc.i4 0",
            "stfld <>c__Iterator0._local0",
            "ldarg.0",
            "ldfld <>c__Iterator0._local0",
            "ldc.i4 2",
            "clt",
            "brtrue L11",
            "br L12",
            "ldarg.0",
            "ldarg.0",
            "ldfld <>c__Iterator0._local0",
            "stfld <>c__Iterator0._current",
            "ldarg.0",
            "ldc.i4 2",
            "stfld <>c__Iterator0._state",
            "ldc.i4.1",
            "stloc.1",
            "br L9",
            "ldarg.0",
            "ldarg.0",
            "ldfld <>c__Iterator0._local0",
            "ldc.i4 1",
            "add",
            "stfld <>c__Iterator0._local0",
            "br L10",
            "ldarg.0",
            "ldc.i4 -1",
            "stfld <>c__Iterator0._state",
            "ldc.i4.0",
            "stloc.1",
            "br L9",
            "ldloc.1",
            "ret",
        };

        Assert.Equal(expected, formatted);
        Assert.DoesNotContain(instructions, instruction => instruction.Opcode == OpCodes.Pop);
    }

    [Fact]
    public void CurrentGetter_LoadsCurrentField()
    {
        var (_, instructions) = CaptureIteratorInstructions(IteratorMethodNamed("get_Current"));

        var prefix = instructions.Take(3).ToArray();
        Assert.Equal(3, prefix.Length);

        Assert.Collection(prefix,
            instruction => Assert.Equal(OpCodes.Ldarg_0, instruction.Opcode),
            instruction =>
            {
                Assert.Equal(OpCodes.Ldfld, instruction.Opcode);
                var field = Assert.IsAssignableFrom<FieldBuilder>(instruction.Operand.Value);
                Assert.Equal("_current", field.Name);
            },
            instruction => Assert.Equal(OpCodes.Ret, instruction.Opcode));
    }

    [Fact]
    public void IEnumeratorCurrentGetter_BoxesCurrentValue()
    {
        var (_, instructions) = CaptureIteratorInstructions(IteratorMethodNamed("System.Collections.IEnumerator.get_Current"));

        var prefix = instructions.Take(5).ToArray();
        Assert.Equal(5, prefix.Length);

        Assert.Collection(prefix,
            instruction => Assert.Equal(OpCodes.Ldarg_0, instruction.Opcode),
            instruction =>
            {
                Assert.Equal(OpCodes.Ldfld, instruction.Opcode);
                var field = Assert.IsAssignableFrom<FieldBuilder>(instruction.Operand.Value);
                Assert.Equal("_current", field.Name);
            },
            instruction =>
            {
                Assert.Equal(OpCodes.Box, instruction.Opcode);
                var type = Assert.IsAssignableFrom<Type>(instruction.Operand.Value);
                Assert.Equal(typeof(int).FullName, type.FullName);
            },
            instruction =>
            {
                Assert.Equal(OpCodes.Castclass, instruction.Opcode);
                var type = Assert.IsAssignableFrom<Type>(instruction.Operand.Value);
                Assert.Equal(typeof(object).FullName, type.FullName);
            },
            instruction => Assert.Equal(OpCodes.Ret, instruction.Opcode));
    }

    [Fact]
    public void Dispose_SetsStateToCompleted()
    {
        var (_, instructions) = CaptureIteratorInstructions(IteratorMethodNamed("Dispose"));

        var prefix = instructions.Take(4).ToArray();
        Assert.Equal(4, prefix.Length);

        Assert.Collection(prefix,
            instruction => Assert.Equal(OpCodes.Ldarg_0, instruction.Opcode),
            instruction =>
            {
                Assert.Equal(OpCodes.Ldc_I4, instruction.Opcode);
                Assert.Equal(-1, instruction.Operand.Value);
            },
            instruction =>
            {
                Assert.Equal(OpCodes.Stfld, instruction.Opcode);
                var field = Assert.IsAssignableFrom<FieldBuilder>(instruction.Operand.Value);
                Assert.Equal("_state", field.Name);
            },
            instruction => Assert.Equal(OpCodes.Ret, instruction.Opcode));
    }

    [Fact]
    public void Reset_ThrowsNotSupportedException()
    {
        var (_, instructions) = CaptureIteratorInstructions(IteratorMethodNamed("Reset"));

        var prefix = instructions.Take(3).ToArray();
        Assert.Equal(3, prefix.Length);

        Assert.Collection(prefix,
            instruction =>
            {
                Assert.Equal(OpCodes.Newobj, instruction.Opcode);
                var ctor = Assert.IsAssignableFrom<ConstructorInfo>(instruction.Operand.Value);
                Assert.Equal(typeof(NotSupportedException).FullName, ctor.DeclaringType?.FullName);
            },
            instruction =>
            {
                Assert.Equal(OpCodes.Castclass, instruction.Opcode);
                var type = Assert.IsAssignableFrom<Type>(instruction.Operand.Value);
                Assert.Equal(typeof(Exception).FullName, type.FullName);
            },
            instruction => Assert.Equal(OpCodes.Throw, instruction.Opcode));
    }

    [Fact]
    public void GenericGetEnumerator_ResetsStateAndReturnsSelf()
    {
        var (_, instructions) = CaptureIteratorInstructions(IteratorMethodNamed("GetEnumerator"));

        var prefix = instructions.Take(6).ToArray();
        Assert.Equal(6, prefix.Length);

        Assert.Collection(prefix,
            instruction => Assert.Equal(OpCodes.Ldarg_0, instruction.Opcode),
            instruction =>
            {
                Assert.Equal(OpCodes.Ldc_I4, instruction.Opcode);
                Assert.Equal(0, instruction.Operand.Value);
            },
            instruction =>
            {
                Assert.Equal(OpCodes.Stfld, instruction.Opcode);
                var field = Assert.IsAssignableFrom<FieldBuilder>(instruction.Operand.Value);
                Assert.Equal("_state", field.Name);
            },
            instruction => Assert.Equal(OpCodes.Ldarg_0, instruction.Opcode),
            instruction =>
            {
                Assert.Equal(OpCodes.Castclass, instruction.Opcode);
                var type = Assert.IsAssignableFrom<Type>(instruction.Operand.Value);
                Assert.StartsWith("System.Collections.Generic.IEnumerator`1", type.FullName);
                Assert.Contains("System.Int32", type.FullName);
            },
            instruction => Assert.Equal(OpCodes.Ret, instruction.Opcode));
    }

    [Fact]
    public void NonGenericGetEnumerator_InvokesGenericImplementation()
    {
        var (_, instructions) = CaptureIteratorInstructions(IteratorMethodNamed("System.Collections.IEnumerable.GetEnumerator"));

        var prefix = instructions.Take(4).ToArray();
        Assert.Equal(4, prefix.Length);

        Assert.Collection(prefix,
            instruction => Assert.Equal(OpCodes.Ldarg_0, instruction.Opcode),
            instruction =>
            {
                Assert.Equal(OpCodes.Call, instruction.Opcode);
                var method = Assert.IsAssignableFrom<MethodInfo>(instruction.Operand.Value);
                Assert.Equal("GetEnumerator", method.Name);
            },
            instruction =>
            {
                Assert.Equal(OpCodes.Castclass, instruction.Opcode);
                var type = Assert.IsAssignableFrom<Type>(instruction.Operand.Value);
                Assert.Equal(typeof(IEnumerator).FullName, type.FullName);
            },
            instruction => Assert.Equal(OpCodes.Ret, instruction.Opcode));
    }

    private static Func<MethodGenerator, bool> IteratorMethodNamed(string name)
    {
        return methodGenerator =>
            methodGenerator.MethodSymbol.Name == name &&
            methodGenerator.MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol;
    }

    private static (IMethodSymbol Method, RecordedInstruction[] Instructions) CaptureIteratorInstructions(Func<MethodGenerator, bool> predicate)
    {
        var syntaxTree = SyntaxTree.ParseText(IteratorCode);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        // Force metadata load context initialization so the code generator has a core assembly.
        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var recordingFactory = new RecordingILBuilderFactory(
            ReflectionEmitILBuilderFactory.Instance,
            predicate);

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = recordingFactory
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        var method = recordingFactory.CapturedMethod;
        var instructions = recordingFactory.CapturedInstructions;

        if (method is null)
            throw new InvalidOperationException("Failed to capture iterator method.");

        if (instructions is null)
            throw new InvalidOperationException("Failed to capture iterator method instructions.");

        return (method, instructions.ToArray());
    }

    private sealed class RecordingILBuilderFactory : IILBuilderFactory
    {
        private readonly IILBuilderFactory _inner;
        private readonly Func<MethodGenerator, bool> _predicate;
        private List<RecordedInstruction>? _capturedInstructions;
        private IMethodSymbol? _capturedMethod;

        public RecordingILBuilderFactory(IILBuilderFactory inner, Func<MethodGenerator, bool> predicate)
        {
            _inner = inner;
            _predicate = predicate;
        }

        public IReadOnlyList<RecordedInstruction>? CapturedInstructions => _capturedInstructions;
        public IMethodSymbol? CapturedMethod => _capturedMethod;

        public IILBuilder Create(MethodGenerator methodGenerator)
        {
            var builder = _inner.Create(methodGenerator);

            if (_capturedInstructions is not null || !_predicate(methodGenerator))
                return builder;

            var instructions = new List<RecordedInstruction>();
            _capturedInstructions = instructions;
            _capturedMethod = methodGenerator.MethodSymbol;
            return new RecordingILBuilder(builder, instructions);
        }

        private sealed class RecordingILBuilder : IILBuilder
        {
            private readonly IILBuilder _inner;
            private readonly List<RecordedInstruction> _instructions;
            private readonly Dictionary<ILLabel, int> _labelIds = new();
            private readonly Dictionary<IILocal, int> _localIds = new();
            private int _nextLabelId;
            private int _nextLocalId;

            public RecordingILBuilder(IILBuilder inner, List<RecordedInstruction> instructions)
            {
                _inner = inner;
                _instructions = instructions;
            }

            private void Record(RecordedInstruction instruction) => _instructions.Add(instruction);

            private int GetLabelId(ILLabel label)
            {
                if (!_labelIds.TryGetValue(label, out var id))
                {
                    id = _nextLabelId++;
                    _labelIds[label] = id;
                }

                return id;
            }

            private int GetLocalId(IILocal local)
            {
                if (!_localIds.TryGetValue(local, out var id))
                {
                    id = _nextLocalId++;
                    _localIds[local] = id;
                }

                return id;
            }

            public ILLabel DefineLabel()
            {
                var label = _inner.DefineLabel();
                GetLabelId(label);
                return label;
            }

            public void MarkLabel(ILLabel label)
            {
                GetLabelId(label);
                _inner.MarkLabel(label);
            }

            public IILocal DeclareLocal(Type type)
            {
                var local = _inner.DeclareLocal(type);
                GetLocalId(local);
                return local;
            }

            public void Emit(OpCode opcode)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.None));
                _inner.Emit(opcode);
            }

            public void Emit(OpCode opcode, ILLabel label)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForLabel(GetLabelId(label))));
                _inner.Emit(opcode, label);
            }

            public void Emit(OpCode opcode, IILocal local)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForLocal(GetLocalId(local))));
                _inner.Emit(opcode, local);
            }

            public void Emit(OpCode opcode, int value)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForInt32(value)));
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, long value)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForInt64(value)));
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, float value)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForSingle(value)));
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, double value)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForDouble(value)));
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, string value)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForString(value)));
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, FieldInfo fieldInfo)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForField(fieldInfo)));
                _inner.Emit(opcode, fieldInfo);
            }

            public void Emit(OpCode opcode, FieldBuilder fieldBuilder)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForFieldBuilder(fieldBuilder)));
                _inner.Emit(opcode, fieldBuilder);
            }

            public void Emit(OpCode opcode, MethodInfo methodInfo)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForMethod(methodInfo)));
                _inner.Emit(opcode, methodInfo);
            }

            public void Emit(OpCode opcode, ConstructorInfo constructorInfo)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForConstructor(constructorInfo)));
                _inner.Emit(opcode, constructorInfo);
            }

            public void Emit(OpCode opcode, Type type)
            {
                Record(new RecordedInstruction(opcode, RecordedOperand.ForType(type)));
                _inner.Emit(opcode, type);
            }

            public void BeginExceptionBlock() => _inner.BeginExceptionBlock();
            public void BeginCatchBlock(Type exceptionType) => _inner.BeginCatchBlock(exceptionType);
            public void BeginFinallyBlock() => _inner.BeginFinallyBlock();
            public void EndExceptionBlock() => _inner.EndExceptionBlock();
        }
    }

    internal readonly record struct RecordedInstruction(OpCode Opcode, RecordedOperand Operand);

    internal readonly record struct RecordedOperand(RecordedOperandKind Kind, object? Value)
    {
        public static RecordedOperand None { get; } = new RecordedOperand(RecordedOperandKind.None, null);
        public static RecordedOperand ForLabel(int id) => new RecordedOperand(RecordedOperandKind.Label, id);
        public static RecordedOperand ForLocal(int id) => new RecordedOperand(RecordedOperandKind.Local, id);
        public static RecordedOperand ForInt32(int value) => new RecordedOperand(RecordedOperandKind.Int32, value);
        public static RecordedOperand ForInt64(long value) => new RecordedOperand(RecordedOperandKind.Int64, value);
        public static RecordedOperand ForSingle(float value) => new RecordedOperand(RecordedOperandKind.Single, value);
        public static RecordedOperand ForDouble(double value) => new RecordedOperand(RecordedOperandKind.Double, value);
        public static RecordedOperand ForString(string value) => new RecordedOperand(RecordedOperandKind.String, value);
        public static RecordedOperand ForField(FieldInfo value) => new RecordedOperand(RecordedOperandKind.FieldInfo, value);
        public static RecordedOperand ForFieldBuilder(FieldBuilder value) => new RecordedOperand(RecordedOperandKind.FieldBuilder, value);
        public static RecordedOperand ForMethod(MethodInfo value) => new RecordedOperand(RecordedOperandKind.MethodInfo, value);
        public static RecordedOperand ForConstructor(ConstructorInfo value) => new RecordedOperand(RecordedOperandKind.ConstructorInfo, value);
        public static RecordedOperand ForType(Type value) => new RecordedOperand(RecordedOperandKind.Type, value);
    }

    internal enum RecordedOperandKind
    {
        None,
        Label,
        Local,
        Int32,
        Int64,
        Single,
        Double,
        String,
        FieldInfo,
        FieldBuilder,
        MethodInfo,
        ConstructorInfo,
        Type,
    }

    private static class RecordedInstructionFormatter
    {
        public static string Format(RecordedInstruction instruction)
        {
            var opcode = instruction.Opcode.ToString()!.ToLowerInvariant();

            return instruction.Operand.Kind switch
            {
                RecordedOperandKind.None => opcode,
                RecordedOperandKind.Label => instruction.Operand.Value is int labelId
                    ? $"{opcode} L{labelId}"
                    : opcode,
                RecordedOperandKind.Local => instruction.Operand.Value is int localId
                    ? $"{opcode}.{localId}"
                    : opcode,
                RecordedOperandKind.Int32 => instruction.Operand.Value is int i32
                    ? $"{opcode} {i32}"
                    : opcode,
                RecordedOperandKind.Int64 => instruction.Operand.Value is long i64
                    ? $"{opcode} {i64}"
                    : opcode,
                RecordedOperandKind.Single => instruction.Operand.Value is float f32
                    ? $"{opcode} {f32}"
                    : opcode,
                RecordedOperandKind.Double => instruction.Operand.Value is double f64
                    ? $"{opcode} {f64}"
                    : opcode,
                RecordedOperandKind.String => instruction.Operand.Value is string s
                    ? $"{opcode} \"{s}\""
                    : opcode,
                RecordedOperandKind.FieldInfo => instruction.Operand.Value is FieldInfo fieldInfo
                    ? $"{opcode} {FormatField(fieldInfo)}"
                    : opcode,
                RecordedOperandKind.FieldBuilder => instruction.Operand.Value is FieldBuilder fieldBuilder
                    ? $"{opcode} {FormatFieldBuilder(fieldBuilder)}"
                    : opcode,
                RecordedOperandKind.MethodInfo => instruction.Operand.Value is MethodInfo method
                    ? $"{opcode} {FormatMethod(method)}"
                    : opcode,
                RecordedOperandKind.ConstructorInfo => instruction.Operand.Value is ConstructorInfo ctor
                    ? $"{opcode} {FormatConstructor(ctor)}"
                    : opcode,
                RecordedOperandKind.Type => instruction.Operand.Value is Type type
                    ? $"{opcode} {type.FullName}"
                    : opcode,
                _ => opcode,
            };
        }

        private static string FormatField(FieldInfo field)
        {
            var typeName = field.DeclaringType?.Name ?? field.DeclaringType?.FullName ?? field.ReflectedType?.Name ?? "<unknown>";
            return $"{typeName}.{field.Name}";
        }

        private static string FormatFieldBuilder(FieldBuilder field)
        {
            var typeName = field.DeclaringType?.Name ?? field.DeclaringType?.FullName ?? "<unknown>";
            return $"{typeName}.{field.Name}";
        }

        private static string FormatMethod(MethodInfo method)
        {
            var typeName = method.DeclaringType?.Name ?? method.DeclaringType?.FullName ?? "<unknown>";
            return $"{typeName}.{method.Name}";
        }

        private static string FormatConstructor(ConstructorInfo ctor)
        {
            var typeName = ctor.DeclaringType?.Name ?? ctor.DeclaringType?.FullName ?? "<unknown>";
            return $"{typeName}.ctor";
        }
    }
}
