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

}
