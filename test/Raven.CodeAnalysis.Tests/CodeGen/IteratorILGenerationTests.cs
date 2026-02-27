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
    func Values() -> IEnumerable<int> {
        yield return 42
        var i = 0
        while i < 2 {
            yield return i
            i = i + 1
        }
    }
}
""";

    private const string AsyncIteratorCode = """
import System.Collections.Generic.*

class C {
    async func Values() -> IAsyncEnumerable<int> {
        yield return 42
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
            .Select(static text => text.Replace("C+<>c__Iterator0.", "<>c__Iterator0.", StringComparison.Ordinal))
            .ToArray();

        Assert.DoesNotContain(instructions, instruction => instruction.Opcode == OpCodes.Pop);
        Assert.Contains(formatted, op => op.Contains("stfld <>c__Iterator0._state", StringComparison.Ordinal));
        Assert.Contains(formatted, op => op.Contains("stfld <>c__Iterator0._current", StringComparison.Ordinal));
        Assert.Contains(formatted, op => op == "ret");
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
                Assert.True(instruction.Opcode == OpCodes.Call || instruction.Opcode == OpCodes.Callvirt);
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

    [Fact]
    public void AsyncMoveNextAsync_WrapsMoveNextResultInValueTaskOfBool()
    {
        var (_, instructions) = CaptureIteratorInstructions(
            IteratorMethodNamed("MoveNextAsync"),
            AsyncIteratorCode);

        Assert.Contains(instructions, instruction =>
            (instruction.Opcode == OpCodes.Call || instruction.Opcode == OpCodes.Callvirt) &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name == "MoveNext");
        Assert.Contains(instructions, instruction => instruction.Opcode == OpCodes.Newobj && instruction.Operand.Value is ConstructorInfo ctor && ctor.DeclaringType?.Name.StartsWith("ValueTask", StringComparison.Ordinal) == true);
        Assert.Equal(OpCodes.Ret, instructions[^1].Opcode);
    }

    private static Func<MethodGenerator, bool> IteratorMethodNamed(string name)
    {
        return methodGenerator =>
            methodGenerator.MethodSymbol.Name == name &&
            methodGenerator.MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol;
    }

    private static (IMethodSymbol Method, RecordedInstruction[] Instructions) CaptureIteratorInstructions(Func<MethodGenerator, bool> predicate, string source = IteratorCode)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
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

        return (method, TrimLeadingNops(instructions));
    }

    private static RecordedInstruction[] TrimLeadingNops(IReadOnlyList<RecordedInstruction> instructions)
    {
        var start = 0;
        while (start < instructions.Count && instructions[start].Opcode == OpCodes.Nop)
            start++;

        if (start == 0)
            return instructions.ToArray();

        var trimmed = new RecordedInstruction[instructions.Count - start];
        for (var i = start; i < instructions.Count; i++)
            trimmed[i - start] = instructions[i];

        return trimmed;
    }

}
