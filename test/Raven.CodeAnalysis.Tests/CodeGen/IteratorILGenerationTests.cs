using System;
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

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class IteratorILGenerationTests
{
    [Fact]
    public void MoveNext_DoesNotEmitStackClearingPops()
    {
        var code = """
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

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        var recordingFactory = new RecordingILBuilderFactory(
            ReflectionEmitILBuilderFactory.Instance,
            methodGenerator => methodGenerator.MethodSymbol.Name == "MoveNext" &&
                methodGenerator.MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol);

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = recordingFactory
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        Assert.NotNull(recordingFactory.CapturedMethod);
        Assert.NotNull(recordingFactory.CapturedInstructions);
        Assert.DoesNotContain(recordingFactory.CapturedInstructions!, opcode => opcode == OpCodes.Pop);
    }

    private sealed class RecordingILBuilderFactory : IILBuilderFactory
    {
        private readonly IILBuilderFactory _inner;
        private readonly Func<MethodGenerator, bool> _predicate;
        private List<OpCode>? _capturedInstructions;
        private IMethodSymbol? _capturedMethod;

        public RecordingILBuilderFactory(IILBuilderFactory inner, Func<MethodGenerator, bool> predicate)
        {
            _inner = inner;
            _predicate = predicate;
        }

        public IReadOnlyList<OpCode>? CapturedInstructions => _capturedInstructions;
        public IMethodSymbol? CapturedMethod => _capturedMethod;

        public IILBuilder Create(MethodGenerator methodGenerator)
        {
            var builder = _inner.Create(methodGenerator);

            if (_capturedInstructions is not null || !_predicate(methodGenerator))
                return builder;

            var instructions = new List<OpCode>();
            _capturedInstructions = instructions;
            _capturedMethod = methodGenerator.MethodSymbol;
            return new RecordingILBuilder(builder, instructions);
        }

        private sealed class RecordingILBuilder : IILBuilder
        {
            private readonly IILBuilder _inner;
            private readonly List<OpCode> _instructions;

            public RecordingILBuilder(IILBuilder inner, List<OpCode> instructions)
            {
                _inner = inner;
                _instructions = instructions;
            }

            private void Record(OpCode opcode) => _instructions.Add(opcode);

            public ILLabel DefineLabel() => _inner.DefineLabel();
            public void MarkLabel(ILLabel label) => _inner.MarkLabel(label);
            public IILocal DeclareLocal(Type type) => _inner.DeclareLocal(type);

            public void Emit(OpCode opcode)
            {
                Record(opcode);
                _inner.Emit(opcode);
            }

            public void Emit(OpCode opcode, ILLabel label)
            {
                Record(opcode);
                _inner.Emit(opcode, label);
            }

            public void Emit(OpCode opcode, IILocal local)
            {
                Record(opcode);
                _inner.Emit(opcode, local);
            }

            public void Emit(OpCode opcode, int value)
            {
                Record(opcode);
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, long value)
            {
                Record(opcode);
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, float value)
            {
                Record(opcode);
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, double value)
            {
                Record(opcode);
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, string value)
            {
                Record(opcode);
                _inner.Emit(opcode, value);
            }

            public void Emit(OpCode opcode, FieldInfo fieldInfo)
            {
                Record(opcode);
                _inner.Emit(opcode, fieldInfo);
            }

            public void Emit(OpCode opcode, FieldBuilder fieldBuilder)
            {
                Record(opcode);
                _inner.Emit(opcode, fieldBuilder);
            }

            public void Emit(OpCode opcode, MethodInfo methodInfo)
            {
                Record(opcode);
                _inner.Emit(opcode, methodInfo);
            }

            public void Emit(OpCode opcode, ConstructorInfo constructorInfo)
            {
                Record(opcode);
                _inner.Emit(opcode, constructorInfo);
            }

            public void Emit(OpCode opcode, Type type)
            {
                Record(opcode);
                _inner.Emit(opcode, type);
            }

            public void BeginExceptionBlock() => _inner.BeginExceptionBlock();
            public void BeginCatchBlock(Type exceptionType) => _inner.BeginCatchBlock(exceptionType);
            public void BeginFinallyBlock() => _inner.BeginFinallyBlock();
            public void EndExceptionBlock() => _inner.EndExceptionBlock();
        }
    }
}
