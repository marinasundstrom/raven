using System;
using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen;

internal interface IILBuilderFactory
{
    IILBuilder Create(MethodGenerator methodGenerator);
}

internal interface ILabelTrackingILBuilder
{
    void MarkAllLabels();
}

internal sealed class ReflectionEmitILBuilderFactory : IILBuilderFactory
{
    public static ReflectionEmitILBuilderFactory Instance { get; } = new();

    private ReflectionEmitILBuilderFactory()
    {
    }

    public IILBuilder Create(MethodGenerator methodGenerator)
    {
        var methodBase = methodGenerator.MethodBase;
        var ilGenerator = methodBase switch
        {
            MethodBuilder methodBuilder => methodBuilder.GetILGenerator(),
            ConstructorBuilder constructorBuilder => constructorBuilder.GetILGenerator(),
            _ => throw new InvalidOperationException($"Unsupported method base type: {methodBase?.GetType()}")
        };

        return new ReflectionEmitILBuilder(ilGenerator);
    }

    private sealed class ReflectionEmitILBuilder : IILBuilder, ILabelTrackingILBuilder
    {
        private readonly ILGenerator _inner;
        private readonly List<LabelAdapter> _definedLabels = new();
        private readonly HashSet<LabelAdapter> _markedLabels = new();

        public ReflectionEmitILBuilder(ILGenerator inner)
        {
            _inner = inner;
        }

        public ILLabel DefineLabel()
        {
            var adapter = new LabelAdapter(_inner.DefineLabel());
            _definedLabels.Add(adapter);
            return adapter;
        }

        public void MarkLabel(ILLabel label)
        {
            var adapter = Unwrap(label);
            _markedLabels.Add(adapter);
            _inner.MarkLabel(adapter.Label);
        }

        public IILocal DeclareLocal(Type type) => new LocalAdapter(_inner.DeclareLocal(type));

        public void Emit(OpCode opcode) => _inner.Emit(opcode);

        public void Emit(OpCode opcode, ILLabel label) => _inner.Emit(opcode, Unwrap(label).Label);

        public void Emit(OpCode opcode, IILocal local) => _inner.Emit(opcode, Unwrap(local));

        public void Emit(OpCode opcode, int value) => _inner.Emit(opcode, value);

        public void Emit(OpCode opcode, long value) => _inner.Emit(opcode, value);

        public void Emit(OpCode opcode, float value) => _inner.Emit(opcode, value);

        public void Emit(OpCode opcode, double value) => _inner.Emit(opcode, value);

        public void Emit(OpCode opcode, string value) => _inner.Emit(opcode, value);

        public void Emit(OpCode opcode, FieldInfo fieldInfo) => _inner.Emit(opcode, fieldInfo);

        public void Emit(OpCode opcode, FieldBuilder fieldBuilder) => _inner.Emit(opcode, fieldBuilder);

        public void Emit(OpCode opcode, MethodInfo methodInfo) => _inner.Emit(opcode, methodInfo);

        public void Emit(OpCode opcode, ConstructorInfo constructorInfo) => _inner.Emit(opcode, constructorInfo);

        public void Emit(OpCode opcode, Type type) => _inner.Emit(opcode, type);

        public void BeginExceptionBlock() => _inner.BeginExceptionBlock();

        public void BeginCatchBlock(Type exceptionType) => _inner.BeginCatchBlock(exceptionType);

        public void BeginFinallyBlock() => _inner.BeginFinallyBlock();

        public void EndExceptionBlock() => _inner.EndExceptionBlock();

        public void MarkAllLabels()
        {
            foreach (var label in _definedLabels)
            {
                if (_markedLabels.Contains(label))
                    continue;

                _inner.MarkLabel(label.Label);
            }
        }

        private static LabelAdapter Unwrap(ILLabel label)
        {
            if (label is LabelAdapter adapter)
                return adapter;

            throw new InvalidOperationException("Label was not created by this IL builder.");
        }

        private static LocalBuilder Unwrap(IILocal local)
        {
            if (local is LocalAdapter adapter)
                return adapter.Builder;

            throw new InvalidOperationException("Local was not created by this IL builder.");
        }

        private sealed class LabelAdapter : ILLabel
        {
            public LabelAdapter(Label label)
            {
                Label = label;
            }

            public Label Label { get; }
        }

        private sealed class LocalAdapter : IILocal
        {
            public LocalAdapter(LocalBuilder builder)
            {
                Builder = builder;
            }

            public LocalBuilder Builder { get; }

            public void SetLocalSymInfo(string name) => Builder.SetLocalSymInfo(name);
        }
    }
}
