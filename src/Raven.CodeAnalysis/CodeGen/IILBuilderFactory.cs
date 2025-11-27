using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen;

internal interface IILBuilderFactory
{
    IILBuilder Create(MethodGenerator methodGenerator);
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

        return new ReflectionEmitILBuilder(ilGenerator, methodGenerator.MethodSymbol.ToDisplayString());
    }

        private sealed class ReflectionEmitILBuilder : IILBuilder
        {
            private readonly ILGenerator _inner;
            private readonly string _methodName;
            private readonly HashSet<LabelAdapter> _definedLabels = new();
            private readonly HashSet<LabelAdapter> _markedLabels = new();
            private int _nextLabelId;

            public ReflectionEmitILBuilder(ILGenerator inner, string methodName)
            {
                _inner = inner;
                _methodName = methodName;
            }

            public ILLabel DefineLabel()
            {
                var originFrame = new StackTrace(1, true).GetFrame(0);
                var originMethod = originFrame?.GetMethod();
                var origin = originMethod is null
                    ? string.Empty
                    : $"{originMethod.DeclaringType?.FullName ?? originMethod.DeclaringType?.Name}.{originMethod.Name}:{originFrame?.GetFileLineNumber()}";

                var adapter = new LabelAdapter(_inner.DefineLabel(), _nextLabelId++, origin ?? string.Empty);
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

            public void ValidateLabels()
            {
                var unmarked = _definedLabels.Where(label => !_markedLabels.Contains(label)).ToArray();
                if (unmarked.Length == 0)
                    return;

                var details = string.Join(", ", unmarked.Select(l => $"{l.Id}@{l.Origin}"));
                throw new InvalidOperationException($"Unmarked labels ({details}) emitted for '{_methodName}'.");
            }

            public IReadOnlyCollection<ILLabel> GetUnmarkedLabels()
            {
                return _definedLabels.Where(label => !_markedLabels.Contains(label)).Cast<ILLabel>().ToArray();
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
                public LabelAdapter(Label label, int id, string origin)
                {
                    Label = label;
                    Id = id;
                    Origin = origin;
                }

                public Label Label { get; }
                public int Id { get; }
                public string Origin { get; }
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
