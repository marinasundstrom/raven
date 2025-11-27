using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen;

internal interface IILBuilder
{
    ILLabel DefineLabel();
    void MarkLabel(ILLabel label);

    IILocal DeclareLocal(Type type);

    void Emit(OpCode opcode);
    void Emit(OpCode opcode, ILLabel label);
    void Emit(OpCode opcode, IILocal local);
    void Emit(OpCode opcode, int value);
    void Emit(OpCode opcode, long value);
    void Emit(OpCode opcode, float value);
    void Emit(OpCode opcode, double value);
    void Emit(OpCode opcode, string value);
    void Emit(OpCode opcode, FieldInfo fieldInfo);
    void Emit(OpCode opcode, FieldBuilder fieldBuilder);
    void Emit(OpCode opcode, MethodInfo methodInfo);
    void Emit(OpCode opcode, ConstructorInfo constructorInfo);
    void Emit(OpCode opcode, Type type);

    void BeginExceptionBlock();
    void BeginCatchBlock(Type exceptionType);
    void BeginFinallyBlock();
    void EndExceptionBlock();

    void ValidateLabels();

    IReadOnlyCollection<ILLabel> GetUnmarkedLabels();
}

internal interface IILocal
{
    void SetLocalSymInfo(string name);
}

internal interface ILLabel
{
}
