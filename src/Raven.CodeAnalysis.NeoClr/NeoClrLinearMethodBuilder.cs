using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

// Native handles stay in this adapter. Symbol-to-native call mapping is owned by the
// enclosing assembly emission, including explicit dependency and System bindings.
internal sealed class NeoClrLinearMethodBuilder(MethodBuilder method,
    Action<LinearInstruction, MethodBuilder> emitCall) : ILinearMethodBuilder
{
    private readonly List<BranchLabel> labels = [];
    public void DefineLabel() => labels.Add(method.DefineLabel());

    public void DeclareLocal(SpecialType type) => method.DeclareLocal(NeoClrCallableDefinitionBuilder.Map(type));

    public void Emit(LinearInstruction instruction)
    {
        switch (instruction.Kind)
        {
            case LinearInstructionKind.Constant64: method.Emit(OpCode.Ldc_I8, instruction.Long); break;
            case LinearInstructionKind.Convert64: method.Emit(OpCode.Conv_I8); break;
            case LinearInstructionKind.Convert32: method.Emit(OpCode.Conv_I4); break;
            case LinearInstructionKind.Negate: method.Emit(OpCode.Neg); break;
            case LinearInstructionKind.Complement: method.Emit(OpCode.Not); break;
            case LinearInstructionKind.Pop: method.Emit(OpCode.Pop); break;
            case LinearInstructionKind.Not: method.Emit(OpCode.Ldc_Bool, false); method.Emit(OpCode.Ceq); break;
            case LinearInstructionKind.Boolean: method.Emit(OpCode.Ldc_Bool, instruction.Integer != 0); break;
            case LinearInstructionKind.Equal: method.Emit(OpCode.Ceq); break;
            case LinearInstructionKind.Less: method.Emit(OpCode.Clt); break;
            case LinearInstructionKind.Greater: method.Emit(OpCode.Cgt); break;
            case LinearInstructionKind.Label: method.MarkLabel(labels[instruction.Integer]); break;
            case LinearInstructionKind.Branch: method.Emit(OpCode.Br, labels[instruction.Integer]); break;
            case LinearInstructionKind.BranchTrue: method.Emit(OpCode.Brtrue, labels[instruction.Integer]); break;
            case LinearInstructionKind.BranchFalse: method.Emit(OpCode.Brfalse, labels[instruction.Integer]); break;
            case LinearInstructionKind.Constant: method.Emit(OpCode.Ldc_I4, instruction.Integer); break;
            case LinearInstructionKind.Argument: method.Emit(OpCode.Ldarg, instruction.Integer); break;
            case LinearInstructionKind.LoadLocal: method.Emit(OpCode.Ldloc, instruction.Integer); break;
            case LinearInstructionKind.StoreLocal: method.Emit(OpCode.Stloc, instruction.Integer); break;
            case LinearInstructionKind.Add: method.Emit(OpCode.Add); break;
            case LinearInstructionKind.Subtract: method.Emit(OpCode.Sub); break;
            case LinearInstructionKind.Multiply: method.Emit(OpCode.Mul); break;
            case LinearInstructionKind.Call: emitCall(instruction, method); break;
            case LinearInstructionKind.ConsoleLiteral: method.WriteConsoleLine(instruction.Text!); break;
            case LinearInstructionKind.Return: method.Emit(OpCode.Ret); break;
            default: throw new InvalidOperationException("Unsupported native linear instruction: " + instruction.Kind);
        }
    }
}
