using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

// Native handles stay in this adapter. Symbol-to-native call mapping is owned by the
// enclosing assembly emission, including explicit dependency and System bindings.
internal sealed class NeoClrLinearMethodBuilder(MethodBuilder method,
    Action<LinearInstruction, MethodBuilder> emitCall) : ILinearMethodBuilder
{
    public void Emit(LinearInstruction instruction)
    {
        switch (instruction.Kind)
        {
            case LinearInstructionKind.Constant: method.Emit(OpCode.Ldc_I4, instruction.Integer); break;
            case LinearInstructionKind.Argument: method.Emit(OpCode.Ldarg, instruction.Integer); break;
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
