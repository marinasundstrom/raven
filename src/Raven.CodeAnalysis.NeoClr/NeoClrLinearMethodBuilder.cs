using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

// Native handles stay in this adapter. Symbol-to-native call mapping is owned by the
// enclosing assembly emission, including explicit dependency and System bindings.
internal sealed class NeoClrLinearMethodBuilder(MethodBuilder method,
    Action<LinearInstruction, MethodBuilder> emitCall, Func<IFieldSymbol, NeoClrFieldReference>? resolveField = null, Func<INamedTypeSymbol, TypeBuilder>? resolveType = null) : ILinearMethodBuilder
{
    private readonly List<BranchLabel> labels = [];
    public void DefineLabel() => labels.Add(method.DefineLabel());

    public void DeclareLocal(EmissionType type) => method.DeclareLocal(NeoClrTypeMapper.Map(type, resolveType!));

    public void Emit(LinearInstruction instruction)
    {
        switch (instruction.Kind)
        {
            case LinearInstructionKind.DefaultValue: method.LoadDefault(NeoClrTypeMapper.Map(instruction.Type!, resolveType!)); break;
            case LinearInstructionKind.Duplicate: method.Duplicate(); break;
            case LinearInstructionKind.NewArray: method.NewArray(NeoClrTypeMapper.Map(instruction.Type!, resolveType!)); break;
            case LinearInstructionKind.LoadElement: method.LoadArrayElement(NeoClrTypeMapper.Map(instruction.Type!, resolveType!)); break;
            case LinearInstructionKind.StoreElement: method.StoreArrayElement(NeoClrTypeMapper.Map(instruction.Type!, resolveType!)); break;
            case LinearInstructionKind.ArrayLength: method.LoadArrayLength(); break;
            case LinearInstructionKind.Receiver: method.LoadArgument(0); break;
            case LinearInstructionKind.LoadField: resolveField!(instruction.Field!).Emit(method, false); break;
            case LinearInstructionKind.StoreField: resolveField!(instruction.Field!).Emit(method, true); break;
            case LinearInstructionKind.NewObject:
            case LinearInstructionKind.InstanceCall: emitCall(instruction, method); break;
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
            case LinearInstructionKind.Divide: method.Emit(OpCode.Div); break;
            case LinearInstructionKind.Remainder: method.Emit(OpCode.Rem); break;
            case LinearInstructionKind.BitwiseAnd: method.Emit(OpCode.And); break;
            case LinearInstructionKind.BitwiseOr: method.Emit(OpCode.Or); break;
            case LinearInstructionKind.BitwiseXor: method.Emit(OpCode.Xor); break;
            case LinearInstructionKind.ShiftLeft: method.Emit(OpCode.Shl); break;
            case LinearInstructionKind.ShiftRight: method.Emit(OpCode.Shr); break;
            case LinearInstructionKind.Call: emitCall(instruction, method); break;
            case LinearInstructionKind.String: method.Emit(OpCode.Ldstr, instruction.Text!); break;
            case LinearInstructionKind.ConsoleWrite: method.WriteConsoleLine(); break;
            case LinearInstructionKind.Return: method.Emit(OpCode.Ret); break;
            default: throw new InvalidOperationException("Unsupported native linear instruction: " + instruction.Kind);
        }
    }
}
