using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

// Native handles stay in this adapter. Symbol-to-native call mapping is owned by the
// enclosing assembly emission, including explicit dependency and System bindings.
internal sealed class NeoClrLinearMethodBuilder(IILGenerator method,
    Action<LinearInstruction, IILGenerator> emitCall, Func<IFieldSymbol, NeoClrFieldReference>? resolveField = null, Func<INamedTypeSymbol, TypeBuilder>? resolveType = null, Func<ITypeSymbol, SignatureType>? resolveExternal = null, Func<ITypeParameterSymbol, SignatureType>? mapParameter = null, Action<IMethodSymbol>? declareCompositionLocal = null) : ILinearMethodBuilder
{
    private readonly List<BranchLabel> labels = [];
    public void DefineLabel() => labels.Add(method.DefineLabel());

    public void DeclareLocal(EmissionType type)
    {
        if (type.CompositionProducer is { } producer)
        {
            if (declareCompositionLocal is null) throw new InvalidOperationException("Composition local allocation is unavailable");
            declareCompositionLocal(producer);
        }
        else method.DeclareLocal(NeoClrTypeMapper.Map(type, resolveType!, resolveExternal, mapParameter));
    }

    public void Emit(LinearInstruction instruction)
    {
        switch (instruction.Kind)
        {
            case LinearInstructionKind.NewTuple: method.NewTuple(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.StoreTupleElement: method.StoreTupleElement(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter), instruction.Integer); break;
            case LinearInstructionKind.LoadTupleElement: method.LoadTupleElement(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter), instruction.Integer); break;
            case LinearInstructionKind.LoadTypeToken: method.LoadTypeToken(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.EnumFromInt32: method.ConvertToEnum(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.EnumToInt32: method.ConvertFromEnum(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.ReferenceIsNull: method.IsNull(); break;
            case LinearInstructionKind.TypeTest:
                method.IsInstance(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter));
                method.IsNull(); method.Emit(OpCode.Ldc_Bool, false); method.Emit(OpCode.Ceq); break;
            case LinearInstructionKind.UnboxAny: method.UnboxAny(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.BoxToObject: method.Box(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.ReferenceConvert: method.CastReference(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.LoadCapture:
            case LinearInstructionKind.FunctionBind: emitCall(instruction, method); break;
            case LinearInstructionKind.FunctionInvoke: method.InvokeFunction(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.ArgumentAddress: method.LoadArgumentAddress(instruction.Integer); break;
            case LinearInstructionKind.LocalAddress: method.Emit(OpCode.Ldloca, instruction.Integer); break;
            case LinearInstructionKind.LoadIndirect: method.LoadObject(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.StoreIndirect: method.StoreObject(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.DefaultValue: method.LoadDefault(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.Duplicate: method.Duplicate(); break;
            case LinearInstructionKind.NewArray: method.NewArray(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.ElementAddress: method.Emit(OpCode.Ldelema, NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.LoadElement: method.LoadArrayElement(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.StoreElement: method.StoreArrayElement(NeoClrTypeMapper.Map(instruction.Type!, resolveType!, resolveExternal, mapParameter)); break;
            case LinearInstructionKind.ArrayLength: method.LoadArrayLength(); break;
            case LinearInstructionKind.Receiver: method.LoadArgument(0); break;
            case LinearInstructionKind.FieldAddress: resolveField!(instruction.Field!).EmitAddress(method); break;
            case LinearInstructionKind.LoadField: resolveField!(instruction.Field!).Emit(method, false); break;
            case LinearInstructionKind.StoreField: resolveField!(instruction.Field!).Emit(method, true); break;
            case LinearInstructionKind.DirectInstanceCall:
            case LinearInstructionKind.BaseConstructorCall:
            case LinearInstructionKind.NewObject:
            case LinearInstructionKind.InterfaceCall:
            case LinearInstructionKind.ValueInstanceCall:
            case LinearInstructionKind.InstanceCall: emitCall(instruction, method); break;
            case LinearInstructionKind.ConstantSingle: method.Emit(OpCode.Ldc_R4, BitConverter.Int32BitsToSingle(instruction.Integer)); break;
            case LinearInstructionKind.ConstantDouble: method.Emit(OpCode.Ldc_R8, BitConverter.Int64BitsToDouble(instruction.Long)); break;
            case LinearInstructionKind.ConvertSingle: method.Emit(OpCode.Conv_R4); break;
            case LinearInstructionKind.ConvertDouble: method.Emit(OpCode.Conv_R8); break;
            case LinearInstructionKind.LessOrUnordered: method.Emit(OpCode.Clt_Un); break;
            case LinearInstructionKind.GreaterOrUnordered: method.Emit(OpCode.Cgt_Un); break;
            case LinearInstructionKind.Constant64: method.Emit(OpCode.Ldc_I8, instruction.Long); break;
            case LinearInstructionKind.Convert64: method.Emit(OpCode.Conv_I8); break;
            case LinearInstructionKind.ConvertSByte: method.Emit(OpCode.Conv_I1); break;
            case LinearInstructionKind.ConvertInt16: method.Emit(OpCode.Conv_I2); break;
            case LinearInstructionKind.ConvertUInt16: method.Emit(OpCode.Conv_U2); break;
            case LinearInstructionKind.ConvertUInt32: method.Emit(OpCode.Conv_U4); break;
            case LinearInstructionKind.ConvertUInt64: method.Emit(OpCode.Conv_U8); break;
            case LinearInstructionKind.ConvertNativeInt: method.Emit(OpCode.Conv_I); break;
            case LinearInstructionKind.ConvertNativeUInt: method.Emit(OpCode.Conv_U); break;
            case LinearInstructionKind.UnsignedDivide: method.Emit(OpCode.Div_Un); break;
            case LinearInstructionKind.UnsignedRemainder: method.Emit(OpCode.Rem_Un); break;
            case LinearInstructionKind.UnsignedShiftRight: method.Emit(OpCode.Shr_Un); break;
            case LinearInstructionKind.UnsignedLess: method.Emit(OpCode.Clt_Un); break;
            case LinearInstructionKind.UnsignedGreater: method.Emit(OpCode.Cgt_Un); break;
            case LinearInstructionKind.UnsignedConvertDouble: method.Emit(OpCode.Conv_R_Un); break;
            case LinearInstructionKind.ConvertByte: method.Emit(OpCode.Conv_U1); break;
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
            case LinearInstructionKind.CheckedConvert: method.Emit(CheckedConversionCode((SpecialType)instruction.Integer, instruction.Type!.SpecialType)); break;
            case LinearInstructionKind.CheckedAdd: method.Emit(OpCode.Add_Ovf); break;
            case LinearInstructionKind.UnsignedCheckedAdd: method.Emit(OpCode.Add_Ovf_Un); break;
            case LinearInstructionKind.CheckedSubtract: method.Emit(OpCode.Sub_Ovf); break;
            case LinearInstructionKind.UnsignedCheckedSubtract: method.Emit(OpCode.Sub_Ovf_Un); break;
            case LinearInstructionKind.CheckedMultiply: method.Emit(OpCode.Mul_Ovf); break;
            case LinearInstructionKind.UnsignedCheckedMultiply: method.Emit(OpCode.Mul_Ovf_Un); break;
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
            case LinearInstructionKind.ConstrainedCall:
            case LinearInstructionKind.Call: emitCall(instruction, method); break;
            case LinearInstructionKind.String: method.Emit(OpCode.Ldstr, instruction.Text!); break;
            case LinearInstructionKind.ConsoleWrite: method.WriteConsoleLine(); break;
            case LinearInstructionKind.CompilerFailure: method.Fail(instruction.Text!); break;
            case LinearInstructionKind.Return: method.Emit(OpCode.Ret); break;
            default: throw new InvalidOperationException("Unsupported native linear instruction: " + instruction.Kind);
        }
    }

    private static OpCode CheckedConversionCode(SpecialType source, SpecialType target)
    {
        var unsigned = source is SpecialType.System_Byte or SpecialType.System_UInt16
            or SpecialType.System_UInt32 or SpecialType.System_UInt64 or SpecialType.System_UIntPtr;
        return target switch
        {
            SpecialType.System_SByte => unsigned ? OpCode.Conv_Ovf_I1_Un : OpCode.Conv_Ovf_I1,
            SpecialType.System_Byte => unsigned ? OpCode.Conv_Ovf_U1_Un : OpCode.Conv_Ovf_U1,
            SpecialType.System_Int16 => unsigned ? OpCode.Conv_Ovf_I2_Un : OpCode.Conv_Ovf_I2,
            SpecialType.System_UInt16 => unsigned ? OpCode.Conv_Ovf_U2_Un : OpCode.Conv_Ovf_U2,
            SpecialType.System_Int32 => unsigned ? OpCode.Conv_Ovf_I4_Un : OpCode.Conv_Ovf_I4,
            SpecialType.System_UInt32 => unsigned ? OpCode.Conv_Ovf_U4_Un : OpCode.Conv_Ovf_U4,
            SpecialType.System_Int64 => unsigned ? OpCode.Conv_Ovf_I8_Un : OpCode.Conv_Ovf_I8,
            SpecialType.System_UInt64 => unsigned ? OpCode.Conv_Ovf_U8_Un : OpCode.Conv_Ovf_U8,
            SpecialType.System_IntPtr => unsigned ? OpCode.Conv_Ovf_I_Un : OpCode.Conv_Ovf_I,
            SpecialType.System_UIntPtr => unsigned ? OpCode.Conv_Ovf_U_Un : OpCode.Conv_Ovf_U,
            _ => throw new InvalidOperationException("unsupported checked numeric conversion")
        };
    }

}
