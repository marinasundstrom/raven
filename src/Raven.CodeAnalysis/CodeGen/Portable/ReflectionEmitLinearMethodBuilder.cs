using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Adapts the existing .NET method builder; declaration creation, attributes, signature
// mapping, type completion and PE writing remain owned by the existing code generator.
internal sealed class ReflectionEmitLinearMethodBuilder(MethodGenerator method, IILBuilder output) : ILinearMethodBuilder
{
    private readonly List<IILocal> locals = [];
    private readonly List<ILLabel> labels = [];

    public void DefineLabel() => labels.Add(output.DefineLabel());

    private readonly IEmissionTypeMapper<Type> types = new ReflectionEmitTypeMapper(
        type => method.ResolveClrType(method.Compilation.GetSpecialType(type)));

    public void DeclareLocal(INamedTypeSymbol type) => locals.Add(output.DeclareLocal(method.ResolveClrType(type)));

    public void DeclareLocal(EmissionPrimitiveType type) => locals.Add(output.DeclareLocal(types.Map(type)));

    internal static bool TryEmit(MethodGenerator method)
    {
        var symbol = method.MethodSymbol;
        // Keep debug sequence points and specialized/generic/synthesized lowering on the
        // established path. Widen this gate as the shared model acquires those contracts.
        if (method.Compilation.Options.OptimizationLevel != OptimizationLevel.Release ||
            method.TypeGenerator.CodeGen.HasDebugOutput ||
            !LinearMethodBody.HasSupportedSignature(symbol) ||
            symbol.ContainingType is not { Arity: 0 } || method.LambdaClosure is not null)
            return false;
        // A logical no-result return can use ret directly only when the CLI signature
        // is actually void. Keep any value-bearing Unit representation on general codegen.
        if (!LinearMethodBody.ReturnsValue(symbol) &&
            method.MethodBase is not MethodInfo { ReturnType.FullName: "System.Void" })
            return false;
        if (!SourceCallablePlan.TryCreate(symbol, out var declaration, ReflectionEmitCapabilities.Shared) ||
            !declaration!.TryLowerBody(method.Compilation, IsConsoleWrite, out var lowered, out _, ReflectionEmitCapabilities.Shared))
            return false;
        // Resolution can create metadata proxies just as in general codegen. Builders
        // are only opened after the complete body has passed shared lowering.
        lowered!.Emit(new ReflectionEmitLinearMethodBuilder(method, method.ILBuilderFactory.Create(method)));
        return true;
    }

    private static bool IsConsoleWrite(BoundInvocationExpression call)
        => call.Receiver is null or BoundTypeExpression && call.Method.IsStatic && !call.Method.IsGenericMethod &&
            call.Method.Name == "WriteLine" &&
            call.Method.ContainingType?.ToFullyQualifiedMetadataName() == "System.Console" &&
            call.Method.Parameters.Length == 1 &&
            call.Method.Parameters[0].Type.GetNonNullableType().SpecialType == SpecialType.System_String &&
            call.Method.Parameters[0].RefKind == RefKind.None &&
            call.Method.ReturnType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void;

    public void Emit(LinearInstruction instruction)
    {
        switch (instruction.Kind)
        {
            case LinearInstructionKind.NewObject: output.Emit(OpCodes.Newobj, method.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetConstructorInfo(instruction.Method!)); break;
            case LinearInstructionKind.Receiver: output.Emit(OpCodes.Ldarg_0); break;
            case LinearInstructionKind.LoadField: output.Emit(OpCodes.Ldfld, method.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetFieldInfo(instruction.Field!)); break;
            case LinearInstructionKind.StoreField: output.Emit(OpCodes.Stfld, method.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetFieldInfo(instruction.Field!)); break;
            case LinearInstructionKind.InstanceCall:
                output.Emit(OpCodes.Callvirt, method.TypeGenerator.CodeGen.LinearCallReferences.Resolve(instruction.Method!)); break;
            case LinearInstructionKind.Constant64: output.Emit(OpCodes.Ldc_I8, instruction.Long); break;
            case LinearInstructionKind.Convert64: output.Emit(OpCodes.Conv_I8); break;
            case LinearInstructionKind.Convert32: output.Emit(OpCodes.Conv_I4); break;
            case LinearInstructionKind.Negate: output.Emit(OpCodes.Neg); break;
            case LinearInstructionKind.Complement: output.Emit(OpCodes.Not); break;
            case LinearInstructionKind.Pop: output.Emit(OpCodes.Pop); break;
            case LinearInstructionKind.Not: output.Emit(OpCodes.Ldc_I4_0); output.Emit(OpCodes.Ceq); break;
            case LinearInstructionKind.Boolean: output.Emit(OpCodes.Ldc_I4, instruction.Integer); break;
            case LinearInstructionKind.Equal: output.Emit(OpCodes.Ceq); break;
            case LinearInstructionKind.Less: output.Emit(OpCodes.Clt); break;
            case LinearInstructionKind.Greater: output.Emit(OpCodes.Cgt); break;
            case LinearInstructionKind.Label: output.MarkLabel(labels[instruction.Integer]); break;
            case LinearInstructionKind.Branch: output.Emit(OpCodes.Br, labels[instruction.Integer]); break;
            case LinearInstructionKind.BranchTrue: output.Emit(OpCodes.Brtrue, labels[instruction.Integer]); break;
            case LinearInstructionKind.BranchFalse: output.Emit(OpCodes.Brfalse, labels[instruction.Integer]); break;
            case LinearInstructionKind.Constant: output.Emit(OpCodes.Ldc_I4, instruction.Integer); break;
            case LinearInstructionKind.Argument: output.Emit(OpCodes.Ldarg, instruction.Integer); break;
            case LinearInstructionKind.LoadLocal: output.Emit(OpCodes.Ldloc, locals[instruction.Integer]); break;
            case LinearInstructionKind.StoreLocal: output.Emit(OpCodes.Stloc, locals[instruction.Integer]); break;
            case LinearInstructionKind.Add: output.Emit(OpCodes.Add); break;
            case LinearInstructionKind.Subtract: output.Emit(OpCodes.Sub); break;
            case LinearInstructionKind.Divide: output.Emit(OpCodes.Div); break;
            case LinearInstructionKind.Remainder: output.Emit(OpCodes.Rem); break;
            case LinearInstructionKind.BitwiseAnd: output.Emit(OpCodes.And); break;
            case LinearInstructionKind.BitwiseOr: output.Emit(OpCodes.Or); break;
            case LinearInstructionKind.BitwiseXor: output.Emit(OpCodes.Xor); break;
            case LinearInstructionKind.ShiftLeft: output.Emit(OpCodes.Shl); break;
            case LinearInstructionKind.ShiftRight: output.Emit(OpCodes.Shr); break;
            case LinearInstructionKind.Multiply: output.Emit(OpCodes.Mul); break;
            case LinearInstructionKind.String: output.Emit(OpCodes.Ldstr, instruction.Text!); break;
            case LinearInstructionKind.ConsoleWrite:
                goto case LinearInstructionKind.Call;
            case LinearInstructionKind.Call:
                var target = method.TypeGenerator.CodeGen.LinearCallReferences.Resolve(instruction.Method!);
                output.Emit(OpCodes.Call, target);
                // Raven Unit can have a value representation in imported CLI signatures.
                // This linear subset uses Unit calls only as expression statements.
                if (!LinearMethodBody.ReturnsValue(instruction.Method!) && target.ReturnType.FullName != "System.Void")
                    output.Emit(OpCodes.Pop);
                break;
            case LinearInstructionKind.Return: output.Emit(OpCodes.Ret); break;
            default: throw new InvalidOperationException("Unsupported .NET linear instruction: " + instruction.Kind);
        }
    }
}
