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

    public void DeclareLocal(SpecialType type) => locals.Add(output.DeclareLocal(method.ResolveClrType(
        method.Compilation.GetSpecialType(type))));

    internal static bool TryEmit(MethodGenerator method)
    {
        var symbol = method.MethodSymbol;
        // Keep debug sequence points and specialized/generic/synthesized lowering on the
        // established path. Widen this gate as the shared model acquires those contracts.
        if (method.Compilation.Options.OptimizationLevel != OptimizationLevel.Release ||
            method.TypeGenerator.CodeGen.HasDebugOutput ||
            !symbol.IsStatic || !LinearMethodBody.HasSupportedSignature(symbol) ||
            symbol.ContainingType is not { Arity: 0 } || method.LambdaClosure is not null ||
            symbol.DeclaringSyntaxReferences.Length != 1)
            return false;
        // A logical no-result return can use ret directly only when the CLI signature
        // is actually void. Keep any value-bearing Unit representation on general codegen.
        if (!LinearMethodBody.ReturnsValue(symbol) &&
            method.MethodBase is not MethodInfo { ReturnType.FullName: "System.Void" })
            return false;
        if (!SourceCallablePlan.TryCreate(symbol, out var declaration) ||
            !declaration!.TryLowerBody(method.Compilation, IsConsoleLiteral, out var lowered, out _))
            return false;
        // Resolution can create metadata proxies just as in general codegen. Builders
        // are only opened after the complete body has passed shared lowering.
        lowered!.Emit(new ReflectionEmitLinearMethodBuilder(method, method.ILBuilderFactory.Create(method)));
        return true;
    }

    private static bool IsConsoleLiteral(BoundInvocationExpression call)
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
            case LinearInstructionKind.Multiply: output.Emit(OpCodes.Mul); break;
            case LinearInstructionKind.ConsoleLiteral:
                output.Emit(OpCodes.Ldstr, instruction.Text!);
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
