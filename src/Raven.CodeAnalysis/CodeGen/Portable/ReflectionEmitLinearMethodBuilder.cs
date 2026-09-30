using System.Reflection.Emit;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Adapts the existing .NET method builder; declaration creation, attributes, signature
// mapping, type completion and PE writing remain owned by the existing code generator.
internal sealed class ReflectionEmitLinearMethodBuilder(MethodGenerator method, IILBuilder output) : ILinearMethodBuilder
{
    internal static bool TryEmit(MethodGenerator method)
    {
        var symbol = method.MethodSymbol;
        // Keep debug sequence points and specialized/generic/synthesized lowering on the
        // established path. Widen this gate as the shared model acquires those contracts.
        if (method.Compilation.Options.OptimizationLevel != OptimizationLevel.Release ||
            method.TypeGenerator.CodeGen.HasDebugOutput ||
            !symbol.IsStatic || symbol.ReturnType.SpecialType != SpecialType.System_Int32 ||
            symbol.ContainingType is not { Arity: 0 } || method.LambdaClosure is not null ||
            symbol.DeclaringSyntaxReferences.Length != 1 ||
            symbol.DeclaringSyntaxReferences[0].GetSyntax() is not MethodDeclarationSyntax { Body: { } syntax })
            return false;
        var model = method.Compilation.GetSemanticModel(syntax.SyntaxTree);
        if (model.GetOperation(syntax) is not IBlockOperation body ||
            !LinearMethodBody.TryLower(symbol, body, IsConsoleLiteral, out var lowered, out _))
            return false;
        // Resolution can create metadata proxies just as in general codegen. Builders
        // are only opened after the complete body has passed shared lowering.
        lowered!.Emit(new ReflectionEmitLinearMethodBuilder(method, method.ILBuilderFactory.Create(method)));
        return true;
    }

    private static bool IsConsoleLiteral(IInvocationOperation call)
        => call.Instance is null && call.TargetMethod.IsStatic && !call.TargetMethod.IsGenericMethod &&
            call.TargetMethod.Name == "WriteLine" &&
            call.TargetMethod.ContainingType?.ToFullyQualifiedMetadataName() == "System.Console" &&
            call.TargetMethod.Parameters.Length == 1 &&
            call.TargetMethod.Parameters[0].Type.GetNonNullableType().SpecialType == SpecialType.System_String &&
            call.TargetMethod.Parameters[0].RefKind == RefKind.None &&
            call.TargetMethod.ReturnType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void;

    public void Emit(LinearInstruction instruction)
    {
        switch (instruction.Kind)
        {
            case LinearInstructionKind.Constant: output.Emit(OpCodes.Ldc_I4, instruction.Integer); break;
            case LinearInstructionKind.Argument: output.Emit(OpCodes.Ldarg, instruction.Integer); break;
            case LinearInstructionKind.Add: output.Emit(OpCodes.Add); break;
            case LinearInstructionKind.Subtract: output.Emit(OpCodes.Sub); break;
            case LinearInstructionKind.Multiply: output.Emit(OpCodes.Mul); break;
            case LinearInstructionKind.ConsoleLiteral:
                output.Emit(OpCodes.Ldstr, instruction.Text!);
                goto case LinearInstructionKind.Call;
            case LinearInstructionKind.Call:
                var target = method.TypeGenerator.CodeGen.GetMethodInfoOrMetadataProxy(instruction.Method!);
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
