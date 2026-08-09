using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Scripting;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

/// <summary>
/// Emits the runtime bridge for compiler-owned submission symbols.
/// </summary>
internal static class SubmissionCodeGenerator
{
    internal static bool TryEmitVariableLoad(
        Generator generator,
        ILocalSymbol local,
        out EmitInfo emitInfo)
    {
        if (local is not SubmissionVariableSymbol variable)
        {
            emitInfo = default;
            return false;
        }

        generator.ILGenerator.Emit(OpCodes.Ldc_I4, variable.Slot);
        generator.ILGenerator.Emit(OpCodes.Call, GetRuntimeMethod(
            nameof(SubmissionRuntime.Get),
            generator.ResolveClrType(variable.Type)));
        emitInfo = EmitInfo.ForValue(variable);
        return true;
    }

    internal static bool TryEmitCurrentVariableStore(
        Generator generator,
        ILocalSymbol local,
        IILocal value)
    {
        if (!generator.Compilation.TryGetSubmissionVariable(local, out var variable))
            return false;

        EmitVariableStore(generator, variable, value);
        return true;
    }

    internal static void EmitVariableStore(
        Generator generator,
        SubmissionVariableSymbol variable,
        IILocal value)
    {
        generator.ILGenerator.Emit(OpCodes.Ldc_I4, variable.Slot);
        generator.ILGenerator.Emit(OpCodes.Ldloc, value);
        generator.ILGenerator.Emit(OpCodes.Call, GetRuntimeMethod(
            nameof(SubmissionRuntime.Set),
            generator.ResolveClrType(variable.Type)));
    }

    private static MethodInfo GetRuntimeMethod(string name, Type variableType)
        => typeof(SubmissionRuntime)
            .GetMethod(name, BindingFlags.Public | BindingFlags.Static)!
            .MakeGenericMethod(variableType);
}
