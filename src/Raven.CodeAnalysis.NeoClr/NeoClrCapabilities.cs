using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

// Adapter-owned admission profile for both declarations and shared bodies.
internal static class NeoClrCapabilities
{
    internal static EmissionCapabilities Shared { get; } = new(
        [EmissionPrimitiveType.NoResult, EmissionPrimitiveType.Int32, EmissionPrimitiveType.Int64, EmissionPrimitiveType.Boolean, EmissionPrimitiveType.String],
        [
            LinearInstructionKind.Constant, LinearInstructionKind.Argument, LinearInstructionKind.Add,
            LinearInstructionKind.Subtract, LinearInstructionKind.Multiply, LinearInstructionKind.Call,
            LinearInstructionKind.ConsoleWrite, LinearInstructionKind.String, LinearInstructionKind.Return,
            LinearInstructionKind.LoadLocal, LinearInstructionKind.StoreLocal, LinearInstructionKind.Boolean,
            LinearInstructionKind.Not, LinearInstructionKind.Equal, LinearInstructionKind.Less,
            LinearInstructionKind.Greater, LinearInstructionKind.Label, LinearInstructionKind.Branch,
            LinearInstructionKind.BranchTrue, LinearInstructionKind.BranchFalse, LinearInstructionKind.Pop,
            LinearInstructionKind.Constant64, LinearInstructionKind.Convert64, LinearInstructionKind.Convert32,
            LinearInstructionKind.Negate, LinearInstructionKind.Complement, LinearInstructionKind.Divide, LinearInstructionKind.Remainder,
            LinearInstructionKind.BitwiseAnd, LinearInstructionKind.BitwiseOr, LinearInstructionKind.BitwiseXor
        ],
        [EmissionDeclarationKind.AssemblyFunction, EmissionDeclarationKind.StaticMethod, EmissionDeclarationKind.StaticType],
        [Accessibility.Public, Accessibility.Internal]);
}
