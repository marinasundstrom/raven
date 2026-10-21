using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

internal static class NativeConstraintMapping
{
    internal static TypeParameterConstraints ToMetadata(TypeParameterConstraintKind kind)
        => (kind.HasFlag(TypeParameterConstraintKind.ReferenceType) ? TypeParameterConstraints.ReferenceType : 0)
        | (kind.HasFlag(TypeParameterConstraintKind.ValueType) ? TypeParameterConstraints.ValueType | TypeParameterConstraints.DefaultConstructor : 0)
        | (kind.HasFlag(TypeParameterConstraintKind.Constructor) ? TypeParameterConstraints.DefaultConstructor : 0)
        | (kind.HasFlag(TypeParameterConstraintKind.FunctionType) ? TypeParameterConstraints.FunctionType : 0);

    internal static TypeParameterConstraintKind ToSymbol(TypeParameterConstraints flags)
        => (flags.HasFlag(TypeParameterConstraints.ReferenceType) ? TypeParameterConstraintKind.ReferenceType : 0)
        | (flags.HasFlag(TypeParameterConstraints.ValueType) ? TypeParameterConstraintKind.ValueType : 0)
        | (flags.HasFlag(TypeParameterConstraints.DefaultConstructor) ? TypeParameterConstraintKind.Constructor : 0)
        | (flags.HasFlag(TypeParameterConstraints.FunctionType) ? TypeParameterConstraintKind.FunctionType : 0);
}
