namespace Raven.CodeAnalysis.CodeGen.Portable;

// Adapter-owned admission profile for both declarations and shared bodies.
internal static class ReflectionEmitCapabilities
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
            LinearInstructionKind.BitwiseAnd, LinearInstructionKind.BitwiseOr, LinearInstructionKind.BitwiseXor,
            LinearInstructionKind.ShiftLeft, LinearInstructionKind.ShiftRight, LinearInstructionKind.Receiver, LinearInstructionKind.LoadField, LinearInstructionKind.StoreField, LinearInstructionKind.ValueInstanceCall, LinearInstructionKind.InstanceCall, LinearInstructionKind.InterfaceCall, LinearInstructionKind.NewObject,
            LinearInstructionKind.NewArray, LinearInstructionKind.LoadElement, LinearInstructionKind.StoreElement, LinearInstructionKind.ArrayLength, LinearInstructionKind.LocalAddress, LinearInstructionKind.LoadIndirect, LinearInstructionKind.StoreIndirect, LinearInstructionKind.DefaultValue, LinearInstructionKind.Duplicate
        ],
        [EmissionDeclarationKind.AssemblyFunction, EmissionDeclarationKind.NamespacedAssemblyFunction, EmissionDeclarationKind.StaticMethod, EmissionDeclarationKind.StaticType, EmissionDeclarationKind.RootClass, EmissionDeclarationKind.InstanceMethod, EmissionDeclarationKind.PropertyAccessor, EmissionDeclarationKind.IndexerAccessor, EmissionDeclarationKind.Interface, EmissionDeclarationKind.InterfaceMethod, EmissionDeclarationKind.InterfaceProperty, EmissionDeclarationKind.InterfaceInheritance, EmissionDeclarationKind.InterfaceImplementation],
        [Accessibility.Public, Accessibility.Internal],
        [Accessibility.Public, Accessibility.Internal, Accessibility.Private, Accessibility.ProtectedAndProtected,
            Accessibility.ProtectedOrInternal, Accessibility.ProtectedAndInternal],
        [Accessibility.Public, Accessibility.Internal, Accessibility.Private], allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsArrays: true, allowsGenericMethods: true, allowsGenericInstanceMethods: true, allowsGenericStaticOwners: true, allowsGenericClassOwners: true, allowsConstructedFieldReferences: true, allowsNominalTypeBounds: true, allowsSpecialTypeConstraints: true, allowsGenericInterfaceDeclarations: true, allowsInterfaceSignatures: true, allowsInterfaceDispatch: true, allowsManagedReferences: true);
}
