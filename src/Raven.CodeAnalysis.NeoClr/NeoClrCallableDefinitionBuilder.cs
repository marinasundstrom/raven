using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrCallableDefinitionBuilder(AssemblyBuilder assembly, TypeBuilder? owner = null, Func<INamedTypeSymbol, TypeBuilder>? resolveClass = null, Func<INamedTypeSymbol, SignatureType>? resolveExternal = null)
    : ICallableDefinitionBuilder<MethodBuilder>
{
    public MethodBuilder DefineMethod(string metadataName, SourceCallablePlan plan)
    {
        var visibility = plan.Visibility switch
        {
            Accessibility.Public => MethodVisibility.Public,
            Accessibility.Internal => MethodVisibility.Internal,
            Accessibility.Private => MethodVisibility.Private,
            Accessibility.ProtectedAndProtected when plan.Symbol.MethodKind == MethodKind.Constructor => MethodVisibility.Protected,
            _ => throw new InvalidOperationException("Unsupported native callable visibility")
        };
        var method = owner is null
            ? assembly.AddFunction(plan.Namespace, metadataName, ToOwnedMetadata(plan.Signature), visibility)
            : plan.Symbol.MethodKind == MethodKind.Constructor ? owner.AddConstructor(ToOwnedMetadata(plan.Signature), visibility)
            : plan.Override == EmissionOverrideKind.ObjectToString ? owner.AddOverride(metadataName, ToOwnedMetadata(plan.Signature))
            : plan.Symbol.IsStatic ? owner.AddMethod(metadataName, ToOwnedMetadata(plan.Signature), visibility)
            : owner.AddInstanceMethod(metadataName, ToOwnedMetadata(plan.Signature), visibility);
        foreach (var parameter in plan.Symbol.TypeParameters)
            foreach (var constraint in parameter.ConstraintTypes.Cast<INamedTypeSymbol>())
            {
                if (constraint.DeclaringSyntaxReferences.IsEmpty)
                    method.AddInterfaceConstraint(parameter.Ordinal, resolveExternal!(constraint).ImportedType!);
                else method.AddInterfaceConstraint(parameter.Ordinal, resolveClass!(constraint));
            }
        return method;
    }
    private MethodSignature ToOwnedMetadata(CallableSignature signature)
    {
        SignatureType Map(EmissionType type) => NeoClrTypeMapper.Map(type, resolveClass!, resolveExternal);
        return new(Map(signature.ReturnType), signature.ParameterTypes.Select(Map), signature.GenericParameterNames.IsDefault ? [] : signature.GenericParameterNames, signature.OutParameters.IsDefault ? [] : signature.OutParameters);
    }
    internal static PrimitiveMethodSignature ToMetadata(PrimitiveCallableSignature signature)
        => new(NeoClrTypeMapper.Instance.Map(signature.ReturnType), signature.ParameterTypes.Select(NeoClrTypeMapper.Instance.Map));
}
