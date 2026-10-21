using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrCallableDefinitionBuilder(AssemblyBuilder assembly, TypeBuilder? owner = null, Func<INamedTypeSymbol, TypeBuilder>? resolveClass = null, Func<ITypeSymbol, SignatureType>? resolveExternal = null, Func<ITypeParameterSymbol, SignatureType>? mapParameter = null, bool structuralConstraints = false, Action<INamedTypeSymbol>? admitExternalConstraint = null)
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
            : plan.Symbol.MethodKind == MethodKind.StaticConstructor ? owner.AddTypeInitializer()
            : plan.Symbol.MethodKind == MethodKind.Constructor ? owner.AddConstructor(ToOwnedMetadata(plan.Signature), visibility)
            : plan.IsObjectRootSlot ? owner.AddNativeObjectSlot(metadataName, ToOwnedMetadata(plan.Signature))
            : plan.Symbol.IsOverride ? owner.AddOverride(metadataName, ToOwnedMetadata(plan.Signature))
            : plan.Symbol.IsAbstract ? owner.AddAbstractMethod(metadataName, ToOwnedMetadata(plan.Signature))
            : plan.Symbol.IsVirtual ? owner.AddVirtualMethod(metadataName, ToOwnedMetadata(plan.Signature))
            : plan.Symbol.IsStatic ? owner.AddMethod(metadataName, ToOwnedMetadata(plan.Signature), visibility)
            : owner.AddInstanceMethod(metadataName, ToOwnedMetadata(plan.Signature), visibility);
        foreach (var parameter in plan.Symbol.TypeParameters)
        {
            method.SetSpecialConstraints(parameter.Ordinal, NativeConstraintMapping.ToMetadata(parameter.ConstraintKind));
            var constraints = parameter.ConstraintTypes;
            // Raven's source intersection constraints are represented as conjunctive bounds.
            if (structuralConstraints && constraints.Length > 1 && constraints.All(t => t is INamedTypeSymbol))
            {
                method.AddExperimentalStructuralConstraint(parameter.Ordinal, ReferenceCompositionKind.Intersection,
                    constraints.Select(MapIntersectionConstraint).ToArray());
                continue;
            }
            foreach (var constraint in constraints)
            {
                if (constraint is IStructuralUnionTypeSymbol union)
                    method.AddExperimentalStructuralConstraint(parameter.Ordinal, ReferenceCompositionKind.Union, union.ConstituentTypes.Select(MapConstraint).ToArray());
                else if (constraint is IIntersectionTypeSymbol intersection)
                    method.AddExperimentalStructuralConstraint(parameter.Ordinal, ReferenceCompositionKind.Intersection, intersection.ConstituentTypes.Select(MapIntersectionConstraint).ToArray());
                else if (constraint is INamedTypeSymbol { TypeKind: TypeKind.Class })
                    method.AddBaseTypeConstraint(parameter.Ordinal, MapConstraint(constraint));
                else if (constraint.DeclaringSyntaxReferences.IsEmpty)
                    method.AddInterfaceConstraint(parameter.Ordinal, resolveExternal!(constraint).ImportedType!);
                else method.AddInterfaceConstraint(parameter.Ordinal, resolveClass!((INamedTypeSymbol)constraint));
            }
        }
        SignatureType MapConstraint(ITypeSymbol type) => NeoClrTypeMapper.Map(type, resolveClass!, resolveExternal, mapParameter);
        SignatureType MapIntersectionConstraint(ITypeSymbol type)
        {
            if (type is INamedTypeSymbol named && type.DeclaringSyntaxReferences.IsEmpty)
                admitExternalConstraint?.Invoke(named);
            return NeoClrTypeMapper.Map(type, resolveClass!, resolveExternal, mapParameter);
        }
        return method;
    }
    private MethodSignature ToOwnedMetadata(CallableSignature signature)
    {
        SignatureType Map(EmissionType type) => NeoClrTypeMapper.Map(type, resolveClass!, resolveExternal, mapParameter);
        return new(Map(signature.ReturnType), signature.ParameterTypes.Select(Map), signature.GenericParameterNames.IsDefault ? [] : signature.GenericParameterNames, signature.OutParameters.IsDefault ? [] : signature.OutParameters);
    }
    internal static PrimitiveMethodSignature ToMetadata(PrimitiveCallableSignature signature)
        => new(NeoClrTypeMapper.Instance.Map(signature.ReturnType), signature.ParameterTypes.Select(NeoClrTypeMapper.Instance.Map));
}
