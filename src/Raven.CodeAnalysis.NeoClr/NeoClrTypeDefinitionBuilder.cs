using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeDefinitionBuilder(AssemblyBuilder assembly, Func<INamedTypeSymbol, TypeBuilder> resolveOwner, Func<ITypeSymbol, SignatureType> resolveExternal, Func<ITypeSymbol, SignatureType> resolveSignature) : ITypeDefinitionBuilder<TypeBuilder>
{
    public void BindBase(SourceTypePlan plan, TypeBuilder definition)
    {
        if (plan.IsValueType && (definition.NativePrimitive is not null || definition.NativeGrapheme) &&
            plan.Symbol.BaseType is { SpecialType: SpecialType.System_ValueType } localValueBase &&
            SymbolEqualityComparer.Default.Equals(localValueBase.ContainingAssembly, plan.Symbol.ContainingAssembly))
        {
            definition.SetValueTypeBase(resolveOwner(localValueBase));
            return;
        }
        if (plan.IsEnum && plan.Symbol.BaseType is { SpecialType: SpecialType.System_Enum } localEnumBase &&
            SymbolEqualityComparer.Default.Equals(localEnumBase.ContainingAssembly, plan.Symbol.ContainingAssembly))
        {
            definition.SetEnumBase(resolveOwner(localEnumBase));
            return;
        }
        if (plan.IsEnum && plan.Symbol.BaseType is
            {
                SpecialType: SpecialType.System_Enum,
                ContainingAssembly: IImportedAssemblySymbol { ResolvedArtifact: not null }
            } enumBase)
        {
            definition.SetEnumBase(resolveExternal(enumBase).ImportedType!);
            return;
        }
        if (plan.IsValueType && !plan.IsEnum && plan.Symbol.BaseType is
            {
                SpecialType: SpecialType.System_ValueType,
                ContainingAssembly: IImportedAssemblySymbol { ResolvedArtifact: not null }
            } valueBase)
        {
            definition.SetValueTypeBase(resolveExternal(valueBase).ImportedType!);
            return;
        }
        // Native metadata must retain the selected runtime Object identity, not
        // merely the bootstrap CLI Object placeholder used by AddClass.
        var parent = plan.ClassBase ?? (plan.HasImportedObjectBase ? plan.Symbol.BaseType : null);
        if (parent is null) return;
        if (!SymbolEqualityComparer.Default.Equals(parent.ContainingAssembly, plan.Symbol.ContainingAssembly))
            definition.SetBaseType(resolveExternal(parent).ImportedType!);
        else if (parent.Arity > 0)
            definition.SetBaseType(resolveSignature(parent).GenericInstance!);
    }

    public TypeBuilder DefineType(SourceTypePlan plan)
    {
        var visibility = plan.Visibility switch
        {
            Accessibility.Public => TypeVisibility.Public,
            Accessibility.Internal => TypeVisibility.Internal,
            _ => throw new InvalidOperationException("Unsupported type visibility")
        };
        if (plan.IsObjectRoot) return assembly.AddNativeObjectRoot();
        if (plan.IsClosedHierarchy) return plan.ClassBase is { } closedBase
            ? assembly.AddClosedClass(plan.Namespace, plan.Name, resolveOwner(closedBase), visibility)
            : assembly.AddClosedClass(plan.Namespace, plan.Name, visibility);
        if (plan.IsEnum)
        {
            var type = assembly.AddEnum(plan.Namespace, plan.Name, visibility);
            foreach (var field in plan.Symbol.GetMembers().OfType<IFieldSymbol>().Where(f => f.IsConst))
                type.AddEnumMember(field.MetadataName, (int)field.GetConstantValue()!);
            return type;
        }
        // Binding lifts extension receiver type parameters onto each method,
        // matching Raven's CLR representation in a nongeneric static container.
        if (plan.IsExtensionContainer) return assembly.AddType(plan.Namespace, plan.Symbol.Name, visibility);
        if (plan.MetadataOwner is { } parent)
        {
            var owner = resolveOwner(parent);
            if (!plan.IsValueType && plan.ClassBase is { } localRoot && (SourceTypePlan.IsSourceObjectRoot(localRoot) || SymbolEqualityComparer.Default.Equals(localRoot, parent) && parent.IsSealedHierarchy))
            {
                var rootParameters = NeoClrTypeMapper.OwnerParameters(plan.Symbol);
                return owner.AddNestedClass(plan.Symbol.Name, resolveOwner(localRoot),
                    rootParameters.Length == 0 ? null : rootParameters.Select((p, i) => "T" + i), visibility);
            }
            return plan.IsValueType
                ? plan.Symbol.Arity > 0 ? owner.AddNestedGenericValueType(plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
                    : owner.AddNestedValueType(plan.Name, visibility)
                : NeoClrTypeMapper.OwnerParameters(plan.Symbol) is { Length: > 0 } parameters
                    ? owner.AddNestedGenericClass(plan.Symbol.Name, parameters.Select((p, i) => "T" + i), visibility)
                    : owner.AddNestedClass(plan.Name, visibility);
        }
        if (plan.IsValueType) return plan.Symbol.Arity > 0
            ? assembly.AddGenericValueType(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
            : assembly.AddValueType(plan.Namespace, plan.Name, visibility);
        if (plan.Symbol.Arity > 0)
        {
            var generic = plan.IsStatic
            ? assembly.AddGenericType(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
            : plan.ClassBase is { } genericBase
                ? genericBase.Arity == 0 && SymbolEqualityComparer.Default.Equals(genericBase.ContainingAssembly, plan.Symbol.ContainingAssembly)
                    ? assembly.AddGenericClass(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), resolveOwner(genericBase), visibility)
                    : assembly.AddGenericClass(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
                : assembly.AddGenericClass(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility);
            if (plan.Symbol.IsAbstract && !plan.IsStatic) generic.SetAbstractClass();
            return generic;
        }
        TypeBuilder DefineDerived(INamedTypeSymbol baseType)
        {
            if (SymbolEqualityComparer.Default.Equals(baseType.ContainingAssembly, plan.Symbol.ContainingAssembly))
                return baseType.Arity == 0
                    ? assembly.AddClass(plan.Namespace, plan.Name, resolveOwner(baseType), visibility)
                    : assembly.AddClass(plan.Namespace, plan.Name, visibility);
            return assembly.AddClass(plan.Namespace, plan.Name, visibility);
        }
        var result = plan.ClassBase is { } baseType ? DefineDerived(baseType)
            : plan.IsStatic ? assembly.AddType(plan.Namespace, plan.Name, visibility) : assembly.AddClass(plan.Namespace, plan.Name, visibility);
        if (plan.Symbol.IsAbstract && !plan.IsStatic) result.SetAbstractClass();
        return result;
    }
}
