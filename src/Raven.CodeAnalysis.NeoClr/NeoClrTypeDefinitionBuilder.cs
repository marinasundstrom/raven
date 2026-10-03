using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeDefinitionBuilder(AssemblyBuilder assembly, Func<INamedTypeSymbol, TypeBuilder> resolveOwner) : ITypeDefinitionBuilder<TypeBuilder>
{
    public TypeBuilder DefineType(SourceTypePlan plan)
    {
        var visibility = plan.Visibility switch
        {
            Accessibility.Public => TypeVisibility.Public,
            Accessibility.Internal => TypeVisibility.Internal,
            _ => throw new InvalidOperationException("Unsupported type visibility")
        };
        if (plan.MetadataOwner is { } parent)
        {
            var owner = resolveOwner(parent);
            return plan.IsValueType
                ? plan.Symbol.Arity > 0 ? owner.AddNestedGenericValueType(plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
                    : owner.AddNestedValueType(plan.Name, visibility)
                : owner.AddNestedClass(plan.Name, visibility);
        }
        if (plan.IsValueType) return plan.Symbol.Arity > 0
            ? assembly.AddGenericValueType(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
            : assembly.AddValueType(plan.Namespace, plan.Name, visibility);
        if (plan.Symbol.Arity > 0) return plan.IsStatic
            ? assembly.AddGenericType(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
            : assembly.AddGenericClass(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility);
        return plan.IsStatic ? assembly.AddType(plan.Namespace, plan.Name, visibility) : assembly.AddClass(plan.Namespace, plan.Name, visibility);
    }
}
