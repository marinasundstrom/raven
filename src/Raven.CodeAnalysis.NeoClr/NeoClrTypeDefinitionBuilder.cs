using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeDefinitionBuilder(AssemblyBuilder assembly) : ITypeDefinitionBuilder<TypeBuilder>
{
    public TypeBuilder DefineType(SourceTypePlan plan)
    {
        var visibility = plan.Visibility switch
        {
            Accessibility.Public => TypeVisibility.Public,
            Accessibility.Internal => TypeVisibility.Internal,
            _ => throw new InvalidOperationException("Unsupported top-level type visibility")
        };
        if (plan.Symbol.Arity > 0) return plan.IsStatic
            ? assembly.AddGenericType(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility)
            : assembly.AddGenericClass(plan.Namespace, plan.Symbol.Name, plan.Symbol.TypeParameters.Select(p => p.Name), visibility);
        return plan.IsStatic ? assembly.AddType(plan.Namespace, plan.Name, visibility) : assembly.AddClass(plan.Namespace, plan.Name, visibility);
    }
}
