using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeDefinitionBuilder(AssemblyBuilder assembly) : IStaticTypeDefinitionBuilder<TypeBuilder>
{
    public TypeBuilder DefineType(SourceStaticTypePlan plan) => assembly.AddType(plan.Namespace, plan.Name);
}
