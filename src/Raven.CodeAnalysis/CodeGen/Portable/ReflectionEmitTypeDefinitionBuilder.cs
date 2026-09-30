using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// CLI layout/visibility flags remain owned by TypeGenerator, as do base types,
// attributes, member definitions and completion after creating the type identity.
internal sealed class ReflectionEmitTypeDefinitionBuilder(ModuleBuilder module, TypeAttributes attributes)
    : IStaticTypeDefinitionBuilder<TypeBuilder>
{
    public TypeBuilder DefineType(SourceStaticTypePlan plan) => module.DefineType(plan.FullName, attributes);
}
