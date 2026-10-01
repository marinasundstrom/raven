using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// CLI layout/visibility flags remain owned by TypeGenerator, as do base types,
// attributes, member definitions and completion after creating the type identity.
internal sealed class ReflectionEmitTypeDefinitionBuilder(ModuleBuilder module, TypeAttributes attributes)
    : ITypeDefinitionBuilder<TypeBuilder>
{
    public TypeBuilder DefineType(SourceTypePlan plan) => module.DefineType(plan.FullName, attributes);
}
