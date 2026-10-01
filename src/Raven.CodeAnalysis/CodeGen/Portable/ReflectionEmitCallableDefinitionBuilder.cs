using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitCallableDefinitionBuilder(TypeBuilder owner,
    MethodAttributes attributes, Func<SpecialType, Type> resolveType) : ICallableDefinitionBuilder<MethodBuilder>
{
    private readonly IEmissionTypeMapper<Type> types = new ReflectionEmitTypeMapper(resolveType);

    public MethodBuilder DefineMethod(string metadataName, SourceCallablePlan plan)
    {
        var result = types.Map(plan.Signature.ReturnType);
        var parameters = plan.Signature.ParameterTypes.Select(types.Map).ToArray();
        // Attributes already include source access and the CLI carrier/lifted-method policy.
        return owner.DefineMethod(metadataName, attributes, CallingConventions.Standard, result, parameters);
    }
}
