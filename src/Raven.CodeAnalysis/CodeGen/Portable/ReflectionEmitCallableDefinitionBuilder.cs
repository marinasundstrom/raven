using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitCallableDefinitionBuilder(TypeBuilder owner,
    MethodAttributes attributes, Func<SpecialType, Type> resolveType, Func<INamedTypeSymbol, Type> resolveClass) : ICallableDefinitionBuilder<MethodBuilder>
{
    private readonly IEmissionTypeMapper<Type> types = new ReflectionEmitTypeMapper(resolveType);

    public MethodBuilder DefineMethod(string metadataName, SourceCallablePlan plan)
    {
        Type Map(EmissionType type) => type.Primitive is { } p ? types.Map(p) : resolveClass(type.Class!);
        var result = Map(plan.Signature.ReturnType);
        var parameters = plan.Signature.ParameterTypes.Select(Map).ToArray();
        // Attributes already include source access and the CLI carrier/lifted-method policy.
        return owner.DefineMethod(metadataName, attributes, CallingConventions.Standard, result, parameters);
    }
}
