using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitCallableDefinitionBuilder(TypeBuilder owner,
    MethodAttributes attributes, Func<SpecialType, Type> resolveType, Func<ITypeSymbol, Type> resolveClass) : ICallableDefinitionBuilder<MethodBuilder>
{
    private readonly IEmissionTypeMapper<Type> types = new ReflectionEmitTypeMapper(resolveType);

    public MethodBuilder DefineMethod(string metadataName, SourceCallablePlan plan)
    {
        Type Map(EmissionType type)
        {
            if (type.IsByReference) return Map(type with { IsByReference = false }).MakeByRefType();
            if (type.Primitive is { } p) return types.Map(p);
            if (type.Array is { } array)
            {
                CallableSignature.TryType(array.ElementType, false, out var element);
                return Map(element).MakeArrayType();
            }
            return resolveClass((ITypeSymbol?)type.OwnerParameter ?? type.Nominal!);
        }
        var result = Map(plan.Signature.ReturnType);
        var parameters = plan.Signature.ParameterTypes.Select(Map).ToArray();
        // Attributes already include source access and the CLI carrier/lifted-method policy.
        return owner.DefineMethod(metadataName, attributes, CallingConventions.Standard, result, parameters);
    }
}
