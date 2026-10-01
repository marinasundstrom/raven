using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitCallableDefinitionBuilder(TypeBuilder owner,
    MethodAttributes attributes, Func<SpecialType, Type> resolveType) : ICallableDefinitionBuilder<MethodBuilder>
{
    private readonly IEmissionTypeMapper<Type> types = new ReflectionEmitTypeMapper(resolveType);

    public MethodBuilder DefineMethod(string metadataName, PrimitiveCallableSignature signature)
    {
        var result = types.Map(signature.ReturnType);
        var parameters = signature.ParameterTypes.Select(types.Map).ToArray();
        return owner.DefineMethod(metadataName, attributes, CallingConventions.Standard, result, parameters);
    }
}
