using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitCallableDefinitionBuilder(TypeBuilder owner,
    MethodAttributes attributes, Func<SpecialType, Type> resolveType) : ICallableDefinitionBuilder<MethodBuilder>
{
    public MethodBuilder DefineMethod(string metadataName, PrimitiveCallableSignature signature)
    {
        var result = resolveType(signature.ReturnType);
        var parameters = signature.ParameterTypes.Select(resolveType).ToArray();
        return owner.DefineMethod(metadataName, attributes, CallingConventions.Standard, result, parameters);
    }
}
