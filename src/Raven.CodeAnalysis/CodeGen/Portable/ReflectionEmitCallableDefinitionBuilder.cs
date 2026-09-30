using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitCallableDefinitionBuilder(TypeBuilder owner,
    MethodAttributes attributes, Func<SpecialType, Type> resolveType) : ICallableDefinitionBuilder<MethodBuilder>
{
    public MethodBuilder DefineMethod(string metadataName, Int32CallableSignature signature)
    {
        var result = resolveType(signature.ReturnsValue ? SpecialType.System_Int32 : SpecialType.System_Void);
        var parameters = signature.ParameterCount == 0 ? Type.EmptyTypes :
            Enumerable.Repeat(resolveType(SpecialType.System_Int32), signature.ParameterCount).ToArray();
        return owner.DefineMethod(metadataName, attributes, CallingConventions.Standard, result, parameters);
    }
}
