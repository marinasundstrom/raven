using System;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEMethodSymbol
{
    bool IMethodOverloadPriority.TryGetOverloadResolutionPriority(out int priority)
    {
        return TryGetOverloadResolutionPriority(ReflectionMethodBase, out priority);

        static bool TryGetOverloadResolutionPriority(System.Reflection.MethodBase methodBase, out int priority)
        {
            priority = 0;

            try
            {
                // MetadataLoadContext cannot resolve GetBaseDefinition. Nonvirtual
                // and new-slot methods already declare their own base definition.
                if (methodBase is System.Reflection.MethodInfo { IsVirtual: true } methodInfo &&
                    (methodInfo.Attributes & System.Reflection.MethodAttributes.NewSlot) == 0)
                {
                    methodBase = methodInfo.GetBaseDefinition();
                }

                foreach (var attribute in methodBase.GetCustomAttributesData())
                {
                    if (!string.Equals(attribute.AttributeType.FullName,
                            "System.Runtime.CompilerServices.OverloadResolutionPriorityAttribute",
                            StringComparison.Ordinal))
                    {
                        continue;
                    }

                    if (attribute.ConstructorArguments.Count == 1 &&
                        attribute.ConstructorArguments[0].Value is int constructorPriority)
                    {
                        priority = constructorPriority;
                        return true;
                    }
                }
            }
            catch
            {
            }

            return false;
        }
    }
}
