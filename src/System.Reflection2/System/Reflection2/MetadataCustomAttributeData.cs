namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;

internal sealed class MetadataCustomAttributeData : CustomAttributeData
{
    private readonly ConstructorInfo _constructor;
    private readonly ReadOnlyCollection<CustomAttributeTypedArgument> _constructorArguments;
    private readonly ReadOnlyCollection<CustomAttributeNamedArgument> _namedArguments;

    private MetadataCustomAttributeData(ConstructorInfo constructor, IEnumerable<CustomAttributeTypedArgument> constructorArguments, IEnumerable<CustomAttributeNamedArgument> namedArguments)
    {
        _constructor = constructor ?? throw new ArgumentNullException(nameof(constructor));
        _constructorArguments = new ReadOnlyCollection<CustomAttributeTypedArgument>(constructorArguments.ToArray());
        _namedArguments = new ReadOnlyCollection<CustomAttributeNamedArgument>(namedArguments.ToArray());
    }

    public override ConstructorInfo Constructor => _constructor;

    public override IList<CustomAttributeTypedArgument> ConstructorArguments => _constructorArguments;

    public override IList<CustomAttributeNamedArgument> NamedArguments => _namedArguments;

    public static MetadataCustomAttributeData Create(MetadataModule module, CustomAttributeHandle handle, MetadataType? genericTypeContext, IReadOnlyList<Type>? genericMethodArguments)
    {
        var reader = module.Reader;
        var attribute = reader.GetCustomAttribute(handle);
        if (module.ResolveMethod(attribute.Constructor, genericTypeContext, genericMethodArguments) is not ConstructorInfo constructor)
        {
            throw new NotSupportedException("Custom attribute constructor is not a constructor.");
        }

        var provider = new MetadataCustomAttributeTypeProvider(module, genericTypeContext?.GetGenericArguments(), genericMethodArguments);
        var decoded = attribute.DecodeValue(provider);

        var constructorArguments = decoded.FixedArguments.Select(ConvertTypedArgument);
        var namedArguments = decoded.NamedArguments.Select(arg => ConvertNamedArgument(constructor.DeclaringType ?? throw new InvalidOperationException("Attribute constructor is missing declaring type."), arg));

        return new MetadataCustomAttributeData(constructor, constructorArguments, namedArguments);
    }

    private static CustomAttributeNamedArgument ConvertNamedArgument(Type declaringType, CustomAttributeNamedArgument<Type> argument)
    {
        const BindingFlags Binding = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        var memberName = argument.Name ?? throw new InvalidOperationException("Custom attribute named argument is missing a member name.");

        MemberInfo member = argument.Kind switch
        {
            CustomAttributeNamedArgumentKind.Field => declaringType.GetField(memberName, Binding) ?? throw new MissingFieldException(declaringType.FullName, memberName),
            CustomAttributeNamedArgumentKind.Property => declaringType.GetProperty(memberName, Binding) ?? throw new MissingMemberException(declaringType.FullName, memberName),
            _ => throw new NotSupportedException($"Unsupported custom attribute argument kind '{argument.Kind}'."),
        };

        var valueArgument = argument.Value;
        if (valueArgument is CustomAttributeTypedArgument<Type> typedValue)
        {
            return new CustomAttributeNamedArgument(member, ConvertTypedArgument(typedValue));
        }

        return new CustomAttributeNamedArgument(member, new CustomAttributeTypedArgument(argument.Type, valueArgument));
    }

    private static CustomAttributeTypedArgument ConvertTypedArgument(CustomAttributeTypedArgument<Type> argument)
    {
        return CreateTypedArgument(argument.Type, argument.Value);
    }

    private static CustomAttributeTypedArgument CreateTypedArgument(Type type, object? value)
    {
        if (value is IReadOnlyList<CustomAttributeTypedArgument<Type>> arrayValues)
        {
            var converted = arrayValues.Select(ConvertTypedArgument).ToList().AsReadOnly();
            return new CustomAttributeTypedArgument(type, converted);
        }

        if (value is not null && type is MetadataType metadataEnum && metadataEnum.IsEnum)
        {
            var runtimeEnumType = ResolveRuntimeEnumType(metadataEnum);
            if (runtimeEnumType?.IsEnum == true)
            {
                value = Enum.ToObject(runtimeEnumType, value);
            }

            return new CustomAttributeTypedArgument(metadataEnum, value);
        }

        if (value is not null && type.IsEnum)
        {
            value = Enum.ToObject(type, value);
        }

        return new CustomAttributeTypedArgument(type, value);
    }

    private static Type? ResolveRuntimeEnumType(MetadataType metadataType)
    {
        var fullName = metadataType.FullName ?? string.Empty;
        var assemblyQualifiedName = metadataType.AssemblyQualifiedName ?? string.Empty;

        var runtimeType = Type.GetType(fullName, throwOnError: false, ignoreCase: false)
            ?? Type.GetType(assemblyQualifiedName, throwOnError: false, ignoreCase: false);

        if (runtimeType?.IsEnum == true)
        {
            return runtimeType;
        }

        return AppDomain.CurrentDomain
            .GetAssemblies()
            .Select(assembly => assembly.GetType(fullName, throwOnError: false, ignoreCase: false))
            .FirstOrDefault(t => t?.IsEnum == true);
    }
}

internal static class MetadataCustomAttributeDecoder
{
    public static IList<CustomAttributeData> Decode(MetadataModule module, CustomAttributeHandleCollection handles, MetadataType? genericTypeContext, IReadOnlyList<Type>? genericMethodArguments)
    {
        if (handles.Count == 0)
        {
            return Array.Empty<CustomAttributeData>();
        }

        var results = new List<CustomAttributeData>(handles.Count);
        foreach (var handle in handles)
        {
            results.Add(MetadataCustomAttributeData.Create(module, handle, genericTypeContext, genericMethodArguments));
        }

        return results;
    }
}
