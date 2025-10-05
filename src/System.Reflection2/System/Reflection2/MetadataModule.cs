namespace System.Reflection2;

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

/// <summary>
/// Reflection-only <see cref="Module"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataModule : Module
{
    private readonly MetadataAssembly _assembly;
    private readonly MetadataReader _reader;
    private readonly ConcurrentDictionary<TypeDefinitionHandle, MetadataType> _types = new();
    private readonly ConcurrentDictionary<string, MetadataType> _constructedTypes = new(StringComparer.Ordinal);
    private readonly ConcurrentDictionary<(Type elementType, int rank), MetadataArrayType> _arrayTypes = new();
    private readonly ConcurrentDictionary<Type, MetadataByRefType> _byRefTypes = new();
    private readonly ConcurrentDictionary<Type, MetadataPointerType> _pointerTypes = new();
    private readonly ConcurrentDictionary<FunctionPointerSignatureKey, MetadataFunctionPointerType> _functionPointerTypes = new();
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;
    private readonly Func<int, MethodBodyBlock?>? _methodBodyProvider;

    internal MetadataModule(MetadataAssembly assembly, MetadataReader reader, Func<int, MethodBodyBlock?>? methodBodyProvider)
    {
        _assembly = assembly ?? throw new ArgumentNullException(nameof(assembly));
        _reader = reader ?? throw new ArgumentNullException(nameof(reader));
        _methodBodyProvider = methodBodyProvider;
        Name = reader.GetString(reader.GetModuleDefinition().Name);
        ScopeName = Name;
        FullyQualifiedName = assembly.Location ?? Name;
        Mvid = reader.GetGuid(reader.GetModuleDefinition().Mvid);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(()
            => MetadataCustomAttributeDecoder.Decode(this, reader.GetModuleDefinition().GetCustomAttributes(), null, null));
    }

    internal MetadataReader Reader => _reader;

    internal IMetadataRuntimeBridge? RuntimeBridge => (_assembly.LoadContext).RuntimeBridge;

    public override Assembly Assembly => _assembly;

    public override string FullyQualifiedName { get; }

    public override Guid ModuleVersionId => Mvid;

    public override string Name { get; }

    public override string ScopeName { get; }

    internal Guid Mvid { get; }

    public override bool IsResource()
    {
        var hasUserTypes = false;
        foreach (var handle in _reader.TypeDefinitions)
        {
            var definition = _reader.GetTypeDefinition(handle);
            if (IsModuleType(definition))
            {
                continue;
            }

            hasUserTypes = true;
            break;
        }

        if (hasUserTypes)
        {
            return false;
        }

        return _reader.ManifestResources.Count > 0;
    }

    public override Type ResolveType(int metadataToken, Type[]? genericTypeArguments, Type[]? genericMethodArguments)
    {
        var handle = MetadataTokens.EntityHandle(metadataToken);
        return ResolveType(handle, genericContext: null, genericMethodArguments, genericTypeArguments);
    }

    public override MethodBase ResolveMethod(int metadataToken, Type[]? genericTypeArguments, Type[]? genericMethodArguments)
    {
        var handle = MetadataTokens.EntityHandle(metadataToken);
        return ResolveMethod(handle, genericContext: null, genericMethodArguments, genericTypeArguments);
    }

    public override FieldInfo ResolveField(int metadataToken, Type[]? genericTypeArguments, Type[]? genericMethodArguments)
    {
        var handle = MetadataTokens.EntityHandle(metadataToken);
        return ResolveField(handle, genericContext: null, genericMethodArguments, genericTypeArguments);
    }

    public override MemberInfo ResolveMember(int metadataToken, Type[]? genericTypeArguments, Type[]? genericMethodArguments)
    {
        var handle = MetadataTokens.EntityHandle(metadataToken);
        return handle.Kind switch
        {
            HandleKind.TypeDefinition or HandleKind.TypeReference or HandleKind.TypeSpecification
                => ResolveType(handle, genericContext: null, genericMethodArguments, genericTypeArguments),
            HandleKind.MethodDefinition or HandleKind.MethodSpecification
                => ResolveMethod(handle, genericContext: null, genericMethodArguments, genericTypeArguments),
            HandleKind.FieldDefinition
                => ResolveField(handle, genericContext: null, genericMethodArguments, genericTypeArguments),
            HandleKind.MemberReference => GetMemberReferenceSignatureKind((MemberReferenceHandle)handle) == SignatureKind.Field
                ? ResolveField(handle, genericContext: null, genericMethodArguments, genericTypeArguments)
                : ResolveMethod(handle, genericContext: null, genericMethodArguments, genericTypeArguments),
            _ => throw new NotSupportedException($"Unsupported metadata token kind '{handle.Kind}'."),
        };
    }

    public override byte[] ResolveSignature(int metadataToken)
    {
        var handle = MetadataTokens.Handle(metadataToken);
        if (handle.Kind != HandleKind.Blob)
        {
            throw new ArgumentException($"Token '{metadataToken:X8}' does not reference a signature blob.", nameof(metadataToken));
        }

        return _reader.GetBlobBytes((BlobHandle)handle);
    }

    public override string ResolveString(int metadataToken)
    {
        var handle = MetadataTokens.Handle(metadataToken);
        if (handle.Kind != HandleKind.UserString)
        {
            throw new ArgumentException($"Token '{metadataToken:X8}' does not reference a user string.", nameof(metadataToken));
        }

        return _reader.GetUserString((UserStringHandle)handle);
    }


    public override Type[] GetTypes()
        => _reader.TypeDefinitions.Select(handle => (Type)GetOrAddType(handle)).ToArray();

    public override Type? GetType(string className, bool throwOnError, bool ignoreCase)
    {
        if (className is null)
        {
            throw new ArgumentNullException(nameof(className));
        }

        var comparison = ignoreCase ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
        foreach (var handle in _reader.TypeDefinitions)
        {
            var type = GetOrAddType(handle);
            if (string.Equals(type.FullName, className, comparison))
            {
                return type;
            }
        }

        if (throwOnError)
        {
            throw new TypeLoadException(className);
        }

        return null;
    }

    public override IEnumerable<CustomAttributeData> CustomAttributes => _customAttributes.Value;

    public override IList<CustomAttributeData> GetCustomAttributesData() => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
        => throw new NotSupportedException("Materializing module attributes is not supported in metadata-only context.");

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
        => throw new NotSupportedException("Materializing module attributes is not supported in metadata-only context.");

    public override bool IsDefined(Type attributeType, bool inherit)
    {
        if (attributeType is null)
        {
            throw new ArgumentNullException(nameof(attributeType));
        }

        return _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    }

    internal MetadataType GetOrAddType(TypeDefinitionHandle handle)
        => _types.GetOrAdd(handle, h => new MetadataType(this, h));

    internal MetadataType ConstructGenericType(MetadataType definition, IReadOnlyList<Type> typeArguments)
    {
        var key = CreateGenericCacheKey(definition.Handle, typeArguments);
        return _constructedTypes.GetOrAdd(key, _ => new MetadataType(this, definition.Handle, typeArguments.ToArray()));
    }

    internal Type ResolveType(EntityHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments = null, IReadOnlyList<Type>? genericTypeArguments = null)
    {
        if (handle.IsNil)
        {
            return typeof(void);
        }

        return handle.Kind switch
        {
            HandleKind.TypeDefinition => GetOrAddType((TypeDefinitionHandle)handle),
            HandleKind.TypeReference => ResolveTypeReference((TypeReferenceHandle)handle),
            HandleKind.TypeSpecification => ResolveTypeSpecification((TypeSpecificationHandle)handle, genericContext, genericMethodArguments, genericTypeArguments ?? genericContext?.GetGenericArguments()),
            _ => throw new NotSupportedException($"Unsupported handle kind '{handle.Kind}'."),
        };
    }

    internal MethodBase ResolveMethod(EntityHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments = null, IReadOnlyList<Type>? genericTypeArguments = null)
        => handle.Kind switch
        {
            HandleKind.MethodDefinition => ResolveMethodDefinition((MethodDefinitionHandle)handle),
            HandleKind.MemberReference => ResolveMemberReferenceMethod((MemberReferenceHandle)handle, genericContext, genericMethodArguments, genericTypeArguments),
            HandleKind.MethodSpecification => ResolveMethodSpecification((MethodSpecificationHandle)handle, genericContext, genericMethodArguments, genericTypeArguments),
            _ => throw new NotSupportedException($"Unsupported method handle kind '{handle.Kind}'."),
        };

    internal FieldInfo ResolveField(EntityHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments = null, IReadOnlyList<Type>? genericTypeArguments = null)
        => handle.Kind switch
        {
            HandleKind.FieldDefinition => ResolveFieldDefinition((FieldDefinitionHandle)handle),
            HandleKind.MemberReference => ResolveFieldReference((MemberReferenceHandle)handle, genericContext, genericMethodArguments, genericTypeArguments),
            _ => throw new NotSupportedException($"Unsupported field handle kind '{handle.Kind}'."),
        };

    private Type ResolveTypeReference(TypeReferenceHandle handle)
    {
        var reference = _reader.GetTypeReference(handle);
        if (reference.ResolutionScope.Kind == HandleKind.AssemblyReference)
        {
            var assembly = _assembly.ResolveAssembly((AssemblyReferenceHandle)reference.ResolutionScope);
            if (assembly is null)
            {
                throw new TypeLoadException($"Unable to resolve assembly for type reference '{GetFullTypeName(reference)}'.");
            }

            return assembly.GetType(GetFullTypeName(reference))
                ?? throw new TypeLoadException($"Unable to resolve type '{GetFullTypeName(reference)}'.");
        }

        if (reference.ResolutionScope.Kind == HandleKind.ModuleDefinition)
        {
            return GetType(GetFullTypeName(reference), throwOnError: true, ignoreCase: false)!;
        }

        if (reference.ResolutionScope.Kind == HandleKind.ModuleReference)
        {
            return GetType(GetFullTypeName(reference), throwOnError: true, ignoreCase: false)!;
        }

        throw new NotSupportedException($"Resolution scope '{reference.ResolutionScope.Kind}' is not supported.");
    }

    internal Type GetArrayType(Type elementType, int rank)
        => _arrayTypes.GetOrAdd((elementType, rank), key => new MetadataArrayType(key.elementType, rank));

    internal Type GetByRefType(Type elementType)
        => _byRefTypes.GetOrAdd(elementType, type => new MetadataByRefType(type));

    internal Type GetPointerType(Type elementType)
        => _pointerTypes.GetOrAdd(elementType, type => new MetadataPointerType(type));

    internal Type GetFunctionPointerType(MethodSignature<Type> signature)
        => _functionPointerTypes.GetOrAdd(new FunctionPointerSignatureKey(signature), _ => new MetadataFunctionPointerType(this, signature));

    internal MethodBodyBlock? TryGetMethodBody(int relativeVirtualAddress)
    {
        if (relativeVirtualAddress == 0)
        {
            return null;
        }

        if (_methodBodyProvider is null)
        {
            return null;
        }

        return _methodBodyProvider(relativeVirtualAddress);
    }

    private Type ResolveTypeSpecification(TypeSpecificationHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments, IReadOnlyList<Type>? genericTypeArguments)
    {
        var provider = new MetadataSignatureTypeProvider(this, genericTypeArguments ?? genericContext?.GetGenericArguments(), genericMethodArguments, genericContext);
        var specification = _reader.GetTypeSpecification(handle);
        return specification.DecodeSignature(provider, genericContext);
    }

    private FieldInfo ResolveFieldDefinition(FieldDefinitionHandle handle)
    {
        var definition = _reader.GetFieldDefinition(handle);
        var declaringType = GetOrAddType(definition.GetDeclaringType());
        return declaringType.ResolveField(handle)
            ?? throw new MissingFieldException(declaringType.FullName, _reader.GetString(definition.Name));
    }

    private FieldInfo ResolveFieldReference(MemberReferenceHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments, IReadOnlyList<Type>? genericTypeArguments)
    {
        var reference = _reader.GetMemberReference(handle);
        if (GetMemberReferenceSignatureKind(handle) != SignatureKind.Field)
        {
            throw new MissingFieldException(_reader.GetString(reference.Name));
        }

        var parentType = ResolveType(reference.Parent, genericContext, genericMethodArguments, genericTypeArguments);
        var fieldName = _reader.GetString(reference.Name);
        const BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        var field = parentType.GetField(fieldName, bindingFlags);
        if (field is null)
        {
            throw new MissingFieldException(parentType.FullName, fieldName);
        }

        return field;
    }

    private MethodBase ResolveMethodDefinition(MethodDefinitionHandle handle)
    {
        var definition = _reader.GetMethodDefinition(handle);
        var declaringType = GetOrAddType(definition.GetDeclaringType());
        var name = _reader.GetString(definition.Name);
        if (name is ".ctor" or ".cctor")
        {
            return new MetadataConstructorInfo(declaringType, handle);
        }

        return declaringType.ResolveMethod(handle) ?? new MetadataMethodInfo(declaringType, handle);
    }

    private MethodBase ResolveMemberReferenceMethod(MemberReferenceHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments, IReadOnlyList<Type>? genericTypeArguments)
    {
        var reference = _reader.GetMemberReference(handle);
        if (GetMemberReferenceSignatureKind(handle) == SignatureKind.Field)
        {
            throw new MissingMethodException(_reader.GetString(reference.Name));
        }
        var parentType = ResolveType(reference.Parent, genericContext, genericMethodArguments, genericTypeArguments);
        var name = _reader.GetString(reference.Name);
        var parentMetadataType = parentType as MetadataType;
        var provider = new MetadataSignatureTypeProvider(this, genericTypeArguments ?? parentMetadataType?.GetGenericArguments(), genericMethodArguments, parentMetadataType);
        var signature = reference.DecodeMethodSignature(provider, parentMetadataType);
        var parameterTypes = signature.ParameterTypes.ToArray();
        var bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

        if (name is ".ctor" or ".cctor")
        {
            foreach (var ctor in parentType.GetConstructors(bindingFlags))
            {
                if (ParametersMatch(ctor.GetParameters(), parameterTypes))
                {
                    return ctor;
                }
            }

            throw new MissingMethodException(parentType.FullName, name);
        }

        foreach (var method in parentType.GetMethods(bindingFlags))
        {
            if (method.Name == name && ParametersMatch(method.GetParameters(), parameterTypes))
            {
                return method;
            }
        }

        throw new MissingMethodException(parentType.FullName, name);
    }

    private MethodBase ResolveMethodSpecification(MethodSpecificationHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments, IReadOnlyList<Type>? genericTypeArguments)
    {
        var specification = _reader.GetMethodSpecification(handle);
        var unconstructed = ResolveMethod(specification.Method, genericContext, genericMethodArguments, genericTypeArguments);
        if (unconstructed is not MethodInfo method)
        {
            return unconstructed;
        }

        var provider = new MetadataSignatureTypeProvider(this, genericTypeArguments ?? genericContext?.GetGenericArguments(), genericMethodArguments, genericContext);
        var genericArguments = specification.DecodeSignature(provider, genericContext);
        if (genericArguments.Length == 0)
        {
            return method;
        }

        return method.MakeGenericMethod(genericArguments.ToArray());
    }

    private static bool ParametersMatch(IReadOnlyList<ParameterInfo> parameters, IReadOnlyList<Type> types)
    {
        if (parameters.Count != types.Count)
        {
            return false;
        }

        for (var i = 0; i < parameters.Count; i++)
        {
            if (!Equals(parameters[i].ParameterType, types[i]))
            {
                return false;
            }
        }

        return true;
    }

    private string GetFullTypeName(TypeReference reference)
    {
        var ns = reference.Namespace.IsNil ? null : _reader.GetString(reference.Namespace);
        var name = _reader.GetString(reference.Name);
        return string.IsNullOrEmpty(ns) ? name : ns + "." + name;
    }

    private static string CreateGenericCacheKey(TypeDefinitionHandle handle, IReadOnlyList<Type> typeArguments)
    {
        var argumentNames = string.Join(";", typeArguments.Select(argument => argument.AssemblyQualifiedName ?? argument.FullName ?? argument.Name));
        return $"{handle.GetHashCode()}[{argumentNames}]";
    }

    private SignatureKind GetMemberReferenceSignatureKind(MemberReferenceHandle handle)
    {
        var reference = _reader.GetMemberReference(handle);
        var reader = _reader.GetBlobReader(reference.Signature);
        var header = reader.ReadSignatureHeader();
        return header.Kind;
    }

    private bool IsModuleType(TypeDefinition definition)
    {
        if (!definition.GetDeclaringType().IsNil)
        {
            return false;
        }

        var name = _reader.GetString(definition.Name);
        if (!string.Equals(name, "<Module>", StringComparison.Ordinal))
        {
            return false;
        }

        var ns = definition.Namespace.IsNil ? null : _reader.GetString(definition.Namespace);
        return string.IsNullOrEmpty(ns);
    }

    private readonly struct FunctionPointerSignatureKey : IEquatable<FunctionPointerSignatureKey>
    {
        private readonly byte _header;
        private readonly Type _returnType;
        private readonly ImmutableArray<Type> _parameters;
        private readonly int _requiredParameterCount;
        private readonly int _genericParameterCount;

        public FunctionPointerSignatureKey(MethodSignature<Type> signature)
        {
            _header = signature.Header.RawValue;
            _returnType = signature.ReturnType;
            _parameters = signature.ParameterTypes;
            _requiredParameterCount = signature.RequiredParameterCount;
            _genericParameterCount = signature.GenericParameterCount;
        }

        public bool Equals(FunctionPointerSignatureKey other)
            => _header == other._header
                && _returnType.Equals(other._returnType)
                && _requiredParameterCount == other._requiredParameterCount
                && _genericParameterCount == other._genericParameterCount
                && _parameters.SequenceEqual(other._parameters);

        public override bool Equals(object? obj)
            => obj is FunctionPointerSignatureKey other && Equals(other);

        public override int GetHashCode()
        {
            var hash = HashCode.Combine(_header, _returnType, _requiredParameterCount, _genericParameterCount);
            foreach (var parameter in _parameters)
            {
                hash = HashCode.Combine(hash, parameter);
            }

            return hash;
        }
    }
}
