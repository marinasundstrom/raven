namespace System.Reflection2;

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Metadata;
using System.Linq;

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

    internal MetadataModule(MetadataAssembly assembly, MetadataReader reader)
    {
        _assembly = assembly ?? throw new ArgumentNullException(nameof(assembly));
        _reader = reader ?? throw new ArgumentNullException(nameof(reader));
        Name = reader.GetString(reader.GetModuleDefinition().Name);
        ScopeName = Name;
        FullyQualifiedName = assembly.Location ?? Name;
        Mvid = reader.GetGuid(reader.GetModuleDefinition().Mvid);
    }

    internal MetadataReader Reader => _reader;

    public override Assembly Assembly => _assembly;

    public override string FullyQualifiedName { get; }

    public override Guid ModuleVersionId => Mvid;

    public override string Name { get; }

    public override string ScopeName { get; }

    internal Guid Mvid { get; }

    public override bool IsResource() => false;

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

    internal MetadataType GetOrAddType(TypeDefinitionHandle handle)
        => _types.GetOrAdd(handle, h => new MetadataType(this, h));

    internal MetadataType ConstructGenericType(MetadataType definition, IReadOnlyList<Type> typeArguments)
    {
        var key = CreateGenericCacheKey(definition.Handle, typeArguments);
        return _constructedTypes.GetOrAdd(key, _ => new MetadataType(this, definition.Handle, typeArguments.ToArray()));
    }

    internal Type ResolveType(EntityHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments = null)
    {
        if (handle.IsNil)
        {
            return typeof(void);
        }

        return handle.Kind switch
        {
            HandleKind.TypeDefinition => GetOrAddType((TypeDefinitionHandle)handle),
            HandleKind.TypeReference => ResolveTypeReference((TypeReferenceHandle)handle),
            HandleKind.TypeSpecification => ResolveTypeSpecification((TypeSpecificationHandle)handle, genericContext, genericMethodArguments),
            _ => throw new NotSupportedException($"Unsupported handle kind '{handle.Kind}'."),
        };
    }

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

    private Type ResolveTypeSpecification(TypeSpecificationHandle handle, MetadataType? genericContext, IReadOnlyList<Type>? genericMethodArguments)
    {
        var provider = new MetadataSignatureTypeProvider(this, genericContext?.GetGenericArguments(), genericMethodArguments);
        var specification = _reader.GetTypeSpecification(handle);
        return specification.DecodeSignature(provider, genericContext);
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
}
