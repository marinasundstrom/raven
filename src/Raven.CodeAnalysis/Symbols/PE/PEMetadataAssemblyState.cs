using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PEMetadataAssemblyState
{
    private static readonly ConditionalWeakTable<Assembly, Lazy<PEMetadataAssemblyState>> s_states = new();
    private static readonly ConditionalWeakTable<Assembly, Lazy<PEMetadataExtensionMethodState>> s_extensionStates = new();

    private readonly ImmutableDictionary<string, ImmutableArray<Type>> _topLevelTypesByNamespace;
    private readonly ImmutableDictionary<(string NamespaceName, string Name), ImmutableArray<Type>> _topLevelTypesByNamespaceAndName;
    private readonly ImmutableHashSet<string> _namespaces;
    private readonly ConcurrentDictionary<string, Lazy<ImmutableDictionary<string, ImmutableArray<Type>>>> _extensionMethodContainersByNamespace;
    private readonly Lazy<ImmutableArray<Type>> _extensionConversionContainers;
    private readonly Assembly _assembly;

    private PEMetadataAssemblyState(
        Assembly assembly,
        ImmutableDictionary<string, ImmutableArray<Type>> topLevelTypesByNamespace,
        ImmutableDictionary<(string NamespaceName, string Name), ImmutableArray<Type>> topLevelTypesByNamespaceAndName,
        ImmutableHashSet<string> namespaces)
    {
        _assembly = assembly;
        _topLevelTypesByNamespace = topLevelTypesByNamespace;
        _topLevelTypesByNamespaceAndName = topLevelTypesByNamespaceAndName;
        _namespaces = namespaces;
        _extensionMethodContainersByNamespace = new ConcurrentDictionary<string, Lazy<ImmutableDictionary<string, ImmutableArray<Type>>>>(
            StringComparer.Ordinal);
        _extensionConversionContainers = new Lazy<ImmutableArray<Type>>(
            () => CreateExtensionConversionContainers(_assembly),
            LazyThreadSafetyMode.ExecutionAndPublication);
    }

    public static PEMetadataAssemblyState Create(Assembly assembly)
        => s_states.GetValue(
            assembly,
            static key => new Lazy<PEMetadataAssemblyState>(
                () => CreateUncached(key),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;

    public static ImmutableArray<Type> GetExtensionMethodContainers(
        Assembly assembly,
        string namespaceMetadataName,
        string methodName)
        => s_extensionStates.GetValue(
            assembly,
            static key => new Lazy<PEMetadataExtensionMethodState>(
                () => new PEMetadataExtensionMethodState(key),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value.GetExtensionMethodContainers(namespaceMetadataName, methodName);

    private static PEMetadataAssemblyState CreateUncached(Assembly assembly)
    {
        var typesByNamespace = new Dictionary<string, ImmutableArray<Type>.Builder>(StringComparer.Ordinal);
        var typesByNamespaceAndName = new Dictionary<(string NamespaceName, string Name), ImmutableArray<Type>.Builder>();
        var namespaces = ImmutableHashSet.CreateBuilder<string>(StringComparer.Ordinal);
        namespaces.Add(string.Empty);

        var allTypes = GetLoadableTypes(assembly);
        foreach (var type in allTypes)
        {
            var namespaceName = type.Namespace ?? string.Empty;
            AddNamespaceAndParents(namespaces, namespaceName);

            if (type.IsNested)
                continue;

            Add(typesByNamespace, namespaceName, type);
            Add(typesByNamespaceAndName, (namespaceName, StripArity(type.Name)), type);

            if (!string.Equals(type.Name, StripArity(type.Name), StringComparison.Ordinal))
                Add(typesByNamespaceAndName, (namespaceName, type.Name), type);
        }

        return new PEMetadataAssemblyState(
            assembly,
            Freeze(typesByNamespace),
            Freeze(typesByNamespaceAndName),
            namespaces.ToImmutable());
    }

    public ImmutableArray<Type> GetTopLevelTypes(string namespaceMetadataName)
        => _topLevelTypesByNamespace.TryGetValue(namespaceMetadataName, out var types)
            ? types
            : ImmutableArray<Type>.Empty;

    public ImmutableArray<Type> GetTopLevelTypes(string namespaceMetadataName, string name)
        => _topLevelTypesByNamespaceAndName.TryGetValue((namespaceMetadataName, name), out var types)
            ? types
            : ImmutableArray<Type>.Empty;

    public ImmutableArray<Type> GetExtensionMethodContainers(string namespaceMetadataName, string methodName)
        => GetExtensionMethodContainerIndex(namespaceMetadataName).TryGetValue(methodName, out var types)
            ? types
            : ImmutableArray<Type>.Empty;

    public ImmutableArray<Type> GetExtensionConversionContainers()
        => _extensionConversionContainers.Value;

    public bool NamespaceExists(string metadataName)
        => _namespaces.Contains(metadataName);

    public ImmutableArray<string> GetDirectNestedNamespaceNames(string namespaceMetadataName)
    {
        var prefix = string.IsNullOrEmpty(namespaceMetadataName)
            ? string.Empty
            : namespaceMetadataName + ".";
        var builder = ImmutableArray.CreateBuilder<string>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var candidate in _namespaces)
        {
            if (!candidate.StartsWith(prefix, StringComparison.Ordinal) ||
                candidate.Length <= prefix.Length)
            {
                continue;
            }

            var remainder = candidate[prefix.Length..];
            var separator = remainder.IndexOf('.');
            var name = separator >= 0 ? remainder[..separator] : remainder;

            if (seen.Add(name))
                builder.Add(name);
        }

        return builder.ToImmutable();
    }

    private static void AddNamespaceAndParents(ImmutableHashSet<string>.Builder namespaces, string namespaceName)
    {
        namespaces.Add(namespaceName);

        var separator = namespaceName.LastIndexOf('.');
        while (separator >= 0)
        {
            namespaceName = namespaceName[..separator];
            namespaces.Add(namespaceName);
            separator = namespaceName.LastIndexOf('.');
        }
    }

    private static ImmutableArray<Type> GetLoadableTypes(Assembly assembly)
    {
        try
        {
            return assembly.GetTypes().ToImmutableArray();
        }
        catch (ReflectionTypeLoadException ex)
        {
            return ex.Types
                .Where(static type => type is not null)
                .Cast<Type>()
                .ToImmutableArray();
        }
        catch (BadImageFormatException)
        {
            return ImmutableArray<Type>.Empty;
        }
        catch (FileNotFoundException)
        {
            return ImmutableArray<Type>.Empty;
        }
        catch (NotSupportedException)
        {
            return ImmutableArray<Type>.Empty;
        }
        catch (TypeLoadException)
        {
            return ImmutableArray<Type>.Empty;
        }
    }

    private static void Add(Dictionary<string, ImmutableArray<Type>.Builder> map, string key, Type type)
    {
        if (!map.TryGetValue(key, out var builder))
        {
            builder = ImmutableArray.CreateBuilder<Type>();
            map.Add(key, builder);
        }

        builder.Add(type);
    }

    private static void Add<TKey>(Dictionary<TKey, ImmutableArray<Type>.Builder> map, TKey key, Type type)
        where TKey : notnull
    {
        if (!map.TryGetValue(key, out var builder))
        {
            builder = ImmutableArray.CreateBuilder<Type>();
            map.Add(key, builder);
        }

        builder.Add(type);
    }

    private static ImmutableDictionary<string, ImmutableArray<Type>> Freeze(Dictionary<string, ImmutableArray<Type>.Builder> map)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, ImmutableArray<Type>>(StringComparer.Ordinal);
        foreach (var (key, value) in map)
            builder.Add(key, value.ToImmutable());

        return builder.ToImmutable();
    }

    private static ImmutableDictionary<TKey, ImmutableArray<Type>> Freeze<TKey>(Dictionary<TKey, ImmutableArray<Type>.Builder> map)
        where TKey : notnull
    {
        var builder = ImmutableDictionary.CreateBuilder<TKey, ImmutableArray<Type>>();
        foreach (var (key, value) in map)
            builder.Add(key, value.ToImmutable());

        return builder.ToImmutable();
    }

    private ImmutableDictionary<string, ImmutableArray<Type>> GetExtensionMethodContainerIndex(string namespaceMetadataName)
        => _extensionMethodContainersByNamespace.GetOrAdd(
            namespaceMetadataName,
            key => new Lazy<ImmutableDictionary<string, ImmutableArray<Type>>>(
                () => CreateExtensionMethodContainerIndex(_assembly, key, GetTopLevelTypes(key)),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;

    private static ImmutableDictionary<string, ImmutableArray<Type>> CreateExtensionMethodContainerIndex(
        Assembly assembly,
        string namespaceMetadataName,
        ImmutableArray<Type> namespaceTypes)
    {
        if (TryCreateExtensionMethodContainerIndexFromMetadata(
                assembly,
                namespaceMetadataName,
                out var metadataIndex))
        {
            return metadataIndex;
        }

        var map = new Dictionary<string, ImmutableArray<Type>.Builder>(StringComparer.Ordinal);

        foreach (var type in namespaceTypes)
        {
            foreach (var method in GetDeclaredMethodsSafe(type))
            {
                if (!method.IsStatic || !HasExtensionAttributeSafe(method))
                    continue;

                Add(map, method.Name, type);
            }
        }

        return FreezeDistinctTypes(map);
    }

    private static ImmutableDictionary<string, ImmutableArray<Type>> CreateExtensionMethodContainerIndex(
        Assembly assembly,
        string namespaceMetadataName)
    {
        if (TryCreateExtensionMethodContainerIndexFromMetadata(
                assembly,
                namespaceMetadataName,
                out var metadataIndex))
        {
            return metadataIndex;
        }

        var namespaceTypes = GetLoadableTypes(assembly)
            .Where(type => !type.IsNested && string.Equals(type.Namespace ?? string.Empty, namespaceMetadataName, StringComparison.Ordinal))
            .ToImmutableArray();

        return CreateExtensionMethodContainerIndex(assembly, namespaceMetadataName, namespaceTypes);
    }

    private static ImmutableArray<Type> CreateExtensionMethodContainersForName(
        Assembly assembly,
        string namespaceMetadataName,
        string methodName)
    {
        if (TryCreateExtensionMethodContainersForNameFromMetadata(
                assembly,
                namespaceMetadataName,
                methodName,
                out var metadataContainers))
        {
            return metadataContainers;
        }

        var builder = ImmutableArray.CreateBuilder<Type>();
        var seen = new HashSet<Type>();
        foreach (var type in GetLoadableTypes(assembly))
        {
            if (type.IsNested ||
                !string.Equals(type.Namespace ?? string.Empty, namespaceMetadataName, StringComparison.Ordinal))
            {
                continue;
            }

            foreach (var method in GetDeclaredMethodsSafe(type))
            {
                if (!string.Equals(method.Name, methodName, StringComparison.Ordinal) ||
                    !method.IsStatic ||
                    !HasExtensionAttributeSafe(method))
                {
                    continue;
                }

                if (seen.Add(type))
                    builder.Add(type);
                break;
            }
        }

        return builder.ToImmutable();
    }

    private static bool TryCreateExtensionMethodContainersForNameFromMetadata(
        Assembly assembly,
        string namespaceMetadataName,
        string methodName,
        out ImmutableArray<Type> containers)
    {
        containers = ImmutableArray<Type>.Empty;

        string location;
        try
        {
            location = assembly.Location;
        }
        catch (NotSupportedException)
        {
            return false;
        }

        if (string.IsNullOrWhiteSpace(location) || !File.Exists(location))
            return false;

        try
        {
            using var stream = File.OpenRead(location);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
                return false;

            var reader = peReader.GetMetadataReader();
            var builder = ImmutableArray.CreateBuilder<Type>();
            var seen = new HashSet<Type>();

            foreach (var typeHandle in reader.TypeDefinitions)
            {
                var typeDefinition = reader.GetTypeDefinition(typeHandle);
                if (IsNested(typeDefinition.Attributes))
                    continue;

                var typeNamespace = reader.GetString(typeDefinition.Namespace);
                if (!string.Equals(typeNamespace, namespaceMetadataName, StringComparison.Ordinal))
                    continue;

                Type? containerType = null;
                foreach (var methodHandle in typeDefinition.GetMethods())
                {
                    var method = reader.GetMethodDefinition(methodHandle);
                    if (!string.Equals(reader.GetString(method.Name), methodName, StringComparison.Ordinal) ||
                        (method.Attributes & MethodAttributes.Static) == 0 ||
                        !HasExtensionAttribute(reader, method.GetCustomAttributes()))
                    {
                        continue;
                    }

                    containerType ??= ResolveMetadataType(assembly, typeNamespace, reader.GetString(typeDefinition.Name));
                    if (containerType is not null && seen.Add(containerType))
                        builder.Add(containerType);

                    break;
                }
            }

            containers = builder.ToImmutable();
            return true;
        }
        catch (BadImageFormatException)
        {
            return false;
        }
        catch (IOException)
        {
            return false;
        }
        catch (NotSupportedException)
        {
            return false;
        }
    }

    private static bool TryCreateExtensionMethodContainerIndexFromMetadata(
        Assembly assembly,
        string namespaceMetadataName,
        out ImmutableDictionary<string, ImmutableArray<Type>> index)
    {
        index = ImmutableDictionary<string, ImmutableArray<Type>>.Empty;

        string location;
        try
        {
            location = assembly.Location;
        }
        catch (NotSupportedException)
        {
            return false;
        }

        if (string.IsNullOrWhiteSpace(location) || !File.Exists(location))
            return false;

        try
        {
            using var stream = File.OpenRead(location);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
                return false;

            var reader = peReader.GetMetadataReader();
            var map = new Dictionary<string, ImmutableArray<Type>.Builder>(StringComparer.Ordinal);

            foreach (var typeHandle in reader.TypeDefinitions)
            {
                var typeDefinition = reader.GetTypeDefinition(typeHandle);
                if (IsNested(typeDefinition.Attributes))
                    continue;

                var typeNamespace = reader.GetString(typeDefinition.Namespace);
                if (!string.Equals(typeNamespace, namespaceMetadataName, StringComparison.Ordinal))
                    continue;

                Type? containerType = null;
                foreach (var methodHandle in typeDefinition.GetMethods())
                {
                    var method = reader.GetMethodDefinition(methodHandle);
                    if ((method.Attributes & MethodAttributes.Static) == 0 ||
                        !HasExtensionAttribute(reader, method.GetCustomAttributes()))
                    {
                        continue;
                    }

                    containerType ??= ResolveMetadataType(assembly, typeNamespace, reader.GetString(typeDefinition.Name));
                    if (containerType is null)
                        break;

                    Add(map, reader.GetString(method.Name), containerType);
                }
            }

            index = FreezeDistinctTypes(map);
            return true;
        }
        catch (BadImageFormatException)
        {
            return false;
        }
        catch (IOException)
        {
            return false;
        }
        catch (NotSupportedException)
        {
            return false;
        }
    }

    private static bool HasExtensionAttribute(MetadataReader reader, CustomAttributeHandleCollection attributes)
    {
        foreach (var attributeHandle in attributes)
        {
            var attribute = reader.GetCustomAttribute(attributeHandle);
            if (IsExtensionAttributeConstructor(reader, attribute.Constructor))
                return true;
        }

        return false;
    }

    private static ImmutableArray<Type> CreateExtensionConversionContainers(Assembly assembly)
    {
        if (TryCreateExtensionConversionContainersFromMetadata(assembly, out var metadataContainers))
            return metadataContainers;

        var builder = ImmutableArray.CreateBuilder<Type>();
        foreach (var type in GetLoadableTypes(assembly))
        {
            foreach (var method in GetDeclaredMethodsSafe(type))
            {
                if (!method.IsStatic ||
                    !IsConversionOperatorName(method.Name) ||
                    !HasExtensionMarkerAttributeSafe(method))
                {
                    continue;
                }

                builder.Add(type);
                break;
            }
        }

        return builder.ToImmutable();
    }

    private static bool TryCreateExtensionConversionContainersFromMetadata(
        Assembly assembly,
        out ImmutableArray<Type> containers)
    {
        containers = ImmutableArray<Type>.Empty;

        string location;
        try
        {
            location = assembly.Location;
        }
        catch (NotSupportedException)
        {
            return false;
        }

        if (string.IsNullOrWhiteSpace(location) || !File.Exists(location))
            return false;

        try
        {
            using var stream = File.OpenRead(location);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
                return false;

            var reader = peReader.GetMetadataReader();
            var builder = ImmutableArray.CreateBuilder<Type>();

            foreach (var typeHandle in reader.TypeDefinitions)
            {
                var typeDefinition = reader.GetTypeDefinition(typeHandle);
                Type? containerType = null;

                foreach (var methodHandle in typeDefinition.GetMethods())
                {
                    var method = reader.GetMethodDefinition(methodHandle);
                    if ((method.Attributes & MethodAttributes.Static) == 0 ||
                        !IsConversionOperatorName(reader.GetString(method.Name)) ||
                        !HasExtensionMarkerAttribute(reader, method.GetCustomAttributes()))
                    {
                        continue;
                    }

                    var typeNamespace = reader.GetString(typeDefinition.Namespace);
                    containerType ??= ResolveMetadataType(
                        assembly,
                        typeNamespace,
                        reader.GetString(typeDefinition.Name));
                    if (containerType is not null)
                        builder.Add(containerType);

                    break;
                }
            }

            containers = builder.ToImmutable();
            return true;
        }
        catch (BadImageFormatException)
        {
            return false;
        }
        catch (IOException)
        {
            return false;
        }
        catch (NotSupportedException)
        {
            return false;
        }
    }

    private static bool HasExtensionMarkerAttribute(
        MetadataReader reader,
        CustomAttributeHandleCollection attributes)
    {
        foreach (var attributeHandle in attributes)
        {
            var attribute = reader.GetCustomAttribute(attributeHandle);
            if (IsExtensionMarkerAttributeConstructor(reader, attribute.Constructor))
                return true;
        }

        return false;
    }

    private static bool IsExtensionMarkerAttributeConstructor(MetadataReader reader, EntityHandle constructor)
    {
        EntityHandle attributeTypeHandle;
        switch (constructor.Kind)
        {
            case HandleKind.MemberReference:
                attributeTypeHandle = reader.GetMemberReference((MemberReferenceHandle)constructor).Parent;
                break;
            case HandleKind.MethodDefinition:
                attributeTypeHandle = reader.GetMethodDefinition((MethodDefinitionHandle)constructor).GetDeclaringType();
                break;
            default:
                return false;
        }

        return IsExtensionMarkerAttributeType(reader, attributeTypeHandle);
    }

    private static bool IsExtensionMarkerAttributeType(MetadataReader reader, EntityHandle typeHandle)
    {
        string namespaceName;
        string typeName;

        switch (typeHandle.Kind)
        {
            case HandleKind.TypeReference:
                var typeReference = reader.GetTypeReference((TypeReferenceHandle)typeHandle);
                namespaceName = reader.GetString(typeReference.Namespace);
                typeName = reader.GetString(typeReference.Name);
                break;
            case HandleKind.TypeDefinition:
                var typeDefinition = reader.GetTypeDefinition((TypeDefinitionHandle)typeHandle);
                namespaceName = reader.GetString(typeDefinition.Namespace);
                typeName = reader.GetString(typeDefinition.Name);
                break;
            default:
                return false;
        }

        return string.Equals(namespaceName, "System.Runtime.CompilerServices", StringComparison.Ordinal) &&
               typeName is nameof(ExtensionAttribute) or "ExtensionMarkerNameAttribute";
    }

    private static bool IsConversionOperatorName(string name)
        => name is "op_Implicit" or "op_Explicit";

    private static bool IsExtensionAttributeConstructor(MetadataReader reader, EntityHandle constructor)
    {
        EntityHandle attributeTypeHandle;
        switch (constructor.Kind)
        {
            case HandleKind.MemberReference:
                attributeTypeHandle = reader.GetMemberReference((MemberReferenceHandle)constructor).Parent;
                break;
            case HandleKind.MethodDefinition:
                attributeTypeHandle = reader.GetMethodDefinition((MethodDefinitionHandle)constructor).GetDeclaringType();
                break;
            default:
                return false;
        }

        return IsExtensionAttributeType(reader, attributeTypeHandle);
    }

    private static bool IsExtensionAttributeType(MetadataReader reader, EntityHandle typeHandle)
    {
        switch (typeHandle.Kind)
        {
            case HandleKind.TypeReference:
                var typeReference = reader.GetTypeReference((TypeReferenceHandle)typeHandle);
                return IsExtensionAttributeName(
                    reader.GetString(typeReference.Namespace),
                    reader.GetString(typeReference.Name));
            case HandleKind.TypeDefinition:
                var typeDefinition = reader.GetTypeDefinition((TypeDefinitionHandle)typeHandle);
                return IsExtensionAttributeName(
                    reader.GetString(typeDefinition.Namespace),
                    reader.GetString(typeDefinition.Name));
            default:
                return false;
        }
    }

    private static bool IsExtensionAttributeName(string namespaceName, string typeName)
        => string.Equals(namespaceName, "System.Runtime.CompilerServices", StringComparison.Ordinal) &&
           string.Equals(typeName, nameof(ExtensionAttribute), StringComparison.Ordinal);

    private static Type? ResolveMetadataType(Assembly assembly, string namespaceName, string metadataName)
    {
        var fullName = string.IsNullOrEmpty(namespaceName) ? metadataName : namespaceName + "." + metadataName;
        try
        {
            return assembly.GetType(fullName, throwOnError: false, ignoreCase: false);
        }
        catch (BadImageFormatException)
        {
            return null;
        }
        catch (FileNotFoundException)
        {
            return null;
        }
        catch (NotSupportedException)
        {
            return null;
        }
        catch (TypeLoadException)
        {
            return null;
        }
    }

    private static bool IsNested(TypeAttributes attributes)
        => (attributes & TypeAttributes.VisibilityMask) is
            TypeAttributes.NestedPublic or
            TypeAttributes.NestedPrivate or
            TypeAttributes.NestedFamily or
            TypeAttributes.NestedAssembly or
            TypeAttributes.NestedFamANDAssem or
            TypeAttributes.NestedFamORAssem;

    private static IEnumerable<MethodInfo> GetDeclaredMethodsSafe(Type type)
    {
        try
        {
            return type.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.DeclaredOnly);
        }
        catch (BadImageFormatException)
        {
            return [];
        }
        catch (FileNotFoundException)
        {
            return [];
        }
        catch (NotSupportedException)
        {
            return [];
        }
        catch (ReflectionTypeLoadException)
        {
            return [];
        }
        catch (TypeLoadException)
        {
            return [];
        }
    }

    private static bool HasExtensionAttributeSafe(MethodInfo method)
    {
        try
        {
            foreach (var attribute in method.GetCustomAttributesData())
            {
                if (string.Equals(
                        attribute.AttributeType.FullName,
                        typeof(ExtensionAttribute).FullName,
                        StringComparison.Ordinal))
                {
                    return true;
                }
            }
        }
        catch (BadImageFormatException)
        {
        }
        catch (FileNotFoundException)
        {
        }
        catch (NotSupportedException)
        {
        }
        catch (TypeLoadException)
        {
        }

        return false;
    }

    private static bool HasExtensionMarkerAttributeSafe(MethodInfo method)
    {
        try
        {
            foreach (var attribute in method.GetCustomAttributesData())
            {
                if (attribute.AttributeType.FullName is
                    "System.Runtime.CompilerServices.ExtensionAttribute" or
                    "System.Runtime.CompilerServices.ExtensionMarkerNameAttribute")
                {
                    return true;
                }
            }
        }
        catch (BadImageFormatException)
        {
        }
        catch (FileNotFoundException)
        {
        }
        catch (NotSupportedException)
        {
        }
        catch (TypeLoadException)
        {
        }

        return false;
    }

    private static ImmutableDictionary<string, ImmutableArray<Type>> FreezeDistinctTypes(
        Dictionary<string, ImmutableArray<Type>.Builder> map)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, ImmutableArray<Type>>(StringComparer.Ordinal);
        foreach (var (key, value) in map)
        {
            var seen = new HashSet<Type>();
            var types = ImmutableArray.CreateBuilder<Type>();
            foreach (var type in value)
            {
                if (seen.Add(type))
                    types.Add(type);
            }

            builder.Add(key, types.ToImmutable());
        }

        return builder.ToImmutable();
    }

    private static string StripArity(string name)
    {
        var index = name.IndexOf('`');
        return index >= 0 ? name[..index] : name;
    }

    private sealed class PEMetadataExtensionMethodState
    {
        private readonly Assembly _assembly;
        private readonly ConcurrentDictionary<(string NamespaceMetadataName, string MethodName), Lazy<ImmutableArray<Type>>> _containersByNamespaceAndMethod = new();

        public PEMetadataExtensionMethodState(Assembly assembly)
        {
            _assembly = assembly;
        }

        public ImmutableArray<Type> GetExtensionMethodContainers(string namespaceMetadataName, string methodName)
            => _containersByNamespaceAndMethod.GetOrAdd(
                (namespaceMetadataName, methodName),
                static (key, self) => new Lazy<ImmutableArray<Type>>(
                    () => CreateExtensionMethodContainersForName(self._assembly, key.NamespaceMetadataName, key.MethodName),
                    LazyThreadSafetyMode.ExecutionAndPublication),
                this).Value;
    }
}
