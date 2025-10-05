namespace System.Reflection2;

using System;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Linq;
using System.Collections.Generic;

/// <summary>
/// A reflection-only <see cref="Assembly"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataAssembly : Assembly
{
    private readonly MetadataLoadContext _context;
    private readonly MetadataReader _reader;
    private readonly MetadataModule _manifestModule;
    private readonly Func<int, MethodBodyBlock?>? _methodBodyProvider;
    private readonly AssemblyName _name;
    private readonly string? _location;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;

    internal MetadataAssembly(
        MetadataLoadContext context,
        MetadataReader reader,
        string? location,
        MetadataReaderProvider? provider = null,
        Func<int, MethodBodyBlock?>? methodBodyProvider = null)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _reader = reader ?? throw new ArgumentNullException(nameof(reader));
        _location = location;
        _methodBodyProvider = methodBodyProvider;
        _name = CreateAssemblyName(reader);
        _manifestModule = new MetadataModule(this, reader, methodBodyProvider);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(()
            => MetadataCustomAttributeDecoder.Decode(_manifestModule, reader.GetAssemblyDefinition().GetCustomAttributes(), null, null));
    }

    public MetadataLoadContext LoadContext => _context;

    public MetadataReader Reader => _reader;

    internal Func<int, MethodBodyBlock?>? MethodBodyProvider => _methodBodyProvider;

    public override string Location => _location ?? string.Empty;

    [Obsolete("CodeBase is obsolete.")]
    public override string? CodeBase => _location;

    public override string? FullName => _name.FullName;

    public override Module ManifestModule => _manifestModule;

    public override IEnumerable<Module> Modules => new[] { _manifestModule };

    public override IEnumerable<Type> ExportedTypes => _manifestModule.GetTypes().Where(t => t.IsPublic || t.IsNestedPublic);

    public override IEnumerable<TypeInfo> DefinedTypes => ExportedTypes.Select(t => (TypeInfo)t.GetTypeInfo());

    public override Type[] GetExportedTypes() => ExportedTypes.ToArray();

    public override Type[] GetTypes() => _manifestModule.GetTypes();

    public override Type? GetType(string name, bool throwOnError, bool ignoreCase)
        => _manifestModule.GetType(name, throwOnError, ignoreCase);

    public override IEnumerable<CustomAttributeData> CustomAttributes => _customAttributes.Value;

    public override IList<CustomAttributeData> GetCustomAttributesData() => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
        => throw new NotSupportedException("Materializing assembly attributes is not supported in metadata-only context.");

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
        => throw new NotSupportedException("Materializing assembly attributes is not supported in metadata-only context.");

    public override bool IsDefined(Type attributeType, bool inherit)
    {
        if (attributeType is null)
        {
            throw new ArgumentNullException(nameof(attributeType));
        }

        return _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    }

    public override AssemblyName GetName(bool copiedName) => _name;

    public override AssemblyName[] GetReferencedAssemblies()
    {
        if (_reader.AssemblyReferences.Count == 0)
        {
            return Array.Empty<AssemblyName>();
        }

        var references = new List<AssemblyName>(_reader.AssemblyReferences.Count);
        foreach (var handle in _reader.AssemblyReferences)
        {
            var reference = _reader.GetAssemblyReference(handle);
            var assemblyName = new AssemblyName
            {
                Name = _reader.GetString(reference.Name),
                Version = reference.Version,
                CultureName = reference.Culture.IsNil ? null : _reader.GetString(reference.Culture),
                Flags = reference.Flags.ToAssemblyNameFlags(),
            };

            if (!reference.PublicKeyOrToken.IsNil)
            {
                var blob = _reader.GetBlobContent(reference.PublicKeyOrToken).ToArray();
                if ((reference.Flags & AssemblyFlags.PublicKey) != 0)
                {
                    assemblyName.SetPublicKey(blob);
                }
                else
                {
                    assemblyName.SetPublicKeyToken(blob);
                }
            }

            references.Add(assemblyName);
        }

        return references.ToArray();
    }

    internal Type Resolve(EntityHandle handle)
        => _manifestModule.ResolveType(handle, null);

    internal MetadataAssembly? ResolveAssembly(AssemblyReferenceHandle handle)
    {
        return _context.Resolve(_reader, handle);
    }

    public override string[] GetManifestResourceNames()
    {
        if (_reader.ManifestResources.Count == 0)
        {
            return Array.Empty<string>();
        }

        var names = new string[_reader.ManifestResources.Count];
        var index = 0;
        foreach (var handle in _reader.ManifestResources)
        {
            var resource = _reader.GetManifestResource(handle);
            names[index++] = _reader.GetString(resource.Name);
        }

        return names;
    }

    public override ManifestResourceInfo? GetManifestResourceInfo(string resourceName)
    {
        if (resourceName is null)
        {
            throw new ArgumentNullException(nameof(resourceName));
        }

        foreach (var handle in _reader.ManifestResources)
        {
            var resource = _reader.GetManifestResource(handle);
            var name = _reader.GetString(resource.Name);
            if (!string.Equals(name, resourceName, StringComparison.Ordinal))
            {
                continue;
            }

            return CreateManifestResourceInfo(resource);
        }

        return null;
    }

    public override Stream? GetManifestResourceStream(string name)
    {
        if (name is null)
        {
            throw new ArgumentNullException(nameof(name));
        }

        foreach (var handle in _reader.ManifestResources)
        {
            var resource = _reader.GetManifestResource(handle);
            var resourceName = _reader.GetString(resource.Name);
            if (!string.Equals(resourceName, name, StringComparison.Ordinal))
            {
                continue;
            }

            throw new NotSupportedException("Reading manifest resource streams is not supported in metadata-only context.");
        }

        return null;
    }

    public override Stream? GetManifestResourceStream(Type type, string name)
    {
        if (type is null)
        {
            throw new ArgumentNullException(nameof(type));
        }

        if (name is null)
        {
            throw new ArgumentNullException(nameof(name));
        }

        var scopedName = type.Namespace is null ? name : type.Namespace + "." + name;
        return GetManifestResourceStream(scopedName);
    }

    private static AssemblyName CreateAssemblyName(MetadataReader reader)
    {
        var definition = reader.GetAssemblyDefinition();
        var name = reader.GetString(definition.Name);
        var culture = definition.Culture.IsNil ? null : reader.GetString(definition.Culture);
        var version = definition.Version;
        var publicKey = definition.PublicKey.IsNil ? null : reader.GetBlobContent(definition.PublicKey).ToArray();

        var assemblyName = new AssemblyName
        {
            Name = name,
            CultureName = culture,
            Version = version,
        };

        if (publicKey is not null && publicKey.Length > 0)
        {
            assemblyName.SetPublicKey(publicKey);
        }

        assemblyName.Flags = definition.Flags.ToAssemblyNameFlags();
        return assemblyName;
    }

    private ManifestResourceInfo CreateManifestResourceInfo(ManifestResource resource)
    {
        var implementation = resource.Implementation;
        ResourceLocation location = 0;
        Assembly? containingAssembly = this;
        string? containingFile = null;

        if (implementation.IsNil)
        {
            location = ResourceLocation.Embedded | ResourceLocation.ContainedInManifestFile;
        }
        else
        {
            switch (implementation.Kind)
            {
                case HandleKind.AssemblyFile:
                {
                    var file = _reader.GetAssemblyFile((AssemblyFileHandle)implementation);
                    containingFile = _reader.GetString(file.Name);
                    location = ResourceLocation.ContainedInManifestFile;
                    break;
                }
                case HandleKind.ModuleReference:
                {
                    var module = _reader.GetModuleReference((ModuleReferenceHandle)implementation);
                    containingFile = _reader.GetString(module.Name);
                    location = ResourceLocation.ContainedInManifestFile;
                    break;
                }
                case HandleKind.AssemblyReference:
                {
                    containingAssembly = ResolveAssembly((AssemblyReferenceHandle)implementation);
                    location = ResourceLocation.ContainedInAnotherAssembly;
                    break;
                }
                default:
                    throw new NotSupportedException($"Manifest resource implementation '{implementation.Kind}' is not supported.");
            }
        }

        return new ManifestResourceInfo(containingAssembly, containingFile, location);
    }
}

internal static class AssemblyFlagExtensions
{
    public static AssemblyNameFlags ToAssemblyNameFlags(this AssemblyFlags flags)
    {
        AssemblyNameFlags result = AssemblyNameFlags.None;
        if ((flags & AssemblyFlags.PublicKey) != 0)
        {
            result |= AssemblyNameFlags.PublicKey;
        }

        if ((flags & AssemblyFlags.Retargetable) != 0)
        {
            result |= AssemblyNameFlags.Retargetable;
        }

        return result;
    }
}
