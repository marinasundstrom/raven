namespace System.Reflection2;

using System;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Linq;

/// <summary>
/// A reflection-only <see cref="Assembly"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataAssembly : Assembly
{
    private readonly MetadataLoadContext _context;
    private readonly MetadataReader _reader;
    private readonly MetadataModule _manifestModule;
    private readonly AssemblyName _name;
    private readonly string? _location;

    internal MetadataAssembly(MetadataLoadContext context, MetadataReader reader, string? location)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _reader = reader ?? throw new ArgumentNullException(nameof(reader));
        _location = location;
        _name = CreateAssemblyName(reader);
        _manifestModule = new MetadataModule(this, reader);
    }

    public MetadataLoadContext LoadContext => _context;

    public MetadataReader Reader => _reader;

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

    public override AssemblyName GetName(bool copiedName) => _name;

    internal Type Resolve(EntityHandle handle)
        => _manifestModule.ResolveType(handle, null);

    internal MetadataAssembly? ResolveAssembly(AssemblyReferenceHandle handle)
    {
        return _context.Resolve(_reader, handle);
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
