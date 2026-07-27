using System.Collections.Immutable;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroAssemblyMetadata
{
    private const string CompilerPluginAttributeNamespace = "Raven.CodeAnalysis.Macros";
    private const string CompilerPluginAttributeName = "RavenCompilerPluginAttribute";

    public static bool HasCompilerPluginMarker(string assemblyPath)
    {
        try
        {
            using var stream = File.OpenRead(assemblyPath);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
                return false;

            var reader = peReader.GetMetadataReader();
            if (!reader.IsAssembly)
                return false;

            foreach (var attributeHandle in reader.GetAssemblyDefinition().GetCustomAttributes())
            {
                var attribute = reader.GetCustomAttribute(attributeHandle);
                if (IsCompilerPluginAttribute(reader, attribute.Constructor))
                    return true;
            }
        }
        catch (Exception exception) when (
            exception is BadImageFormatException or
                IOException or
                InvalidOperationException or
                UnauthorizedAccessException)
        {
        }

        return false;
    }

    public static string? GetAssemblyName(string assemblyPath)
    {
        try
        {
            using var stream = File.OpenRead(assemblyPath);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
                return null;

            var reader = peReader.GetMetadataReader();
            return reader.IsAssembly
                ? reader.GetString(reader.GetAssemblyDefinition().Name)
                : null;
        }
        catch (Exception exception) when (IsMetadataReadFailure(exception))
        {
            return null;
        }
    }

    public static ImmutableArray<string> GetReferencedAssemblyNames(string assemblyPath)
    {
        try
        {
            using var stream = File.OpenRead(assemblyPath);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
                return ImmutableArray<string>.Empty;

            var reader = peReader.GetMetadataReader();
            var builder = ImmutableArray.CreateBuilder<string>();
            foreach (var handle in reader.AssemblyReferences)
                builder.Add(reader.GetString(reader.GetAssemblyReference(handle).Name));
            return builder.ToImmutable();
        }
        catch (Exception exception) when (IsMetadataReadFailure(exception))
        {
            return ImmutableArray<string>.Empty;
        }
    }

    private static bool IsMetadataReadFailure(Exception exception)
        => exception is BadImageFormatException or
            IOException or
            InvalidOperationException or
            UnauthorizedAccessException;

    private static bool IsCompilerPluginAttribute(MetadataReader reader, EntityHandle constructor)
    {
        var declaringType = constructor.Kind switch
        {
            HandleKind.MemberReference =>
                reader.GetMemberReference((MemberReferenceHandle)constructor).Parent,
            HandleKind.MethodDefinition =>
                reader.GetMethodDefinition((MethodDefinitionHandle)constructor).GetDeclaringType(),
            _ => default
        };

        return declaringType.Kind switch
        {
            HandleKind.TypeReference => Matches(reader, reader.GetTypeReference((TypeReferenceHandle)declaringType)),
            HandleKind.TypeDefinition => Matches(reader, reader.GetTypeDefinition((TypeDefinitionHandle)declaringType)),
            _ => false
        };
    }

    private static bool Matches(MetadataReader reader, TypeReference type)
        => reader.StringComparer.Equals(type.Namespace, CompilerPluginAttributeNamespace)
            && reader.StringComparer.Equals(type.Name, CompilerPluginAttributeName);

    private static bool Matches(MetadataReader reader, TypeDefinition type)
        => reader.StringComparer.Equals(type.Namespace, CompilerPluginAttributeNamespace)
            && reader.StringComparer.Equals(type.Name, CompilerPluginAttributeName);
}
