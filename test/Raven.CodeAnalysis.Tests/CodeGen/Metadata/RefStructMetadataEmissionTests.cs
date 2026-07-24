using System.Collections.Immutable;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class RefStructMetadataEmissionTests
{
    private const string IsByRefLikeAttributeName =
        "System.Runtime.CompilerServices.IsByRefLikeAttribute";
    private const string IsReadOnlyAttributeName =
        "System.Runtime.CompilerServices.IsReadOnlyAttribute";

    [Theory]
    [InlineData("ref struct Buffer {}", 0)]
    [InlineData("ref struct Buffer<T> {}", 1)]
    public void RefStruct_EmitsIsByRefLikeAttribute(string source, int genericParameterCount)
    {
        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();
        var typeHandle = metadata.TypeDefinitions.Single(handle =>
            metadata.GetString(metadata.GetTypeDefinition(handle).Name).StartsWith("Buffer", StringComparison.Ordinal));
        var type = metadata.GetTypeDefinition(typeHandle);

        Assert.Equal(genericParameterCount, type.GetGenericParameters().Count);
        var attributeHandle = Assert.Single(
            type.GetCustomAttributes(),
            handle => GetAttributeTypeName(metadata, handle) == IsByRefLikeAttributeName);
        var attribute = metadata.GetCustomAttribute(attributeHandle);
        var value = metadata.GetBlobReader(attribute.Value);
        Assert.Equal(0x0001, value.ReadUInt16());
        Assert.Equal(0, value.ReadUInt16());
        Assert.Equal(0, value.RemainingBytes);
    }

    [Fact]
    public void OrdinaryStruct_DoesNotEmitIsByRefLikeAttribute()
    {
        using var peReader = EmitToMetadataReader("struct Buffer {}");
        var metadata = peReader.GetMetadataReader();
        var type = metadata.TypeDefinitions
            .Select(metadata.GetTypeDefinition)
            .Single(type => metadata.GetString(type.Name) == "Buffer");

        Assert.DoesNotContain(
            type.GetCustomAttributes(),
            handle => GetAttributeTypeName(metadata, handle) == IsByRefLikeAttributeName);
    }

    [Theory]
    [InlineData("readonly ref struct Buffer {}", 0)]
    [InlineData("readonly ref struct Buffer<T> {}", 1)]
    public void ReadonlyRefStruct_EmitsBothMarkerAttributes(string source, int genericParameterCount)
    {
        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();
        var type = metadata.TypeDefinitions
            .Select(metadata.GetTypeDefinition)
            .Single(type => metadata.GetString(type.Name).StartsWith("Buffer", StringComparison.Ordinal));
        var attributeNames = type.GetCustomAttributes()
            .Select(handle => GetAttributeTypeName(metadata, handle))
            .ToArray();

        Assert.Equal(genericParameterCount, type.GetGenericParameters().Count);
        Assert.Contains(IsByRefLikeAttributeName, attributeNames);
        Assert.Contains(IsReadOnlyAttributeName, attributeNames);
    }

    private static PEReader EmitToMetadataReader(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "lib",
                [tree],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return new PEReader(ImmutableArray.Create(peStream.ToArray()));
    }

    private static string GetAttributeTypeName(MetadataReader metadata, CustomAttributeHandle handle)
    {
        var attribute = metadata.GetCustomAttribute(handle);
        return attribute.Constructor.Kind switch
        {
            HandleKind.MemberReference =>
                GetTypeName(metadata, metadata.GetMemberReference((MemberReferenceHandle)attribute.Constructor).Parent),
            HandleKind.MethodDefinition =>
                GetTypeName(
                    metadata,
                    metadata.GetMethodDefinition((MethodDefinitionHandle)attribute.Constructor).GetDeclaringType()),
            _ => string.Empty,
        };
    }

    private static string GetTypeName(MetadataReader metadata, EntityHandle handle)
    {
        StringHandle namespaceHandle;
        StringHandle nameHandle;
        if (handle.Kind == HandleKind.TypeReference)
        {
            var type = metadata.GetTypeReference((TypeReferenceHandle)handle);
            namespaceHandle = type.Namespace;
            nameHandle = type.Name;
        }
        else
        {
            var type = metadata.GetTypeDefinition((TypeDefinitionHandle)handle);
            namespaceHandle = type.Namespace;
            nameHandle = type.Name;
        }

        var @namespace = metadata.GetString(namespaceHandle);
        var name = metadata.GetString(nameHandle);
        return string.IsNullOrEmpty(@namespace) ? name : $"{@namespace}.{name}";
    }
}
