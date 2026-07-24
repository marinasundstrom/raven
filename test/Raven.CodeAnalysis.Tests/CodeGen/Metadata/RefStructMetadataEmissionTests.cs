using System.Collections.Immutable;
using System.Reflection;
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

    [Fact]
    public void RefField_EmitsByRefFieldSignature()
    {
        using var peReader = EmitToMetadataReader("""
            ref struct Buffer {
                field Value: &int
            }
            """);
        var metadata = peReader.GetMetadataReader();
        var type = metadata.TypeDefinitions
            .Select(metadata.GetTypeDefinition)
            .Single(type => metadata.GetString(type.Name) == "Buffer");
        var fieldHandle = Assert.Single(type.GetFields());
        var field = metadata.GetFieldDefinition(fieldHandle);
        var signature = metadata.GetBlobBytes(field.Signature);

        Assert.Equal("Value", metadata.GetString(field.Name));
        Assert.Equal([0x06, 0x10, 0x08], signature);
    }

    [Theory]
    [InlineData("class Buffer<T> where T: allows ref struct {}", "Buffer")]
    [InlineData("func Accept<T>() where T: allows ref struct {}", "Program")]
    public void AllowsRefStruct_EmitsAllowByRefLikeGenericParameterFlag(
        string source,
        string declaringTypeName)
    {
        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();
        var type = metadata.TypeDefinitions
            .Select(metadata.GetTypeDefinition)
            .Single(type => metadata.GetString(type.Name).StartsWith(declaringTypeName, StringComparison.Ordinal));
        var genericParameter = declaringTypeName == "Buffer"
            ? metadata.GetGenericParameter(Assert.Single(type.GetGenericParameters()))
            : metadata.GetGenericParameter(Assert.Single(
                metadata.MethodDefinitions
                    .Select(metadata.GetMethodDefinition)
                    .SelectMany(method => method.GetGenericParameters())));

        Assert.NotEqual(
            0,
            (int)(genericParameter.Attributes & GenericParameterAttributes.AllowByRefLike));
    }

    [Fact]
    public void GenericReadonlyRefStruct_RoundTripsThroughMetadataAndReflection()
    {
        const string source = """
            readonly ref struct Buffer<T> where T: allows ref struct {
                readonly field Value: &int
            }
            """;

        var image = EmitToImage(source);
        var reference = MetadataReference.CreateFromImage(image.ToArray());
        var consumer = Compilation.Create(
                "consumer",
                [SyntaxTree.ParseText(string.Empty)],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([.. TestMetadataReferences.Default, reference]);
        var definition = consumer.GetTypeByMetadataName("Buffer`1");

        Assert.NotNull(definition);
        Assert.True(definition!.IsRefLikeType);
        Assert.True(definition.IsReadOnly);
        Assert.Equal(
            TypeParameterConstraintKind.AllowByRefLike,
            definition.TypeParameters[0].ConstraintKind & TypeParameterConstraintKind.AllowByRefLike);
        Assert.Equal(RefKind.Ref, Assert.Single(definition.GetMembers("Value").OfType<IFieldSymbol>()).RefKind);

        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(
            definition.Construct(consumer.GetSpecialType(SpecialType.System_Int32)));
        Assert.True(constructed.IsRefLikeType);
        Assert.True(constructed.IsReadOnly);
        Assert.Equal(RefKind.Ref, Assert.Single(constructed.GetMembers("Value").OfType<IFieldSymbol>()).RefKind);

        var assembly = Assembly.Load(image.ToArray());
        var reflectedDefinition = assembly.GetType("Buffer`1", throwOnError: true)!;
        Assert.True(reflectedDefinition.IsByRefLike);
        Assert.True(reflectedDefinition.MakeGenericType(typeof(int)).IsByRefLike);
        Assert.NotEqual(
            0,
            (int)(reflectedDefinition.GetGenericArguments()[0].GenericParameterAttributes &
                  GenericParameterAttributes.AllowByRefLike));
        Assert.True(reflectedDefinition.GetField("Value")!.FieldType.IsByRef);
    }

    private static PEReader EmitToMetadataReader(string source)
    {
        return new PEReader(EmitToImage(source));
    }

    private static ImmutableArray<byte> EmitToImage(string source)
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

        return ImmutableArray.Create(peStream.ToArray());
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
