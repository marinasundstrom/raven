using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TupleElementNamesAttributeEmissionTests
{
    private const string TupleAttributeName = "System.Runtime.CompilerServices.TupleElementNamesAttribute";

    [Fact]
    public void TupleField_EmitsTupleElementNamesAttribute()
    {
        const string source = """
class C
{
    public var Data: (no: int, str: string)
}
""";

        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();

        var typeDef = metadata.TypeDefinitions
            .Select(h => metadata.GetTypeDefinition(h))
            .Single(t => metadata.GetString(t.Name) == "C");
        var fieldHandle = typeDef.GetFields().Single();
        var fieldAttribute = metadata.GetCustomAttributes(fieldHandle)
            .Select(h => (Handle: h, Name: GetAttributeTypeName(metadata, h)))
            .Single(a => a.Name == TupleAttributeName);

        var names = GetTupleElementNames(metadata, fieldAttribute.Handle);
        Assert.Equal(new string?[] { "no", "str" }, names.ToArray());
    }

    [Fact]
    public void TupleParameterAndReturn_EmitTupleElementNamesAttribute()
    {
        const string source = """
class C
{
    public M(x: (no: int, str: string)) -> (flag: bool, text: string)
    {
        return (flag: x.no == 0, text: x.str)
    }
}
""";

        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();

        var typeDef = metadata.TypeDefinitions
            .Select(h => metadata.GetTypeDefinition(h))
            .Single(t => metadata.GetString(t.Name) == "C");
        var methodHandle = typeDef.GetMethods()
            .Single(h => metadata.GetString(metadata.GetMethodDefinition(h).Name) == "M");
        var methodDef = metadata.GetMethodDefinition(methodHandle);
        var parameters = methodDef.GetParameters().ToArray();

        var returnParam = parameters.Single(p => metadata.GetParameter(p).SequenceNumber == 0);
        var returnAttrHandle = metadata.GetCustomAttributes(returnParam)
            .Single(h => GetAttributeTypeName(metadata, h) == TupleAttributeName);
        var returnNames = GetTupleElementNames(metadata, returnAttrHandle);
        Assert.Equal(new string?[] { "flag", "text" }, returnNames.ToArray());

        var parameter = parameters.Single(p => metadata.GetParameter(p).SequenceNumber == 1);
        var parameterAttrHandle = metadata.GetCustomAttributes(parameter)
            .Single(h => GetAttributeTypeName(metadata, h) == TupleAttributeName);
        var parameterNames = GetTupleElementNames(metadata, parameterAttrHandle);
        Assert.Equal(new string?[] { "no", "str" }, parameterNames.ToArray());
    }

    [Fact]
    public void TupleInGenericType_EmitsTupleElementNamesAttribute()
    {
        const string source = """
class C
{
    public M(x: System.Collections.Generic.Dictionary<string, (count: int, name: string)>) -> unit
    {
    }
}
""";

        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();

        var typeDef = metadata.TypeDefinitions
            .Select(h => metadata.GetTypeDefinition(h))
            .Single(t => metadata.GetString(t.Name) == "C");
        var methodHandle = typeDef.GetMethods()
            .Single(h => metadata.GetString(metadata.GetMethodDefinition(h).Name) == "M");
        var methodDef = metadata.GetMethodDefinition(methodHandle);
        var parameterHandle = methodDef.GetParameters()
            .Single(h => metadata.GetParameter(h).SequenceNumber == 1);

        var tupleAttr = metadata.GetCustomAttributes(parameterHandle)
            .Single(h => GetAttributeTypeName(metadata, h) == TupleAttributeName);
        var names = GetTupleElementNames(metadata, tupleAttr);
        Assert.Equal(new string?[] { "count", "name" }, names.ToArray());
    }

    [Fact]
    public void TupleWithoutExplicitNames_DoesNotEmitTupleElementNamesAttribute()
    {
        const string source = """
class C
{
    public M(x: (int, string)) -> (int, string)
    {
        return x
    }
}
""";

        using var peReader = EmitToMetadataReader(source);
        var metadata = peReader.GetMetadataReader();

        var typeDef = metadata.TypeDefinitions
            .Select(h => metadata.GetTypeDefinition(h))
            .Single(t => metadata.GetString(t.Name) == "C");
        var methodHandle = typeDef.GetMethods()
            .Single(h => metadata.GetString(metadata.GetMethodDefinition(h).Name) == "M");
        var methodDef = metadata.GetMethodDefinition(methodHandle);
        var parameters = methodDef.GetParameters().ToArray();

        var returnParam = parameters.Single(p => metadata.GetParameter(p).SequenceNumber == 0);
        Assert.DoesNotContain(metadata.GetCustomAttributes(returnParam), h => GetAttributeTypeName(metadata, h) == TupleAttributeName);

        var parameter = parameters.Single(p => metadata.GetParameter(p).SequenceNumber == 1);
        Assert.DoesNotContain(metadata.GetCustomAttributes(parameter), h => GetAttributeTypeName(metadata, h) == TupleAttributeName);
    }

    private static PEReader EmitToMetadataReader(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        return new PEReader(ImmutableArray.Create(peStream.ToArray()));
    }

    private static ImmutableArray<string?> GetTupleElementNames(MetadataReader metadata, CustomAttributeHandle handle)
    {
        var attribute = metadata.GetCustomAttribute(handle);
        var reader = metadata.GetBlobReader(attribute.Value);
        Assert.Equal(0x0001, reader.ReadUInt16());
        var count = reader.ReadInt32();
        var builder = ImmutableArray.CreateBuilder<string?>(count);
        for (var i = 0; i < count; i++)
            builder.Add(reader.ReadSerializedString());
        return builder.MoveToImmutable();
    }

    private static string GetAttributeTypeName(MetadataReader metadata, CustomAttributeHandle handle)
    {
        var attribute = metadata.GetCustomAttribute(handle);
        var ctor = attribute.Constructor;
        return ctor.Kind switch
        {
            HandleKind.MemberReference =>
                GetAttributeTypeName(metadata, metadata.GetMemberReference((MemberReferenceHandle)ctor).Parent),
            HandleKind.MethodDefinition =>
                GetAttributeTypeName(metadata, metadata.GetMethodDefinition((MethodDefinitionHandle)ctor).GetDeclaringType()),
            _ => string.Empty
        };
    }

    private static string GetAttributeTypeName(MetadataReader metadata, EntityHandle handle)
    {
        return handle.Kind switch
        {
            HandleKind.TypeDefinition =>
                GetFullName(metadata, metadata.GetTypeDefinition((TypeDefinitionHandle)handle).Namespace, metadata.GetTypeDefinition((TypeDefinitionHandle)handle).Name),
            HandleKind.TypeReference =>
                GetFullName(metadata, metadata.GetTypeReference((TypeReferenceHandle)handle).Namespace, metadata.GetTypeReference((TypeReferenceHandle)handle).Name),
            _ => string.Empty
        };
    }

    private static string GetFullName(MetadataReader metadata, StringHandle @namespace, StringHandle name)
    {
        var ns = metadata.GetString(@namespace);
        var typeName = metadata.GetString(name);
        return string.IsNullOrEmpty(ns) ? typeName : $"{ns}.{typeName}";
    }
}
