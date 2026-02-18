using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class DiscriminatedUnionGenericsTests
{
    private const string GenericUnionSample = """
import System.*

val ok : Result<int, string> = .Ok(99)
val err = Result<int, string>.Error("boom")

Console.WriteLine(format(ok))
Console.WriteLine(format(err))


func format(result: Result<int, string>) -> string {
    return result match {
        .Ok(val value) => "ok ${value}"
        .Error(val message) => "error '${message}'"
    }
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}
""";

    [Fact]
    public void GenericUnionPattern_EmitsClosedCaseTypes()
    {
        var syntaxTree = SyntaxTree.ParseText(GenericUnionSample, path: "discriminated-unions-generics.rav");
        var compilation = Compilation.Create(
                "discriminated-unions-generics",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var typeNames = loaded.Assembly.GetTypes().Select(type => type.FullName ?? type.Name).ToArray();

        Assert.Contains(typeNames, name => name.Contains("Result`2+Ok", StringComparison.Ordinal));
        Assert.Contains(typeNames, name => name.Contains("Result`2+Error", StringComparison.Ordinal));
    }

    [Fact]
    public void GenericUnionPattern_RunsSuccessfully()
    {
        var syntaxTree = SyntaxTree.ParseText(GenericUnionSample, path: "discriminated-unions-generics.rav");
        var compilation = Compilation.Create(
                "discriminated-unions-generics",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint!;

        using var writer = new StringWriter();
        var originalOut = Console.Out;
        Console.SetOut(writer);

        try
        {
            var args = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };

            entryPoint.Invoke(null, args);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString().ReplaceLineEndings("\n").TrimEnd('\n');
        Assert.Equal("ok 99\nerror 'boom'", output);
    }

    private static List<string> DecodeTypeSpecificationNames(MetadataReader metadata)
    {
        var provider = new TypeNameProvider(metadata);
        var names = new List<string>(metadata.GetTableRowCount(TableIndex.TypeSpec));

        var typeSpecCount = metadata.GetTableRowCount(TableIndex.TypeSpec);
        for (var row = 1; row <= typeSpecCount; row++)
        {
            var handle = MetadataTokens.TypeSpecificationHandle(row);
            var typeSpec = metadata.GetTypeSpecification(handle);
            var blobReader = metadata.GetBlobReader(typeSpec.Signature);
            var decoder = new SignatureDecoder<string, object?>(provider, metadata, genericContext: null);
            var name = decoder.DecodeType(ref blobReader, allowTypeSpecifications: true);
            names.Add(name);
        }

        return names;
    }

    private sealed class TypeNameProvider : ISignatureTypeProvider<string, object?>
    {
        private readonly MetadataReader _reader;

        public TypeNameProvider(MetadataReader reader)
        {
            _reader = reader;
        }

        public string GetArrayType(string elementType, ArrayShape shape)
        {
            var commas = new string(',', shape.Rank - 1);
            return $"{elementType}[{commas}]";
        }

        public string GetByReferenceType(string elementType) => $"{elementType}&";

        public string GetFunctionPointerType(MethodSignature<string> signature) => "fnptr";

        public string GetGenericInstantiation(string genericType, ImmutableArray<string> typeArguments)
            => $"{genericType}[{string.Join(", ", typeArguments)}]";

        public string GetGenericMethodParameter(object? genericContext, int index) => $"!!{index}";

        public string GetGenericTypeParameter(object? genericContext, int index) => $"!{index}";

        public string GetModifiedType(string modifier, string unmodifiedType, bool isRequired)
            => unmodifiedType;

        public string GetPinnedType(string elementType) => elementType;

        public string GetPointerType(string elementType) => $"{elementType}*";

        public string GetPrimitiveType(PrimitiveTypeCode typeCode) => typeCode.ToString();

        public string GetSZArrayType(string elementType) => $"{elementType}[]";

        public string GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        {
            var definition = reader.GetTypeDefinition(handle);
            return BuildQualifiedName(
                reader.GetString(definition.Namespace),
                reader.GetString(definition.Name),
                definition.GetDeclaringType());
        }

        public string GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
        {
            var reference = reader.GetTypeReference(handle);
            return BuildQualifiedName(
                reader.GetString(reference.Namespace),
                reader.GetString(reference.Name),
                reference.ResolutionScope);
        }

        public string GetTypeFromSpecification(MetadataReader reader, object? genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
        {
            var spec = reader.GetTypeSpecification(handle);
            var blobReader = reader.GetBlobReader(spec.Signature);
            var decoder = new SignatureDecoder<string, object?>(this, reader, genericContext);
            return decoder.DecodeType(ref blobReader, allowTypeSpecifications: true);
        }

        private string BuildQualifiedName(string @namespace, string name, EntityHandle declaringType)
        {
            var declaringPrefix = declaringType.IsNil ? string.Empty : $"{GetDeclaringTypeName(declaringType)}+";
            var qualified = $"{declaringPrefix}{name}";
            return string.IsNullOrEmpty(@namespace) ? qualified : $"{@namespace}.{qualified}";
        }

        private string GetDeclaringTypeName(EntityHandle handle)
        {
            return handle.Kind switch
            {
                HandleKind.TypeDefinition => GetTypeFromDefinition(_reader, (TypeDefinitionHandle)handle, rawTypeKind: 0),
                HandleKind.TypeReference => GetTypeFromReference(_reader, (TypeReferenceHandle)handle, rawTypeKind: 0),
                _ => string.Empty
            };
        }
    }
}
