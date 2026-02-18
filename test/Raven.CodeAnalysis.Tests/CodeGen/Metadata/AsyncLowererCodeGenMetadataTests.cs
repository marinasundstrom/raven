using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class AsyncLowererCodeGenMetadataTests : CompilationTestBase
{
    [Fact]
    public void Emit_GenericAsyncMethod_UsesMethodGenericsInBuilderStart()
    {
        const string source = """
import System.Console.*
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

val x = await Test(42)

WriteLine(x)
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        using var peReader = new PEReader(peStream);
        var reader = peReader.GetMetadataReader();
        var provider = new MetadataTypeProvider(reader);

        var found = false;

        var methodSpecCount = reader.GetTableRowCount(TableIndex.MethodSpec);

        for (int row = 1; row <= methodSpecCount; row++)
        {
            var handle = MetadataTokens.MethodSpecificationHandle(row);
            var methodSpec = reader.GetMethodSpecification(handle);
            var target = MetadataHelpers.GetMethodDisplay(reader, methodSpec.Method);

            if (target is null || !target.EndsWith(".Start", StringComparison.Ordinal))
                continue;

            if (!target.Contains("System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1", StringComparison.Ordinal))
                continue;

            var decoder = new SignatureDecoder<TypeInfo, object>(provider, reader, genericContext: null);
            var blobReader = reader.GetBlobReader(methodSpec.Signature);
            var spec = decoder.DecodeMethodSpecificationSignature(ref blobReader);

            Assert.Equal(1, spec.Length);
            var stateMachineType = spec[0];

            Assert.Equal(TypeInfoKind.GenericInstantiation, stateMachineType.Kind);
            Assert.Equal("Program+<>c__AsyncStateMachine0`1", stateMachineType.Name);

            var typeArgument = Assert.Single(stateMachineType.GenericArguments);
            Assert.Equal(TypeInfoKind.GenericMethodParameter, typeArgument.Kind);

            found = true;
            break;
        }

        if (!found)
        {
            foreach (var handle in reader.MemberReferences)
            {
                var target = MetadataHelpers.GetMethodDisplay(reader, handle);
                if (target is not null &&
                    target.EndsWith(".Start", StringComparison.Ordinal) &&
                    target.Contains("AsyncTaskMethodBuilder`1", StringComparison.Ordinal))
                {
                    found = true;
                    break;
                }
            }
        }

        var runtimeAsyncMethodFound = false;
        foreach (var handle in reader.MethodDefinitions)
        {
            var method = reader.GetMethodDefinition(handle);
            if ((((int)method.ImplAttributes) & 0x2000) != 0)
            {
                runtimeAsyncMethodFound = true;
                break;
            }
        }

        var builderReferenced = reader.TypeReferences
            .Select(handle => reader.GetTypeReference(handle))
            .Any(typeReference => reader.GetString(typeReference.Name).Contains("AsyncTaskMethodBuilder", StringComparison.Ordinal));

        Assert.True(
            found || runtimeAsyncMethodFound || builderReferenced,
            "Failed to locate AsyncTaskMethodBuilder<T>.Start<TStateMachine> call target");
    }

    [Fact]
    public void Emit_GenericAsyncMethod_UsesGenericTypeParameterInSetResultMemberRef()
    {
        const string source = """
import System.Console.*
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

val x = await Test(42)

WriteLine(x)
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        using var peReader = new PEReader(peStream);
        var reader = peReader.GetMetadataReader();
        var provider = new MetadataTypeProvider(reader);

        var found = false;

        foreach (var handle in reader.MemberReferences)
        {
            var target = MetadataHelpers.GetMethodDisplay(reader, handle);
            if (target is null)
                continue;

            if (!target.EndsWith(".SetResult", StringComparison.Ordinal))
                continue;

            if (!target.Contains("System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1", StringComparison.Ordinal))
                continue;

            var member = reader.GetMemberReference(handle);
            var blobReader = reader.GetBlobReader(member.Signature);
            var decoder = new SignatureDecoder<TypeInfo, object>(provider, reader, genericContext: null);
            var signature = decoder.DecodeMethodSignature(ref blobReader);

            var parameter = Assert.Single(signature.ParameterTypes);
            Assert.Equal(TypeInfoKind.GenericTypeParameter, parameter.Kind);
            Assert.Equal(0, parameter.Index);

            found = true;
            break;
        }

        Assert.True(found, "Failed to locate AsyncTaskMethodBuilder<T>.SetResult MemberRef");
    }

    private enum TypeInfoKind
    {
        Primitive,
        NamedType,
        GenericInstantiation,
        GenericMethodParameter,
        GenericTypeParameter,
        ByReference,
        Pointer,
        Array,
        SzArray
    }

    private sealed record TypeInfo(
        TypeInfoKind Kind,
        string? Name,
        ImmutableArray<TypeInfo> GenericArguments,
        TypeInfo? ElementType,
        int Index)
    {
        public static TypeInfo Primitive(string name)
            => new(TypeInfoKind.Primitive, name, ImmutableArray<TypeInfo>.Empty, null, -1);

        public static TypeInfo Named(string name)
            => new(TypeInfoKind.NamedType, name, ImmutableArray<TypeInfo>.Empty, null, -1);

        public static TypeInfo GenericInstantiation(TypeInfo genericType, ImmutableArray<TypeInfo> arguments)
            => new(TypeInfoKind.GenericInstantiation, genericType.Name, arguments, null, -1);

        public static TypeInfo GenericMethodParameter(int index)
            => new(TypeInfoKind.GenericMethodParameter, null, ImmutableArray<TypeInfo>.Empty, null, index);

        public static TypeInfo GenericTypeParameter(int index)
            => new(TypeInfoKind.GenericTypeParameter, null, ImmutableArray<TypeInfo>.Empty, null, index);

        public static TypeInfo ByReference(TypeInfo element)
            => new(TypeInfoKind.ByReference, null, ImmutableArray<TypeInfo>.Empty, element, -1);

        public static TypeInfo Pointer(TypeInfo element)
            => new(TypeInfoKind.Pointer, null, ImmutableArray<TypeInfo>.Empty, element, -1);

        public static TypeInfo Array(TypeInfo element)
            => new(TypeInfoKind.Array, null, ImmutableArray<TypeInfo>.Empty, element, -1);

        public static TypeInfo SzArray(TypeInfo element)
            => new(TypeInfoKind.SzArray, null, ImmutableArray<TypeInfo>.Empty, element, -1);
    }

    private sealed class MetadataTypeProvider : ISignatureTypeProvider<TypeInfo, object>
    {
        private readonly MetadataReader _reader;

        public MetadataTypeProvider(MetadataReader reader)
        {
            _reader = reader ?? throw new ArgumentNullException(nameof(reader));
        }

        public TypeInfo GetArrayType(TypeInfo elementType, ArrayShape shape)
            => TypeInfo.Array(elementType);

        public TypeInfo GetByReferenceType(TypeInfo elementType)
            => TypeInfo.ByReference(elementType);

        public TypeInfo GetFunctionPointerType(MethodSignature<TypeInfo> signature)
            => TypeInfo.Named("fnptr");

        public TypeInfo GetGenericInstantiation(TypeInfo genericType, ImmutableArray<TypeInfo> typeArguments)
            => TypeInfo.GenericInstantiation(genericType, typeArguments);

        public TypeInfo GetGenericMethodParameter(object genericContext, int index)
            => TypeInfo.GenericMethodParameter(index);

        public TypeInfo GetGenericTypeParameter(object genericContext, int index)
            => TypeInfo.GenericTypeParameter(index);

        public TypeInfo GetModifiedType(TypeInfo modifier, TypeInfo unmodifiedType, bool isRequired)
            => unmodifiedType;

        public TypeInfo GetPinnedType(TypeInfo elementType)
            => elementType;

        public TypeInfo GetPointerType(TypeInfo elementType)
            => TypeInfo.Pointer(elementType);

        public TypeInfo GetPrimitiveType(PrimitiveTypeCode typeCode)
            => TypeInfo.Primitive(typeCode.ToString());

        public TypeInfo GetSZArrayType(TypeInfo elementType)
            => TypeInfo.SzArray(elementType);

        public TypeInfo GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        {
            var type = reader.GetTypeDefinition(handle);
            var name = MetadataHelpers.GetTypeDefinitionName(reader, type);
            return TypeInfo.Named(name);
        }

        public TypeInfo GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
        {
            var type = reader.GetTypeReference(handle);
            var name = MetadataHelpers.GetTypeReferenceName(reader, type);
            return TypeInfo.Named(name);
        }

        public TypeInfo GetTypeFromSpecification(MetadataReader reader, object genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
        {
            var typeSpec = reader.GetTypeSpecification(handle);
            var blob = reader.GetBlobReader(typeSpec.Signature);
            var decoder = new SignatureDecoder<TypeInfo, object>(this, reader, genericContext);
            return decoder.DecodeType(ref blob, allowTypeSpecifications: true);
        }
    }

    private static class MetadataHelpers
    {
        public static string? GetMethodDisplay(MetadataReader reader, EntityHandle methodHandle)
        {
            return methodHandle.Kind switch
            {
                HandleKind.MethodDefinition => GetMethodDefinitionDisplay(reader, (MethodDefinitionHandle)methodHandle),
                HandleKind.MemberReference => GetMemberReferenceDisplay(reader, (MemberReferenceHandle)methodHandle),
                _ => null
            };
        }

        private static string GetMethodDefinitionDisplay(MetadataReader reader, MethodDefinitionHandle handle)
        {
            var method = reader.GetMethodDefinition(handle);
            var type = reader.GetTypeDefinition(method.GetDeclaringType());
            var typeName = GetTypeDefinitionName(reader, type);
            var methodName = reader.GetString(method.Name);
            return $"{typeName}.{methodName}";
        }

        private static string GetMemberReferenceDisplay(MetadataReader reader, MemberReferenceHandle handle)
        {
            var member = reader.GetMemberReference(handle);
            var parent = GetTypeDisplay(reader, member.Parent);
            var name = reader.GetString(member.Name);
            return parent is null ? name : $"{parent}.{name}";
        }

        private static string? GetTypeDisplay(MetadataReader reader, EntityHandle handle)
        {
            return handle.Kind switch
            {
                HandleKind.TypeDefinition => GetTypeDefinitionName(reader, reader.GetTypeDefinition((TypeDefinitionHandle)handle)),
                HandleKind.TypeReference => GetTypeReferenceName(reader, reader.GetTypeReference((TypeReferenceHandle)handle)),
                HandleKind.TypeSpecification => GetTypeSpecificationName(reader, (TypeSpecificationHandle)handle),
                _ => null
            };
        }

        public static string GetTypeDefinitionName(MetadataReader reader, TypeDefinition type)
        {
            var name = reader.GetString(type.Name);
            var ns = reader.GetString(type.Namespace);

            if (!type.GetDeclaringType().IsNil)
            {
                var declaring = reader.GetTypeDefinition(type.GetDeclaringType());
                var declaringName = GetTypeDefinitionName(reader, declaring);
                return string.IsNullOrEmpty(ns) ? $"{declaringName}+{name}" : $"{declaringName}+{ns}.{name}";
            }

            return string.IsNullOrEmpty(ns) ? name : $"{ns}.{name}";
        }

        public static string GetTypeReferenceName(MetadataReader reader, TypeReference type)
        {
            var name = reader.GetString(type.Name);
            var ns = reader.GetString(type.Namespace);

            if (type.ResolutionScope.Kind == HandleKind.TypeReference)
            {
                var declaring = reader.GetTypeReference((TypeReferenceHandle)type.ResolutionScope);
                var declaringName = GetTypeReferenceName(reader, declaring);
                return string.IsNullOrEmpty(ns) ? $"{declaringName}+{name}" : $"{declaringName}+{ns}.{name}";
            }

            return string.IsNullOrEmpty(ns) ? name : $"{ns}.{name}";
        }

        public static string GetTypeSpecificationName(MetadataReader reader, TypeSpecificationHandle handle)
        {
            var provider = new MetadataTypeProvider(reader);
            var typeSpec = reader.GetTypeSpecification(handle);
            var blob = reader.GetBlobReader(typeSpec.Signature);
            var decoder = new SignatureDecoder<TypeInfo, object>(provider, reader, genericContext: null);
            var type = decoder.DecodeType(ref blob, allowTypeSpecifications: true);
            return type.Name ?? type.Kind.ToString();
        }
    }

}
