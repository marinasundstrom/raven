using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.Metadata;

internal readonly record struct DotNetInterfaceDeclaration(
    Type Type, ImmutableArray<byte> NullableFlags, bool UniformFlags);

// Image facts belong to the metadata session, not to a Compilation or its symbols.
internal sealed class DotNetInterfaceMetadata
{
    private readonly ConcurrentDictionary<Module, ImmutableArray<byte>> _images = new();
    private readonly ConcurrentDictionary<Type, ImmutableArray<DotNetInterfaceDeclaration>> _interfaces = new();

    internal void Capture(Assembly assembly, byte[] image)
    {
        if (_images.ContainsKey(assembly.ManifestModule))
            return;
        using var stream = new MemoryStream(image, writable: false);
        using var pe = new PEReader(stream);
        _images.TryAdd(assembly.ManifestModule, pe.GetMetadata().GetContent());
    }

    internal ImmutableArray<DotNetInterfaceDeclaration> Read(System.Reflection.TypeInfo type, MetadataLoadContext context)
    {
        if (!_images.TryGetValue(type.Module, out var image))
            return default;
        return _interfaces.GetOrAdd(type, _ => Decode(type, context, image));
    }

    private static ImmutableArray<DotNetInterfaceDeclaration> Decode(
        System.Reflection.TypeInfo type, MetadataLoadContext context, ImmutableArray<byte> image)
    {
        using var metadata = MetadataReaderProvider.FromMetadataImage(image);
        var reader = metadata.GetMetadataReader();
        var definition = reader.GetTypeDefinition((TypeDefinitionHandle)MetadataTokens.Handle(type.MetadataToken));
        var provider = new ReflectionSignatureProvider(context, type.Module);
        var arguments = type.IsGenericType ? type.GetGenericArguments() : Type.EmptyTypes;
        var result = ImmutableArray.CreateBuilder<DotNetInterfaceDeclaration>();
        foreach (var handle in definition.GetInterfaceImplementations())
        {
            var implementation = reader.GetInterfaceImplementation(handle);
            var interfaceType = provider.Resolve(reader, implementation.Interface, arguments);
            var flags = ImmutableArray<byte>.Empty;
            var uniform = false;
            foreach (var attributeHandle in implementation.GetCustomAttributes())
            {
                var attribute = reader.GetCustomAttribute(attributeHandle);
                EntityHandle owner;
                BlobHandle signature;
                if (attribute.Constructor.Kind == HandleKind.MemberReference)
                {
                    var constructor = reader.GetMemberReference((MemberReferenceHandle)attribute.Constructor);
                    owner = constructor.Parent;
                    signature = constructor.Signature;
                }
                else if (attribute.Constructor.Kind == HandleKind.MethodDefinition)
                {
                    var constructor = reader.GetMethodDefinition((MethodDefinitionHandle)attribute.Constructor);
                    owner = constructor.GetDeclaringType();
                    signature = constructor.Signature;
                }
                else
                    continue;

                var attributeName = owner.Kind switch
                {
                    HandleKind.TypeDefinition => reader.GetString(reader.GetTypeDefinition((TypeDefinitionHandle)owner).Name),
                    HandleKind.TypeReference => reader.GetString(reader.GetTypeReference((TypeReferenceHandle)owner).Name),
                    _ => null
                };
                var attributeNamespace = owner.Kind switch
                {
                    HandleKind.TypeDefinition => reader.GetString(reader.GetTypeDefinition((TypeDefinitionHandle)owner).Namespace),
                    HandleKind.TypeReference => reader.GetString(reader.GetTypeReference((TypeReferenceHandle)owner).Namespace),
                    _ => null
                };
                if (attributeName != "NullableAttribute" || attributeNamespace != "System.Runtime.CompilerServices")
                    continue;

                var constructorSignature = reader.GetBlobReader(signature);
                constructorSignature.ReadSignatureHeader();
                if (constructorSignature.ReadCompressedInteger() != 1 ||
                    constructorSignature.ReadSignatureTypeCode() != SignatureTypeCode.Void)
                    continue;
                var parameterCode = constructorSignature.ReadSignatureTypeCode();
                var value = reader.GetBlobReader(attribute.Value);
                if (value.ReadUInt16() != 1)
                    throw new BadImageFormatException("Invalid nullable interface attribute prolog.");
                if (parameterCode == SignatureTypeCode.Byte)
                {
                    flags = [value.ReadByte()];
                    uniform = true;
                }
                else if (parameterCode == SignatureTypeCode.SZArray &&
                    constructorSignature.ReadSignatureTypeCode() == SignatureTypeCode.Byte)
                {
                    var count = value.ReadInt32();
                    if (count >= 0)
                        flags = ImmutableArray.Create(value.ReadBytes(count));
                }
                break;
            }
            result.Add(new(interfaceType, flags, uniform));
        }
        return result.ToImmutable();
    }

    private sealed class ReflectionSignatureProvider(MetadataLoadContext context, Module module)
        : ISignatureTypeProvider<Type, Type[]>
    {
        internal Type Resolve(MetadataReader reader, EntityHandle handle, Type[] arguments) => handle.Kind switch
        {
            HandleKind.TypeDefinition => GetTypeFromDefinition(reader, (TypeDefinitionHandle)handle, 0),
            HandleKind.TypeReference => GetTypeFromReference(reader, (TypeReferenceHandle)handle, 0),
            HandleKind.TypeSpecification => GetTypeFromSpecification(reader, arguments, (TypeSpecificationHandle)handle, 0),
            _ => throw new BadImageFormatException("Invalid interface type handle.")
        };

        public Type GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        {
            var definition = reader.GetTypeDefinition(handle);
            var name = reader.GetString(definition.Name);
            if (!definition.GetDeclaringType().IsNil)
                return GetTypeFromDefinition(reader, definition.GetDeclaringType(), 0)
                    .GetNestedType(name, BindingFlags.Public | BindingFlags.NonPublic)!;
            var ns = reader.GetString(definition.Namespace);
            return module.GetType(ns.Length == 0 ? name : ns + "." + name, throwOnError: true, ignoreCase: false)!;
        }

        public Type GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
        {
            var reference = reader.GetTypeReference(handle);
            var name = reader.GetString(reference.Name);
            var ns = reader.GetString(reference.Namespace);
            var fullName = ns.Length == 0 ? name : ns + "." + name;
            return reference.ResolutionScope.Kind switch
            {
                HandleKind.TypeReference => GetTypeFromReference(reader, (TypeReferenceHandle)reference.ResolutionScope, 0)
                    .GetNestedType(name, BindingFlags.Public | BindingFlags.NonPublic)!,
                HandleKind.AssemblyReference => context.LoadFromAssemblyName(
                    reader.GetAssemblyReference((AssemblyReferenceHandle)reference.ResolutionScope).GetAssemblyName())
                    .GetType(fullName, throwOnError: true, ignoreCase: false)!,
                HandleKind.ModuleDefinition => module.GetType(fullName, throwOnError: true, ignoreCase: false)!,
                HandleKind.ModuleReference => module.Assembly.GetModule(reader.GetString(
                    reader.GetModuleReference((ModuleReferenceHandle)reference.ResolutionScope).Name))!
                    .GetType(fullName, throwOnError: true, ignoreCase: false)!,
                _ => throw new BadImageFormatException("Invalid interface type resolution scope.")
            };
        }

        public Type GetTypeFromSpecification(MetadataReader reader, Type[] genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
            => reader.GetTypeSpecification(handle).DecodeSignature(this, genericContext);
        public Type GetGenericInstantiation(Type genericType, ImmutableArray<Type> typeArguments)
            => genericType.MakeGenericType(typeArguments.ToArray());
        public Type GetGenericTypeParameter(Type[] genericContext, int index) => genericContext[index];
        public Type GetGenericMethodParameter(Type[] genericContext, int index)
            => throw new BadImageFormatException("An interface declaration cannot reference a method type parameter.");
        public Type GetArrayType(Type elementType, ArrayShape shape) => elementType.MakeArrayType(shape.Rank);
        public Type GetSZArrayType(Type elementType) => elementType.MakeArrayType();
        public Type GetByReferenceType(Type elementType) => elementType.MakeByRefType();
        public Type GetPointerType(Type elementType) => elementType.MakePointerType();
        public Type GetModifiedType(Type modifier, Type unmodifiedType, bool isRequired) => unmodifiedType;
        public Type GetPinnedType(Type elementType) => elementType;
        public Type GetFunctionPointerType(MethodSignature<Type> signature)
            => throw new BadImageFormatException("A function pointer is not a valid interface type argument.");
        public Type GetPrimitiveType(PrimitiveTypeCode typeCode)
            => context.CoreAssembly!.GetType("System." + (typeCode switch
            {
                PrimitiveTypeCode.Boolean => "Boolean",
                PrimitiveTypeCode.Char => "Char",
                PrimitiveTypeCode.SByte => "SByte",
                PrimitiveTypeCode.Byte => "Byte",
                PrimitiveTypeCode.Int16 => "Int16",
                PrimitiveTypeCode.UInt16 => "UInt16",
                PrimitiveTypeCode.Int32 => "Int32",
                PrimitiveTypeCode.UInt32 => "UInt32",
                PrimitiveTypeCode.Int64 => "Int64",
                PrimitiveTypeCode.UInt64 => "UInt64",
                PrimitiveTypeCode.Single => "Single",
                PrimitiveTypeCode.Double => "Double",
                PrimitiveTypeCode.IntPtr => "IntPtr",
                PrimitiveTypeCode.UIntPtr => "UIntPtr",
                PrimitiveTypeCode.Object => "Object",
                PrimitiveTypeCode.String => "String",
                PrimitiveTypeCode.TypedReference => "TypedReference",
                PrimitiveTypeCode.Void => "Void",
                _ => throw new BadImageFormatException("Invalid primitive interface type argument.")
            }), throwOnError: true, ignoreCase: false)!;
    }
}
