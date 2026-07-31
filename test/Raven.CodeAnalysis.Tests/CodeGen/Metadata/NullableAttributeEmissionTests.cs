using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class NullableAttributeEmissionTests
{
    [Fact]
    public void NullableReferenceTypes_EmitNullableAttribute()
    {
        var source = """
class C {
    func M(x: string?) -> string? { return null }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var peReader = new PEReader(ImmutableArray.Create(peStream.ToArray()));
        var md = peReader.GetMetadataReader();
        var typeDef = md.TypeDefinitions
            .Select(h => md.GetTypeDefinition(h))
            .Single(t => md.GetString(t.Name) == "C");
        var methodHandle = typeDef.GetMethods()
            .Single(h => md.GetString(md.GetMethodDefinition(h).Name) == "M");
        var methodDef = md.GetMethodDefinition(methodHandle);
        var parameters = methodDef.GetParameters().ToArray();
        var returnParam = parameters.Single(p => md.GetParameter(p).SequenceNumber == 0);
        var param = parameters.Single(p => md.GetParameter(p).SequenceNumber == 1);

        Assert.Contains(md.GetCustomAttributes(param), h => IsNullableAttribute(md, h));
        Assert.Contains(md.GetCustomAttributes(returnParam), h => IsNullableAttribute(md, h));
    }

    [Fact]
    public void NonNullableReferenceTypes_UseNonNullContextWithoutPositionAttributes()
    {
        var source = """
class C {
    func M(x: string) -> string { return x }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var peReader = new PEReader(ImmutableArray.Create(peStream.ToArray()));
        var md = peReader.GetMetadataReader();
        var typeDef = md.TypeDefinitions
            .Select(h => md.GetTypeDefinition(h))
            .Single(t => md.GetString(t.Name) == "C");
        var methodHandle = typeDef.GetMethods()
            .Single(h => md.GetString(md.GetMethodDefinition(h).Name) == "M");
        var methodDef = md.GetMethodDefinition(methodHandle);
        var parameters = methodDef.GetParameters().ToArray();
        var returnParam = parameters.Single(p => md.GetParameter(p).SequenceNumber == 0);
        var param = parameters.Single(p => md.GetParameter(p).SequenceNumber == 1);

        Assert.DoesNotContain(md.GetCustomAttributes(param), h => IsNullableAttribute(md, h));
        Assert.DoesNotContain(md.GetCustomAttributes(returnParam), h => IsNullableAttribute(md, h));

        var image = peStream.ToArray();
        var references = TestMetadataReferences.Default;
        using (var loaded = TestAssemblyLoader.LoadFromStream(new MemoryStream(image), references))
        {
            var type = loaded.Assembly.GetType("C", throwOnError: true)!;
            var contextAttribute = Assert.Single(
                type.GetCustomAttributesData(),
                attribute => attribute.AttributeType.FullName ==
                    "System.Runtime.CompilerServices.NullableContextAttribute");
            Assert.Equal((byte)1, Assert.Single(contextAttribute.ConstructorArguments).Value);

            var contextAttributeType = contextAttribute.AttributeType;
            var flagField = contextAttributeType.GetField("Flag")!;
            Assert.False(contextAttributeType.IsPublic);
            Assert.True(contextAttributeType.IsSealed);
            Assert.True(flagField.IsPublic);
            Assert.True(flagField.IsInitOnly);
            var usage = contextAttributeType.GetCustomAttribute<AttributeUsageAttribute>()!;
            Assert.False(usage.Inherited);
            Assert.Equal(
                AttributeTargets.Class |
                AttributeTargets.Struct |
                AttributeTargets.Method |
                AttributeTargets.Interface |
                AttributeTargets.Delegate,
                usage.ValidOn);

            var method = type.GetMethod("M")!;
            var nullability = new NullabilityInfoContext();
            Assert.Equal(
                NullabilityState.NotNull,
                nullability.Create(Assert.Single(method.GetParameters())).ReadState);
            Assert.Equal(
                NullabilityState.NotNull,
                nullability.Create(method.ReturnParameter).ReadState);
        }

        var reference = MetadataReference.CreateFromImage(image);
        var consumer = Compilation.Create(
                "consumer",
                [SyntaxTree.ParseText(string.Empty)],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([.. references, reference]);
        var methodSymbol = Assert.Single(consumer.GetTypeByMetadataName("C")!.GetMembers("M").OfType<IMethodSymbol>());
        Assert.False(Assert.Single(methodSymbol.Parameters).Type.IsNullable);
        Assert.False(methodSymbol.ReturnType.IsNullable);
    }

    [Fact]
    public void NestedNullableReferenceTypes_RoundTripThroughDotNetAndRavenMetadataConsumers()
    {
        var source = """
import System.Collections.Generic.*

class C {
    func M(values: List<string?>?) -> Dictionary<string, string?[]>? { return null }
    func R(ref values: List<string?>?) {}
    func V(value: KeyValuePair<string?, int>) {}
}
""";

        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("nullable-nested", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));

        peStream.Position = 0;
        using (var loaded = TestAssemblyLoader.LoadFromStream(peStream, references))
        {
            var nullableAttributeType = loaded.Assembly.GetType(
                "System.Runtime.CompilerServices.NullableAttribute",
                throwOnError: true)!;
            var nullableFlagsField = nullableAttributeType.GetField("NullableFlags")!;
            Assert.False(nullableAttributeType.IsPublic);
            Assert.True(nullableAttributeType.IsSealed);
            Assert.True(nullableFlagsField.IsPublic);
            Assert.True(nullableFlagsField.IsInitOnly);

            var method = loaded.Assembly.GetType("C", throwOnError: true)!.GetMethod("M")!;
            var nullability = new NullabilityInfoContext();
            var parameterInfo = nullability.Create(Assert.Single(method.GetParameters()));
            var returnInfo = nullability.Create(method.ReturnParameter);

            Assert.Equal(NullabilityState.Nullable, parameterInfo.ReadState);
            Assert.Equal(NullabilityState.Nullable, Assert.Single(parameterInfo.GenericTypeArguments).ReadState);

            Assert.Equal(NullabilityState.Nullable, returnInfo.ReadState);
            Assert.Equal(NullabilityState.NotNull, returnInfo.GenericTypeArguments[0].ReadState);
            Assert.Equal(NullabilityState.NotNull, returnInfo.GenericTypeArguments[1].ReadState);
            Assert.Equal(NullabilityState.Nullable, returnInfo.GenericTypeArguments[1].ElementType!.ReadState);

            var byRefInfo = nullability.Create(Assert.Single(loaded.Assembly.GetType("C")!.GetMethod("R")!.GetParameters()));
            Assert.Equal(NullabilityState.Nullable, byRefInfo.ReadState);
            Assert.Equal(NullabilityState.Nullable, Assert.Single(byRefInfo.GenericTypeArguments).ReadState);

            var valueInfo = nullability.Create(Assert.Single(loaded.Assembly.GetType("C")!.GetMethod("V")!.GetParameters()));
            Assert.Equal(NullabilityState.NotNull, valueInfo.ReadState);
            Assert.Equal(NullabilityState.Nullable, valueInfo.GenericTypeArguments[0].ReadState);
            Assert.Equal(NullabilityState.NotNull, valueInfo.GenericTypeArguments[1].ReadState);
        }

        var reference = MetadataReference.CreateFromImage(peStream.ToArray());
        var consumer = Compilation.Create(
                "consumer",
                [SyntaxTree.ParseText(string.Empty)],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([.. references, reference]);
        var methodSymbol = Assert.Single(consumer.GetTypeByMetadataName("C")!.GetMembers("M").OfType<IMethodSymbol>());

        var nullableParameter = Assert.IsType<NullableTypeSymbol>(Assert.Single(methodSymbol.Parameters).Type);
        var parameterType = Assert.IsAssignableFrom<INamedTypeSymbol>(nullableParameter.UnderlyingType);
        Assert.IsType<NullableTypeSymbol>(Assert.Single(parameterType.TypeArguments));

        var nullableReturn = Assert.IsType<NullableTypeSymbol>(methodSymbol.ReturnType);
        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(nullableReturn.UnderlyingType);
        Assert.False(returnType.TypeArguments[0].IsNullable);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(returnType.TypeArguments[1]);
        Assert.True(arrayType.ElementType.IsNullable);

        var byRefMethod = Assert.Single(consumer.GetTypeByMetadataName("C")!.GetMembers("R").OfType<IMethodSymbol>());
        var byRefParameter = Assert.Single(byRefMethod.Parameters);
        Assert.Equal(RefKind.Ref, byRefParameter.RefKind);
        var nullableByRefType = Assert.IsType<NullableTypeSymbol>(byRefParameter.Type);
        var byRefType = Assert.IsAssignableFrom<INamedTypeSymbol>(nullableByRefType.UnderlyingType);
        Assert.IsType<NullableTypeSymbol>(Assert.Single(byRefType.TypeArguments));

        var valueMethod = Assert.Single(consumer.GetTypeByMetadataName("C")!.GetMembers("V").OfType<IMethodSymbol>());
        var valueType = Assert.IsAssignableFrom<INamedTypeSymbol>(Assert.Single(valueMethod.Parameters).Type);
        Assert.IsType<NullableTypeSymbol>(valueType.TypeArguments[0]);
        Assert.False(valueType.TypeArguments[1].IsNullable);
    }

    private static bool IsNullableAttribute(MetadataReader md, CustomAttributeHandle handle)
    {
        var attr = md.GetCustomAttribute(handle);
        var ctor = attr.Constructor;
        string name;
        if (ctor.Kind == HandleKind.MemberReference)
        {
            var mr = md.GetMemberReference((MemberReferenceHandle)ctor);
            var parent = mr.Parent;
            if (parent.Kind == HandleKind.TypeReference)
            {
                var tr = md.GetTypeReference((TypeReferenceHandle)parent);
                name = md.GetString(tr.Namespace) + "." + md.GetString(tr.Name);
            }
            else
            {
                var td = md.GetTypeDefinition((TypeDefinitionHandle)parent);
                name = md.GetString(td.Namespace) + "." + md.GetString(td.Name);
            }
        }
        else
        {
            var mdh = (MethodDefinitionHandle)ctor;
            var td = md.GetTypeDefinition(md.GetMethodDefinition(mdh).GetDeclaringType());
            name = md.GetString(td.Namespace) + "." + md.GetString(td.Name);
        }

        if (name != "System.Runtime.CompilerServices.NullableAttribute")
            return false;

        var reader = md.GetBlobReader(attr.Value);
        if (reader.ReadUInt16() != 0x0001)
            return false;
        var flag = reader.ReadByte();
        return flag == 2;
    }
}
