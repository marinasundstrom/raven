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

public class NullableAttributeEmissionTests
{
    [Fact]
    public void NullableReferenceTypes_EmitNullableAttribute()
    {
        var source = """
class C {
    M(x: string?) -> string? { return null }
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
    public void NonNullableReferenceTypes_DoNotEmitNullableAttribute()
    {
        var source = """
class C {
    M(x: string) -> string { return x }
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
    }

    [Fact]
    public void UnionContainingNull_EmitsNullableAttribute()
    {
        var source = """
class C {
    M(x: string | null) -> string | null { return null }
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
