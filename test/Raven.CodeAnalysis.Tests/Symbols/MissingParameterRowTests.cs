using System;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MissingParameterRowTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void RequiredParametersDoNotAcquireDefaultsFromMissingMetadata(bool parameterRow, bool explicitDefault)
    {
        var tree = SyntaxTree.ParseText("func Test() -> int { return Example.Api.Identity() }");
        var compilation = Compilation.Create("consumer", [tree],
            TestMetadataReferences.Default.Concat([MetadataReference.CreateFromImage(Image(parameterRow, explicitDefault))]).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var type = compilation.GetTypeByMetadataName("Example.Api")!;
        var method = type.GetMembers("Identity").OfType<IMethodSymbol>().Single();
        var parameter = Assert.Single(method.Parameters);
        Assert.Equal(explicitDefault, parameter.HasExplicitDefaultValue);
        Assert.Equal(explicitDefault, parameter.IsOptional);
        if (explicitDefault) Assert.Equal(7, parameter.ExplicitDefaultValue);
        Assert.Equal(!explicitDefault, compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error));
    }

    private static byte[] Image(bool parameterRow, bool explicitDefault)
    {
        var metadata = new MetadataBuilder();
        metadata.AddModule(0, metadata.GetOrAddString("ParameterRows.dll"), metadata.GetOrAddGuid(Guid.NewGuid()), default, default);
        metadata.AddAssembly(metadata.GetOrAddString("ParameterRows"), new(1, 0, 0, 0), default, default, 0, AssemblyHashAlgorithm.None);
        var core = typeof(object).Assembly.GetName();
        var coreRef = metadata.AddAssemblyReference(metadata.GetOrAddString(core.Name!), core.Version!, default, metadata.GetOrAddBlob(core.GetPublicKeyToken()!), 0, default);
        var objectType = metadata.AddTypeReference(coreRef, metadata.GetOrAddString("System"), metadata.GetOrAddString("Object"));
        metadata.AddTypeDefinition(0, default, metadata.GetOrAddString("<Module>"), default, MetadataTokens.FieldDefinitionHandle(1), MetadataTokens.MethodDefinitionHandle(1));
        metadata.AddTypeDefinition(TypeAttributes.Public | TypeAttributes.Abstract | TypeAttributes.Sealed, metadata.GetOrAddString("Example"), metadata.GetOrAddString("Api"), objectType, MetadataTokens.FieldDefinitionHandle(1), MetadataTokens.MethodDefinitionHandle(1));
        if (parameterRow)
        {
            var parameter = metadata.AddParameter(explicitDefault ? ParameterAttributes.Optional | ParameterAttributes.HasDefault : 0, metadata.GetOrAddString("value"), 1);
            if (explicitDefault) metadata.AddConstant(parameter, 7);
        }
        var code = new BlobBuilder(); code.WriteByte(0x02); code.WriteByte(0x2a);
        var bodies = new BlobBuilder();
        var body = new MethodBodyStreamEncoder(bodies).AddMethodBody(new InstructionEncoder(code), 1);
        metadata.AddMethodDefinition(MethodAttributes.Public | MethodAttributes.Static, 0, metadata.GetOrAddString("Identity"), metadata.GetOrAddBlob(new byte[] { 0, 1, 8, 8 }), body, MetadataTokens.ParameterHandle(1));
        var pe = new ManagedPEBuilder(new PEHeaderBuilder(), new MetadataRootBuilder(metadata), bodies, strongNameSignatureSize: 0);
        var image = new BlobBuilder(); pe.Serialize(image); return image.ToArray();
    }
}
