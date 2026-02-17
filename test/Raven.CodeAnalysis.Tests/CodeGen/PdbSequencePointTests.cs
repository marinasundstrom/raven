using System;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class PdbSequencePointTests
{
    [Fact(Skip = "Known gap: async sequence points are not emitted into Portable PDB yet.")]
    public void AsyncMethod_KickoffAndMoveNext_HaveSequencePoints()
    {
        var code = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task<int> {
        await Task.Delay(1)
        return 42
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);

        var kickoff = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Work");

        var moveNext = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName.Contains("<>c__AsyncStateMachine", StringComparison.Ordinal) &&
            methodName == "MoveNext");

        AssertMethodHasVisibleSequencePoint(pdbReader, kickoff);
        AssertMethodHasVisibleSequencePoint(pdbReader, moveNext);

        peReader.Dispose();
    }

    [Fact(Skip = "Known gap: iterator sequence points are not emitted into Portable PDB yet.")]
    public void IteratorMoveNext_HasSequencePoints()
    {
        var code = """
import System.Collections.Generic.*

class C {
    Values() -> IEnumerable<int> {
        yield return 1
        yield return 2
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);

        var moveNext = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName.Contains("<>c__Iterator", StringComparison.Ordinal) &&
            methodName == "MoveNext");

        AssertMethodHasVisibleSequencePoint(pdbReader, moveNext);

        peReader.Dispose();
    }

    private static (PEReader PeReader, MetadataReader MetadataReader, MetadataReader PdbReader) EmitWithPortablePdb(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("pdb_spans", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var peStream = new MemoryStream();
        var pdbStream = new MemoryStream();

        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        pdbStream.Position = 0;

        var peReader = new PEReader(peStream, PEStreamOptions.LeaveOpen);
        var metadataReader = peReader.GetMetadataReader();
        var pdbReader = MetadataReaderProvider
            .FromPortablePdbStream(pdbStream, MetadataStreamOptions.LeaveOpen)
            .GetMetadataReader();

        return (peReader, metadataReader, pdbReader);
    }

    private static MethodDefinitionHandle FindMethod(
        MetadataReader metadataReader,
        Func<string, string, bool> predicate)
    {
        foreach (var typeHandle in metadataReader.TypeDefinitions)
        {
            var type = metadataReader.GetTypeDefinition(typeHandle);
            var typeName = metadataReader.GetString(type.Name);

            foreach (var methodHandle in type.GetMethods())
            {
                var method = metadataReader.GetMethodDefinition(methodHandle);
                var methodName = metadataReader.GetString(method.Name);

                if (predicate(typeName, methodName))
                    return methodHandle;
            }
        }

        throw new InvalidOperationException("Expected method definition was not found in emitted metadata.");
    }

    private static void AssertMethodHasVisibleSequencePoint(MetadataReader pdbReader, MethodDefinitionHandle methodHandle)
    {
        var debugHandle = methodHandle.ToDebugInformationHandle();
        var debugInfo = pdbReader.GetMethodDebugInformation(debugHandle);
        var points = debugInfo.GetSequencePoints().ToArray();

        Assert.NotEmpty(points);
        Assert.Contains(points, static point => !point.IsHidden);
    }
}
