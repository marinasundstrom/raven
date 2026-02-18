using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class PdbSequencePointTests
{
    [Fact]
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

    [Fact]
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

        var kickoff = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Values");

        AssertMethodHasVisibleSequencePoint(pdbReader, kickoff);
        AssertMethodHasVisibleSequencePoint(pdbReader, moveNext);

        peReader.Dispose();
    }

    [Fact]
    public void RuntimeAsyncMethod_HasSequencePoints_WithoutSynthesizedMoveNext()
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

        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeAsync(true);
        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code, options);

        var kickoff = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Work");

        AssertMethodHasVisibleSequencePoint(pdbReader, kickoff);
        AssertNoMethod(metadataReader, static (typeName, methodName) =>
            typeName.Contains("<>c__AsyncStateMachine", StringComparison.Ordinal) &&
            methodName == "MoveNext");

        peReader.Dispose();
    }

    [Fact]
    public void MatchStatementMethod_HasMultipleVisibleSequencePoints()
    {
        var code = """
class C {
    Evaluate(x: int) -> int {
        var result = 0
        match x {
            1 => result = 10
            _ => result = 20
        }

        return result
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Evaluate");

        AssertMethodHasVisibleSequencePoint(pdbReader, method);
        AssertMethodHasAtLeastVisibleSequencePoints(pdbReader, method, minimumVisiblePoints: 2);

        peReader.Dispose();
    }

    [Fact]
    public void TryCatchFinallyMethod_HasMultipleVisibleSequencePoints()
    {
        var code = """
import System.*

class C {
    Compute(v: int) -> int {
        try {
            if v == 0 {
                throw Exception()
            }

            return v
        } catch (Exception e) {
            return -1
        } finally {
            val _ = 0
        }
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Compute");

        AssertMethodHasVisibleSequencePoint(pdbReader, method);
        AssertMethodHasAtLeastVisibleSequencePoints(pdbReader, method, minimumVisiblePoints: 4);

        peReader.Dispose();
    }

    private static (PEReader PeReader, MetadataReader MetadataReader, MetadataReader PdbReader) EmitWithPortablePdb(
        string source,
        CompilationOptions? options = null)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("pdb_spans", options ?? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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
        var points = GetVisibleSequencePoints(pdbReader, methodHandle).ToArray();

        Assert.NotEmpty(points);
    }

    private static void AssertMethodHasAtLeastVisibleSequencePoints(
        MetadataReader pdbReader,
        MethodDefinitionHandle methodHandle,
        int minimumVisiblePoints)
    {
        var points = GetVisibleSequencePoints(pdbReader, methodHandle).ToArray();
        Assert.True(
            points.Length >= minimumVisiblePoints,
            $"Expected at least {minimumVisiblePoints} visible sequence points, found {points.Length}.");
    }

    private static IEnumerable<SequencePoint> GetVisibleSequencePoints(
        MetadataReader pdbReader,
        MethodDefinitionHandle methodHandle)
    {
        var debugHandle = methodHandle.ToDebugInformationHandle();
        var debugInfo = pdbReader.GetMethodDebugInformation(debugHandle);
        return debugInfo.GetSequencePoints().Where(static p => !p.IsHidden);
    }

    private static void AssertNoMethod(MetadataReader metadataReader, Func<string, string, bool> predicate)
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
                    Assert.True(false, $"Unexpected method found: {typeName}.{methodName}");
            }
        }
    }
}
