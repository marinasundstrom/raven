using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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
    async func Work() -> Task<int> {
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
    func Values() -> IEnumerable<int> {
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
    async func Work() -> Task<int> {
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
    func Evaluate(x: int) -> int {
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
    func Compute(v: int) -> int {
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

    [Fact]
    public void ForLoopMethod_HasSequencePointsInsideLoopBody()
    {
        var code = """
class C {
    func Sum() -> int {
        var total = 0
        for value in 1..3 {
            total = total + value
        }

        return total
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Sum");

        AssertMethodHasVisibleSequencePoint(pdbReader, method);
        AssertMethodHasVisibleSequencePointOnLine(pdbReader, method, line: 5);

        peReader.Dispose();
    }

    [Fact]
    public void GotoMethod_HasSequencePointOnGotoStatement()
    {
        var code = """
class C {
    func Retry() -> int {
        var retries = 0
retry:
        retries += 1
        if retries < 3 {
            goto retry
        }

        return retries
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Retry");

        AssertMethodHasVisibleSequencePoint(pdbReader, method);
        AssertMethodHasVisibleSequencePointOnLine(pdbReader, method, line: 7);

        peReader.Dispose();
    }

    [Fact]
    public void FunctionMethod_HasSequencePointsInsideBody_ForStepIn()
    {
        var code = """
class C {
    func Compute(x: int) -> int {
        val adjusted = x + 1
        return adjusted
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Compute");

        AssertMethodHasVisibleSequencePoint(pdbReader, method);
        AssertMethodHasVisibleSequencePointOnLine(pdbReader, method, line: 3);
        AssertMethodHasVisibleSequencePointOnLine(pdbReader, method, line: 4);

        peReader.Dispose();
    }

    [Fact]
    public void FunctionMethod_DoesNotEmitWideBlockSequencePoints()
    {
        var code = """
class C {
    func Compute() -> () {
        val x = 1
        val y = x + 2
        val z = y + 3
    }
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code);
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "C" && methodName == "Compute");

        AssertMethodHasVisibleSequencePoint(pdbReader, method);
        AssertMethodHasNoWideVisibleSequencePoint(pdbReader, method, maximumLineSpan: 2);

        peReader.Dispose();
    }

    [Fact]
    public void TopLevelMain_VisibleSequencePointsStayOnExecutableStatements()
    {
        var code = """
import System.*

func Main() {
    Console.WriteLine("Hello, World!")

    val x = "Foo"

    Console.WriteLine("Hello, World!")
}
""";

        var (peReader, metadataReader, pdbReader) = EmitWithPortablePdb(code, new CompilationOptions(OutputKind.ConsoleApplication));
        var method = FindMethod(metadataReader, static (typeName, methodName) =>
            typeName == "Program" && methodName == "Main");

        var points = GetVisibleSequencePoints(pdbReader, method).ToArray();
        var lines = points.Select(static p => p.StartLine).Distinct().ToArray();

        Assert.Contains(4, lines);
        Assert.Contains(6, lines);
        Assert.Contains(8, lines);
        Assert.DoesNotContain(3, lines);
        Assert.Contains(points, static p => p.StartLine == 4 && p.StartColumn == 5);
        Assert.Contains(points, static p => p.StartLine == 8 && p.StartColumn == 5);

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

    private static void AssertMethodHasVisibleSequencePointOnLine(
        MetadataReader pdbReader,
        MethodDefinitionHandle methodHandle,
        int line)
    {
        var points = GetVisibleSequencePoints(pdbReader, methodHandle).ToArray();
        Assert.Contains(points, p => p.StartLine == line);
    }

    private static void AssertMethodHasNoWideVisibleSequencePoint(
        MetadataReader pdbReader,
        MethodDefinitionHandle methodHandle,
        int maximumLineSpan)
    {
        var points = GetVisibleSequencePoints(pdbReader, methodHandle).ToArray();
        var widePoint = points.FirstOrDefault(point => point.EndLine - point.StartLine > maximumLineSpan);
        if (!widePoint.Equals(default(SequencePoint)))
        {
            Assert.True(
                false,
                $"Unexpected wide sequence point found: {widePoint.StartLine}:{widePoint.StartColumn}-{widePoint.EndLine}:{widePoint.EndColumn}");
        }
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
