using System;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class MacroPdbSequencePointTests
{
    [Fact]
    public void ParsedMacroExpression_EmitsSequencePointAtAuthoredFragment()
    {
        const string source = """
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run() -> int => raven!{ 40 + 2 }
            }
            """;
        var sourcePath = Path.GetFullPath("macro-fragment-debug.rvn");
        var expressionStart = source.IndexOf("40 + 2", StringComparison.Ordinal);

        using var emitted = EmitWithPortablePdb(
            source,
            sourcePath,
            new MacroReference(typeof(FreestandingMacroCodeGenTests.RavenBodyMacro)));
        var method = FindMethod(emitted.MetadataReader, "Harness", "Run");
        var points = GetVisibleSequencePoints(emitted.PdbReader, method);
        var matchingPoints = points.Where(
            candidate => candidate.StartLine == 4 &&
                candidate.StartColumn == expressionStart - source.LastIndexOf('\n', expressionStart) &&
                candidate.EndColumn == candidate.StartColumn + "40 + 2".Length).ToArray();
        Assert.True(matchingPoints.Length == 1, FormatPoints(points));
        var point = Assert.Single(matchingPoints);

        Assert.Equal(4, point.EndLine);
        Assert.Equal(sourcePath, GetDocumentPath(emitted.PdbReader, point.Document));
    }

    [Fact]
    public void GeneratedMacroWrapper_DoesNotReplaceAuthoredFragmentSequencePoint()
    {
        const string source = """
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run() -> bool => guard!{ unless false }
            }
            """;
        var sourcePath = Path.GetFullPath("macro-generated-wrapper-debug.rvn");
        var expressionStart = source.IndexOf("false", StringComparison.Ordinal);

        using var emitted = EmitWithPortablePdb(
            source,
            sourcePath,
            new MacroReference(typeof(FreestandingMacroCodeGenTests.GuardMacro)));
        var method = FindMethod(emitted.MetadataReader, "Harness", "Run");
        var points = GetVisibleSequencePoints(emitted.PdbReader, method).ToArray();

        Assert.True(
            points.Any(candidate => candidate.StartLine == 4 &&
                candidate.StartColumn == expressionStart - source.LastIndexOf('\n', expressionStart) &&
                candidate.EndColumn == candidate.StartColumn + "false".Length),
            FormatPoints(points));
        Assert.DoesNotContain(points, candidate => candidate.StartColumn < 1 || candidate.EndColumn <= candidate.StartColumn);
    }

    [Fact]
    public void ParsedMacroStatement_PreservesExpressionSequencePoint()
    {
        const string source = """
            import Raven.CodeAnalysis.Tests.Semantics.Macros.*

            class Harness {
                public static func Run() -> int => statement!{ return 42 }
            }
            """;
        var sourcePath = Path.GetFullPath("macro-statement-debug.rvn");
        var expressionStart = source.LastIndexOf("42", StringComparison.Ordinal);

        using var emitted = EmitWithPortablePdb(
            source,
            sourcePath,
            new MacroReference(typeof(Semantics.Macros.FreestandingMacroSemanticTests.StatementBodyMacro)));
        var method = FindMethod(emitted.MetadataReader, "Harness", "Run");
        var points = GetVisibleSequencePoints(emitted.PdbReader, method).ToArray();

        Assert.True(
            points.Any(candidate => candidate.StartLine == 4 &&
                candidate.StartColumn == expressionStart - source.LastIndexOf('\n', expressionStart) &&
                candidate.EndColumn == candidate.StartColumn + "42".Length),
            FormatPoints(points));
    }

    [Fact]
    public void MacroWithoutExecutableFragments_UsesAuthoredInvocationSequencePoint()
    {
        const string source = """
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run() -> int => add!(20, Right: 22)
            }
            """;
        var sourcePath = Path.GetFullPath("macro-invocation-debug.rvn");
        const string invocationText = "add!(20, Right: 22)";
        var invocationStart = source.IndexOf(invocationText, StringComparison.Ordinal);

        using var emitted = EmitWithPortablePdb(
            source,
            sourcePath,
            new MacroReference(typeof(FreestandingMacroCodeGenTests.AddMacro)));
        var method = FindMethod(emitted.MetadataReader, "Harness", "Run");
        var points = GetVisibleSequencePoints(emitted.PdbReader, method).ToArray();

        Assert.True(
            points.Any(candidate => candidate.StartLine == 4 &&
                candidate.StartColumn == invocationStart - source.LastIndexOf('\n', invocationStart) &&
                candidate.EndColumn == candidate.StartColumn + invocationText.Length),
            FormatPoints(points));
    }

    private static EmittedPdb EmitWithPortablePdb(
        string source,
        string sourcePath,
        MacroReference macroReference)
    {
        var syntaxTree = SyntaxTree.ParseText(source, path: sourcePath);
        var compilation = Compilation.Create(
                "macro_pdb_spans",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(macroReference);
        var peStream = new MemoryStream();
        var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        pdbStream.Position = 0;
        var peReader = new PEReader(peStream, PEStreamOptions.LeaveOpen);
        var metadataReader = peReader.GetMetadataReader();
        var pdbProvider = MetadataReaderProvider.FromPortablePdbStream(
            pdbStream,
            MetadataStreamOptions.LeaveOpen);

        return new EmittedPdb(
            peStream,
            pdbStream,
            peReader,
            pdbProvider,
            metadataReader,
            pdbProvider.GetMetadataReader());
    }

    private static MethodDefinitionHandle FindMethod(
        MetadataReader metadataReader,
        string typeName,
        string methodName)
    {
        foreach (var typeHandle in metadataReader.TypeDefinitions)
        {
            var type = metadataReader.GetTypeDefinition(typeHandle);
            if (metadataReader.GetString(type.Name) != typeName)
                continue;

            foreach (var methodHandle in type.GetMethods())
            {
                var method = metadataReader.GetMethodDefinition(methodHandle);
                if (metadataReader.GetString(method.Name) == methodName)
                    return methodHandle;
            }
        }

        throw new InvalidOperationException($"Method '{typeName}.{methodName}' was not emitted.");
    }

    private static SequencePoint[] GetVisibleSequencePoints(
        MetadataReader pdbReader,
        MethodDefinitionHandle methodHandle)
        => pdbReader.GetMethodDebugInformation(methodHandle.ToDebugInformationHandle())
            .GetSequencePoints()
            .Where(static point => !point.IsHidden)
            .ToArray();

    private static string GetDocumentPath(MetadataReader pdbReader, DocumentHandle handle)
        => Path.GetFullPath(pdbReader.GetString(pdbReader.GetDocument(handle).Name));

    private static string FormatPoints(SequencePoint[] points)
        => string.Join(
            Environment.NewLine,
            points.Select(static point =>
                $"{point.StartLine}:{point.StartColumn}-{point.EndLine}:{point.EndColumn}"));

    private sealed class EmittedPdb(
        MemoryStream peStream,
        MemoryStream pdbStream,
        PEReader peReader,
        MetadataReaderProvider pdbProvider,
        MetadataReader metadataReader,
        MetadataReader pdbReader) : IDisposable
    {
        public MetadataReader MetadataReader { get; } = metadataReader;

        public MetadataReader PdbReader { get; } = pdbReader;

        public void Dispose()
        {
            pdbProvider.Dispose();
            peReader.Dispose();
            pdbStream.Dispose();
            peStream.Dispose();
        }
    }
}
