using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class EmissionBackendTests
{
    [Fact]
    public void ExplicitBackendReceivesValidatedCompilationAndCallerStreams()
    {
        var compilation = Create();
        using var output = new MemoryStream();
        using var debug = new MemoryStream();
        var backend = new RecordingBackend();
        var options = new EmitOptions().WithBackend(backend);
        var result = compilation.Emit(output, debug, options);
        Assert.True(result.Success);
        Assert.Same(compilation, backend.Compilation);
        Assert.Same(output, backend.Output);
        Assert.Same(debug, backend.DebugOutput);
        Assert.Same(options, backend.Options);
        Assert.Equal("BACKEND001", Assert.Single(result.Diagnostics).Id);
        Assert.True(output.CanWrite);
        Assert.True(debug.CanWrite);
        Assert.Equal(1, backend.Calls);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void InvalidCompilationNeverEntersBackend(bool invalidContract)
    {
        var compilation = invalidContract
            ? Create(options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithTargetCoreAssemblyName("Missing.Core"))
            : Create("class Broken { public static func Value() -> int { return missing } }");
        var backend = new RecordingBackend();
        using var output = new MemoryStream();
        output.WriteByte(17);
        var result = compilation.Emit(output, null, new EmitOptions().WithBackend(backend));
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        Assert.Equal(0, backend.Calls);
        Assert.Equal(new byte[] { 17 }, output.ToArray());
        Assert.Equal(1, output.Position);
    }

    [Fact]
    public void SuppliedDiagnosticsCannotBypassResolvedContractChecks()
    {
        var compilation = Create(options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(new("MissingProvider", "Contracts.Info", "Contracts.Context")));
        var backend = new RecordingBackend();
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null, ImmutableArray<Diagnostic>.Empty,
            new EmitOptions().WithBackend(backend));
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Id == "RAVT003");
        Assert.Equal(0, backend.Calls);
        Assert.Equal(0, output.Length);
    }

    [Fact]
    public void OptionCopiesPreserveBackendAndRemovingItRestoresDotNetEmission()
    {
        var backend = new RecordingBackend();
        var identity = new AssemblyName("Some.Core, Version=1.0.0.0");
        var original = new EmitOptions().WithBackend(backend);
        var copy = original.WithTargetCoreLibraryIdentity(identity);
        Assert.Same(backend, copy.Backend);
        Assert.Null(original.TargetCoreLibraryIdentity);
        Assert.Equal(identity.FullName, copy.TargetCoreLibraryIdentity!.FullName);
        Assert.Equal(identity.FullName, copy.WithBackend(null).TargetCoreLibraryIdentity!.FullName);
        using var output = new MemoryStream();
        Assert.True(Create("public static class Example { public static func Value() -> int { return 40 + 2 } }")
            .Emit(output, null, original.WithBackend(null)).Success);
        Assert.Equal(0, backend.Calls);
        using var reader = new System.Reflection.PortableExecutable.PEReader(new MemoryStream(output.ToArray()));
        Assert.True(reader.HasMetadata);
        var assembly = Assembly.Load(output.ToArray());
        Assert.Equal(42, assembly.GetType("Example")!.GetMethod("Value")!.Invoke(null, null));
    }

    private static Compilation Create(string source = "class Example {}", CompilationOptions? options = null)
        => Compilation.Create("BackendTest", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            options ?? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    private sealed class RecordingBackend : ICompilationEmissionBackend
    {
        internal int Calls { get; private set; }
        internal Compilation? Compilation { get; private set; }
        internal Stream? Output { get; private set; }
        internal Stream? DebugOutput { get; private set; }
        internal EmitOptions? Options { get; private set; }

        public EmitResult Emit(Compilation compilation, Stream output, Stream? debugOutput, EmitOptions options)
        {
            Calls++;
            Compilation = compilation;
            Output = output;
            DebugOutput = debugOutput;
            Options = options;
            return new(true, [Diagnostic.Create(DiagnosticDescriptor.Create(
                "BACKEND001", "Backend warning", "", "", "Backend warning", "test", DiagnosticSeverity.Warning, true), Location.None)]);
        }
    }
}
