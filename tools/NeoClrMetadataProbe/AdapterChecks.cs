using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class AdapterChecks
{
    internal static void Run(Func<string, Compilation> compile, string source, NeoClrEmitOptions options)
    {
        var conversion = compile(source.Replace("value + 2", "(int)(double)value"));
        var unsupported = Rejected(conversion, options, "NEOMETA001");
        var diagnostic = unsupported.Diagnostics.Single(d => d.Id == "NEOMETA001");
        Check(diagnostic.GetMessage().Contains("lowered expression BoundConversionExpression"), "unsupported operation diagnostic");
        var location = diagnostic.Location;
        Check(location.IsInSource && ReferenceEquals(location.SourceTree, conversion.SyntaxTrees[0]), "unsupported source tree");
        Check(conversion.SyntaxTrees[0].GetRoot().ToFullString().Substring(location.SourceSpan.Start, location.SourceSpan.Length) == "(int)(double)value", "unsupported expression span");
        var broken = compile(source.Replace("MathLibrary.Twice", "MathLibrary.Missing"));
        var binding = Rejected(broken, options);
        Check(binding.Diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error && !d.Id.StartsWith("NEOMETA")), "binding diagnostics preserved");
        var originalErrors = broken.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).Select(d => d.Id);
        Check(originalErrors.SequenceEqual(binding.Diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error).Select(d => d.Id)), "original error identities retained");
        var tooManyParameters = source + "\nfunc Many(" + string.Join(", ", Enumerable.Range(0, 257).Select(i => $"p{i}: int")) + ") -> int { return 0 }";
        Rejected(compile(tooManyParameters), options, "NEOMETA003");
        var good = compile(source);
        Rejected(good, new(new("WrongName", options.Identity.Version), options.CoreLibrary, options.Dependencies), "NEOMETA002");
        Rejected(good, new(options.Identity, options.CoreLibrary, options.Dependencies.Concat(options.Dependencies)), "NEOMETA002");
        Rejected(good, new(options.Identity, new("OtherCore", new Version(1, 0, 0, 0)), options.Dependencies), "NEOMETA002");
        var dependency = options.Dependencies[0];
        Rejected(good, new(options.Identity, options.CoreLibrary,
            [new NeoClrMetadataDependency(MetadataReference.CreateFromFile(((PortableExecutableReference)dependency.Reference).FilePath), dependency.Definition, dependency.CoreLibrary)]), "NEOMETA002");
        Rejected(good, new(options.Identity, options.CoreLibrary, []), "NEOMETA001");
        using var first = new MemoryStream();
        using var second = new MemoryStream();
        Check(NeoClrCompilationEmitter.Emit(good, first, options).Success && first.CanWrite, "successful output remains caller-owned");
        Check(NeoClrCompilationEmitter.Emit(good, second, options).Success && first.ToArray().SequenceEqual(second.ToArray()), "repeat emission is stable");
        using var readOnly = new MemoryStream(new byte[3], writable: false);
        Throws<ArgumentException>(() => NeoClrCompilationEmitter.Emit(good, readOnly, options));
        using var failing = new FailingStream();
        Throws<IOException>(() => NeoClrCompilationEmitter.Emit(good, failing, options));
        using var peOutput = new MemoryStream();
        peOutput.Write(new byte[] { 1, 2, 3, 4 }); peOutput.Position = 2;
        var peFailure = NeoClrCompilationEmitter.EmitMetadataAssembly(conversion, peOutput, options);
        Check(!peFailure.Success && peFailure.Diagnostics.Any(d => d.Id == "NEOMETA001") && peOutput.Position == 2 &&
            peOutput.ToArray().SequenceEqual(new byte[] { 1, 2, 3, 4 }), "PE validation preserves output");
        Throws<ArgumentException>(() => NeoClrCompilationEmitter.EmitMetadataAssembly(good, readOnly, options));
        Throws<IOException>(() => NeoClrCompilationEmitter.EmitMetadataAssembly(good, failing, options));
        using var container = new MemoryStream();
        Check(NeoClrCompilationEmitter.EmitMetadataAssembly(good, container, options).Success && container.CanWrite,
            "PE output remains caller-owned");
        Check(NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.Read(container.ToArray()).SequenceEqual(first.ToArray()),
            "PE and JSON emission preserve identical native payload");
        var backend = new NeoClrEmissionBackend(options);
        using var sharedOutput = new MemoryStream();
        var sharedResult = good.Emit(sharedOutput, null, new EmitOptions().WithBackend(backend));
        Check(sharedResult.Success && NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.Read(sharedOutput.ToArray())
            .SequenceEqual(NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.Read(container.ToArray())), "Compilation.Emit and compatibility wrapper agree");
        foreach (var coreOverride in new[] { false, true })
        {
            using var rejectedOutput = new MemoryStream();
            using var debug = new MemoryStream();
            rejectedOutput.WriteByte(17);
            debug.WriteByte(18);
            var artifactOptions = new EmitOptions().WithBackend(backend);
            if (coreOverride) artifactOptions = artifactOptions.WithTargetCoreLibraryIdentity(new System.Reflection.AssemblyName("Wrong.Core"));
            var rejected = good.Emit(rejectedOutput, coreOverride ? null : debug, artifactOptions);
            Check(!rejected.Success && rejected.Diagnostics.Any(d => d.Id == "NEOMETA002"), "unsupported backend artifact options rejected");
            Check(rejectedOutput.ToArray().SequenceEqual(new byte[] { 17 }) && rejectedOutput.Position == 1 &&
                debug.ToArray().SequenceEqual(new byte[] { 18 }) && debug.Position == 1, "backend option failure preserves both streams");
        }
        Console.WriteLine("PASS shared Compilation.Emit backend, option rejection and wrapper equivalence");
        Console.WriteLine("PASS adapter diagnostics, source locations, configuration, repeat emission and stream contracts");
    }
    private static NeoClrEmitResult Rejected(Compilation compilation, NeoClrEmitOptions options, string? expected = null)
    {
        using var output = new MemoryStream();
        output.Write(new byte[] { 1, 2, 3, 4 }); output.Position = 2;
        var result = NeoClrCompilationEmitter.Emit(compilation, output, options);
        Check(!result.Success, "expected failed result");
        Check(output.CanWrite && output.Position == 2 && output.ToArray().SequenceEqual(new byte[] { 1, 2, 3, 4 }), "failed validation leaves output unchanged");
        if (expected is not null) Check(result.Diagnostics.Any(d => d.Id == expected), "expected diagnostic " + expected + ": " + string.Join("; ", result.Diagnostics));
        return result;
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
    private static void Throws<T>(Action action) where T : Exception
    {
        try { action(); } catch (T) { return; }
        throw new Exception("expected " + typeof(T).Name);
    }
    private sealed class FailingStream : MemoryStream
    {
        public override void Write(ReadOnlySpan<byte> buffer) => throw new IOException("host write failed");
        public override void Write(byte[] buffer, int offset, int count) => throw new IOException("host write failed");
    }
}
