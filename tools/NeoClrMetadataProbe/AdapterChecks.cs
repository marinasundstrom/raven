using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class AdapterChecks
{
    internal static void Run(Func<string, Compilation> compile, string source, NeoClrEmitOptions options)
    {
        var division = compile(source.Replace("value + 2", "value / 2"));
        var unsupported = Rejected(division, options, "NEOMETA001");
        var diagnostic = unsupported.Diagnostics.Single(d => d.Id == "NEOMETA001");
        var location = diagnostic.Location;
        Check(location.IsInSource && ReferenceEquals(location.SourceTree, division.SyntaxTrees[0]), "unsupported source tree");
        Check(division.SyntaxTrees[0].GetRoot().ToFullString().Substring(location.SourceSpan.Start, location.SourceSpan.Length) == "value / 2", "unsupported expression span");
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
