using System.Collections.Immutable;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>Opt-in native format-5 emitter for the documented static Int32 source subset.</summary>
/// <remarks>Uses public semantic operations and the existing .NET binding bootstrap. It is not installed in Compilation.Emit.</remarks>
public static class NeoClrCompilationEmitter
{
    private static readonly DiagnosticDescriptor Unsupported = Descriptor("NEOMETA001", "Unsupported native source", "Native emission does not support {0}.");
    private static readonly DiagnosticDescriptor Configuration = Descriptor("NEOMETA002", "Invalid native configuration", "Native emission configuration: {0}.");
    private static readonly DiagnosticDescriptor Encoding = Descriptor("NEOMETA003", "Invalid native graph", "Native metadata encoding failed: {0}.");

    /// <summary>Validates the compilation and configuration, then writes native bytes to a caller-owned stream.</summary>
    /// <param name="compilation">Source trees using the .NET primitive binding bootstrap.</param>
    /// <param name="output">Writable stream; validation failure leaves its bytes and position unchanged.</param>
    /// <param name="options">Explicit output/core identities and compiler-reference bindings.</param>
    /// <returns>Success and preserved compiler diagnostics, or a source/backend diagnostic without output.</returns>
    /// <remarks>Null/unwritable arguments throw. Stream I/O failures propagate and may leave partial output. No stream is closed.</remarks>
    public static NeoClrEmitResult Emit(Compilation compilation, Stream output, NeoClrEmitOptions options)
    {
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(output);
        ArgumentNullException.ThrowIfNull(options);
        if (!output.CanWrite) throw new ArgumentException("Output must be writable", nameof(output));
        var diagnostics = compilation.GetDiagnostics().ToImmutableArray();
        if (diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error && !d.IsSuppressed)) return new(false, diagnostics);
        NeoClrEmitResult Fail(DiagnosticDescriptor descriptor, string detail, Location? location = null)
            => new(false, diagnostics.Add(Diagnostic.Create(descriptor, location ?? Location.None, detail)));
        if (compilation.Options.TargetPlatform != TargetPlatform.DotNet || compilation.Options.OutputKind != OutputKind.ConsoleApplication)
            return Fail(Configuration, "requires the .NET primitive bootstrap and console output");
        if (compilation.SyntaxTrees.Length == 0 || compilation.MacroSyntaxTrees.Length != 0)
            return Fail(Configuration, "requires source trees; macro trees are unsupported");
        if (options.Identity.Name != compilation.AssemblyName || options.Identity.PublicKeyToken.Length != 0 || options.Identity.Flags != 0)
            return Fail(Configuration, "requires matching unsigned output identity");
        if (options.Dependencies.Length > 256) return Fail(Configuration, "too many dependencies");
        var bindings = new List<(IAssemblySymbol Symbol, NeoClrMetadataDependency Dependency)>();
        foreach (var dependency in options.Dependencies)
        {
            if (!compilation.References.Any(r => ReferenceEquals(r, dependency.Reference)))
                return Fail(Configuration, "dependency reference is not registered with this compilation");
            if (!options.CoreLibrary.Equals(dependency.CoreLibrary)) return Fail(Configuration, "dependency core contract mismatch");
            if (compilation.GetAssemblyOrModuleSymbol(dependency.Reference) is not IAssemblySymbol symbol || symbol.Name != dependency.Definition.Name)
                return Fail(Configuration, "dependency snapshot does not match the reference assembly name");
            if (bindings.Any(b => b.Dependency.Definition.Identity.Equals(dependency.Definition.Identity) || SymbolEqualityComparer.Default.Equals(b.Symbol, symbol)))
                return Fail(Configuration, "duplicate dependency identity or assembly symbol");
            bindings.Add((symbol, dependency));
        }
        byte[] image;
        try { image = Int32Emitter.Emit(compilation, options, bindings); }
        catch (UnsupportedInputException error) { return Fail(Unsupported, error.Message, error.Location); }
        catch (InvalidDataException error) { return Fail(Encoding, error.Message); }
        catch (ArgumentException error) { return Fail(Encoding, error.Message); }
        // Keep caller I/O outside diagnostic translation: a broken stream is not a source error.
        output.Write(image);
        return new(true, diagnostics);
    }
    private static DiagnosticDescriptor Descriptor(string id, string title, string message)
        => DiagnosticDescriptor.Create(id, title, "", "", message, "compiler", DiagnosticSeverity.Error, true);
}

internal sealed class UnsupportedInputException(string detail, Location location) : Exception(detail)
{
    internal Location Location { get; } = location;
}
