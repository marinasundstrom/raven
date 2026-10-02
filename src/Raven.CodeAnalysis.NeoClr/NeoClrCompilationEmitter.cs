using System.Collections.Immutable;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>Opt-in native format-5 emitter for the documented static primitive source subset.</summary>
/// <remarks>Reuses compiler-lowered bodies with the host bootstrap or a validated neoCLR CLI declaration core. Uses the shared Compilation.Emit pipeline through an explicit backend.</remarks>
public static class NeoClrCompilationEmitter
{
    private static readonly DiagnosticDescriptor Unsupported = Descriptor("NEOMETA001", "Unsupported native source", "Native emission does not support {0}.");
    private static readonly DiagnosticDescriptor Configuration = Descriptor("NEOMETA002", "Invalid native configuration", "Native emission configuration: {0}.");
    private static readonly DiagnosticDescriptor Encoding = Descriptor("NEOMETA003", "Invalid native graph", "Native metadata encoding failed: {0}.");

    /// <summary>Validates the compilation and configuration, then writes native bytes to a caller-owned stream.</summary>
    /// <param name="compilation">Source trees using the host bootstrap or CompilationOptions.NeoCLR with a matching CLI declaration core.</param>
    /// <param name="output">Writable stream; validation failure leaves its bytes and position unchanged.</param>
    /// <param name="options">Explicit output/core identities and compiler-reference bindings.</param>
    /// <returns>Success and preserved compiler diagnostics, or a source/backend diagnostic without output.</returns>
    /// <remarks>Null/unwritable arguments throw. Stream I/O failures propagate and may leave partial output. No stream is closed.</remarks>
    public static NeoClrEmitResult Emit(Compilation compilation, Stream output, NeoClrEmitOptions options)
    {
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(options);
        ArgumentNullException.ThrowIfNull(output);
        if (!output.CanWrite) throw new ArgumentException("Output must be writable", nameof(output));
        var result = compilation.Emit(output, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options, emitMetadataAssembly: false)));
        return new(result.Success, result.Diagnostics);
    }

    internal static NeoClrEmitResult EmitPrepared(Compilation compilation, Stream output, NeoClrEmitOptions options)
    {
        ArgumentNullException.ThrowIfNull(output);
        if (!output.CanWrite) throw new ArgumentException("Output must be writable", nameof(output));
        var diagnostics = ImmutableArray<Diagnostic>.Empty;
        NeoClrEmitResult Fail(DiagnosticDescriptor descriptor, string detail, Location? location = null)
            => new(false, diagnostics.Add(Diagnostic.Create(descriptor, location ?? Location.None, detail)));
        if (compilation.Options.OutputKind is not (OutputKind.ConsoleApplication or OutputKind.DynamicallyLinkedLibrary))
            return Fail(Configuration, "requires console or library output");
        if (NeoClrBindingContract.GetError(compilation, options) is { } bindingError)
            return Fail(Configuration, bindingError);
        if (compilation.SyntaxTrees.Length == 0 || compilation.MacroSyntaxTrees.Length != 0)
            return Fail(Configuration, "requires source trees; macro trees are unsupported");
        if (options.Identity.Name != compilation.AssemblyName || options.Identity.PublicKeyToken.Length != 0 || options.Identity.Flags != 0)
            return Fail(Configuration, "requires matching unsigned output identity");
        if (options.BootstrapReference is { } bootstrap &&
            (!compilation.References.Any(r => ReferenceEquals(r, bootstrap)) ||
             compilation.GetAssemblyOrModuleSymbol(bootstrap) is not IAssemblySymbol bootstrapAssembly ||
             !SymbolEqualityComparer.Default.Equals(bootstrapAssembly, compilation.GetSpecialType(SpecialType.System_Int32).ContainingAssembly)))
            return Fail(Configuration, "bootstrap reference must be the registered primitive core reference");
        if (options.ConsoleReference is { } console &&
            (!compilation.References.Any(r => ReferenceEquals(r, console)) || compilation.GetAssemblyOrModuleSymbol(console) is not IAssemblySymbol))
            return Fail(Configuration, "console contract reference is not registered or has no assembly symbol");
        if (options.SystemSymbols is { } system &&
            (system.Library.ModuleName != "System" || system.Functions.IsDefaultOrEmpty || system.Functions.Length > 4096 ||
            system.Functions.Distinct().Count() != system.Functions.Length ||
            system.Functions.Any(f => f is null || !ReferenceEquals(f.Library, system.Library) || !f.TryGetStaticInt32Signature(out _)) ||
            !compilation.References.Any(r => ReferenceEquals(r, system.Reference)) ||
            compilation.GetAssemblyOrModuleSymbol(system.Reference) is not IAssemblySymbol systemAssembly || systemAssembly.Name != system.ProjectionAssemblyName))
            return Fail(Configuration, "invalid explicit System callable selection or projection reference");
        if (options.Dependencies.Length > 256) return Fail(Configuration, "too many dependencies");
        var bindings = new List<(IAssemblySymbol Symbol, NeoClrMetadataDependency Dependency)>();
        foreach (var dependency in options.Dependencies)
        {
            if (!compilation.References.Any(r => ReferenceEquals(r, dependency.Reference)))
                return Fail(Configuration, "dependency reference is not registered with this compilation");
            if (dependency.Reference is NeoClrMetadataReference native &&
                (!ReferenceEquals(native.Definition, dependency.Definition) || dependency.NativeImplementation is not null))
                return Fail(Configuration, "native dependency must use its semantic snapshot without a translated implementation");
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
    /// <summary>Emits a PE/#Neo assembly containing authoritative native metadata and a CLI reference projection.</summary>
    /// <param name="compilation">Compilation using the same bounded subset and validated binding contract as Emit.</param>
    /// <param name="output">Caller-owned writable stream; validation errors leave it unchanged.</param>
    /// <param name="options">Explicit output/core identities and dependency bindings.</param>
    /// <returns>Compiler diagnostics and emission success; container errors use NEOMETA003.</returns>
    /// <remarks>CLI bodies are reference-only. neoCLR loads the required native section. Stream I/O errors propagate.</remarks>
    public static NeoClrEmitResult EmitMetadataAssembly(Compilation compilation, Stream output, NeoClrEmitOptions options)
    {
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(options);
        ArgumentNullException.ThrowIfNull(output);
        if (!output.CanWrite) throw new ArgumentException("Output must be writable", nameof(output));
        var result = compilation.Emit(output, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options)));
        return new(result.Success, result.Diagnostics);
    }

    internal static NeoClrEmitResult EmitPreparedMetadataAssembly(Compilation compilation, Stream output, NeoClrEmitOptions options)
    {
        ArgumentNullException.ThrowIfNull(output);
        if (!output.CanWrite) throw new ArgumentException("Output must be writable", nameof(output));
        using var native = new MemoryStream();
        var result = EmitPrepared(compilation, native, options);
        if (!result.Success) return result;
        byte[] image;
        try { image = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.WriteBinary(native.ToArray(), options.CoreLibrary); }
        catch (Exception error) when (error is InvalidDataException or ArgumentException)
        {
            return new(false, result.Diagnostics.Add(Diagnostic.Create(Encoding, Location.None, error.Message)));
        }
        output.Write(image);
        return result;
    }

    private static DiagnosticDescriptor Descriptor(string id, string title, string message)
        => DiagnosticDescriptor.Create(id, title, "", "", message, "compiler", DiagnosticSeverity.Error, true);
}

internal sealed class UnsupportedInputException(string detail, Location location) : Exception(detail)
{
    internal Location Location { get; } = location;
}
