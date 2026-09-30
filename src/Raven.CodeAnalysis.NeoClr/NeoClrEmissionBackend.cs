namespace Raven.CodeAnalysis.NeoClr;

/// <summary>Explicit native backend for Compilation.Emit, using the separate neoCLR metadata library.</summary>
/// <remarks>
/// Binding still uses the .NET primitive bootstrap and explicit native reference projections.
/// This selects artifact production only. The supported source subset is the same as
/// NeoClrCompilationEmitter. Debug output and CLI core-reference rewriting are unsupported.
/// </remarks>
public sealed class NeoClrEmissionBackend : ICompilationEmissionBackend
{
    private static readonly DiagnosticDescriptor Configuration = DiagnosticDescriptor.Create(
        "NEOMETA002", "Invalid native configuration", "", "", "Native emission configuration: {0}.",
        "compiler", DiagnosticSeverity.Error, true);
    private readonly NeoClrEmitOptions _options;
    private readonly bool _emitMetadataAssembly;

    /// <summary>Creates reusable native artifact configuration; mutable builders are allocated per emission.</summary>
    /// <param name="options">Explicit native identities and compiler-reference bindings.</param>
    /// <param name="emitMetadataAssembly">True for binary PE/#Neo; false for native JSON interchange.</param>
    public NeoClrEmissionBackend(NeoClrEmitOptions options, bool emitMetadataAssembly = true)
    {
        ArgumentNullException.ThrowIfNull(options);
        _options = options;
        _emitMetadataAssembly = emitMetadataAssembly;
    }

    /// <inheritdoc />
    public EmitResult Emit(Compilation compilation, Stream output, Stream? debugOutput, EmitOptions options)
    {
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(output);
        ArgumentNullException.ThrowIfNull(options);
        if (!output.CanWrite) throw new ArgumentException("Output must be writable", nameof(output));
        if (debugOutput is not null || options.TargetCoreLibraryIdentity is not null)
            return new(false, [Diagnostic.Create(Configuration, Location.None,
                "debug output and CLI core-reference rewriting are unsupported; configure the native core through NeoClrEmitOptions")]);
        var result = _emitMetadataAssembly
            ? NeoClrCompilationEmitter.EmitPreparedMetadataAssembly(compilation, output, _options)
            : NeoClrCompilationEmitter.EmitPrepared(compilation, output, _options);
        return new(result.Success, result.Diagnostics);
    }
}
