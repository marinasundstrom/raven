using System.IO;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

// Composition for the existing CLI implementation. Session/core reflection is
// still .NET-specific; this is not yet a replaceable target-provider interface.
internal sealed class DotNetCompilationTarget(CompilationOptions options)
{
    internal DotNetRuntimeContract RuntimeContract { get; } = new(options);

    internal DotNetMetadataSession OpenMetadataSession(
        Compilation compilation,
        DotNetMetadataSession? reusableSession)
        => DotNetSemanticDataLoader.OpenSession(compilation, reusableSession);

    internal ISemanticDataLoader CreateSemanticDataLoader(
        Compilation compilation,
        DotNetMetadataSession session)
        => new DotNetSemanticDataLoader(compilation, session);

    internal string? GetResolvedConfigurationError(Compilation compilation)
        => RuntimeContract.GetResolvedConfigurationError(compilation, compilation.CoreAssembly.GetName().Name);

    internal string? ResolveEmitOptions(Compilation compilation, EmitOptions? requested, out EmitOptions? effective)
    {
        effective = requested;
        if (GetResolvedConfigurationError(compilation) is { } error)
            return error;
        if (options.TargetCoreAssemblyName is null && !options.UsesDiscoveredTargetCore)
            return null;

        // The selected metadata core supplies the emitted identity; never infer it
        // from a runtime implementation loaded by the compiler host.
        var identity = compilation.CoreAssembly.GetName();
        if (requested?.TargetCoreLibraryIdentity is { } explicitIdentity && explicitIdentity.FullName != identity.FullName)
            return "explicit emission options conflict with the project's target core identity";

        effective = new EmitOptions(identity);
        return null;
    }

    internal void Emit(Compilation compilation, EmitOptions? options, Stream peStream, Stream? pdbStream)
        => new CodeGenerator(compilation, options).Emit(peStream, pdbStream);
}
