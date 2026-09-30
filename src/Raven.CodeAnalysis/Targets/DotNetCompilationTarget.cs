using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

// Composition for one compilation's existing CLI implementation. Reflection
// handles stay here; shared semantic services still have .NET-facing adapters.
internal sealed class DotNetCompilationTarget
{
    private readonly Compilation _compilation;
    private readonly Lazy<ReflectionTypeLoader> _reflectionTypeLoader;
    private DotNetMetadataSession _metadataSession = null!;
    private DotNetMetadataSession? _previousMetadataSessionForReuse;

    internal DotNetCompilationTarget(Compilation compilation)
    {
        _compilation = compilation;
        RuntimeContract = new DotNetRuntimeContract(compilation.Options);
        // Allocation must not bind or load references: reflection queries may
        // request this projector before setup or during same-thread setup reentrancy.
        _reflectionTypeLoader = new(() => new ReflectionTypeLoader(compilation));
    }

    internal ReflectionTypeLoader ReflectionTypeLoader => _reflectionTypeLoader.Value;
    internal DotNetRuntimeContract RuntimeContract { get; }
    internal DotNetHostRuntime HostRuntime { get; } = new();
    internal Assembly CoreAssembly { get; private set; } = null!;
    internal Assembly RuntimeCoreAssembly { get; private set; } = null!;
    internal Assembly EmitCoreAssembly { get; private set; } = null!;

    internal void AdoptMetadataReuseFrom(DotNetCompilationTarget previous)
    {
        // Keep only compilation-independent state, never the earlier target,
        // projector, host service or compilation.
        _previousMetadataSessionForReuse = previous._metadataSession;
    }

    internal void BeginSetup()
    {
        // Seed host handles before setup can reenter type/emission services.
        RuntimeCoreAssembly = HostRuntime.RuntimeCoreAssembly;
        EmitCoreAssembly = RuntimeCoreAssembly;
    }

    internal ISemanticDataLoader InitializeSemanticData(bool reuseMetadataSession)
    {
        _metadataSession = DotNetSemanticDataLoader.OpenSession(_compilation,
            reuseMetadataSession ? _previousMetadataSessionForReuse : null);
        _previousMetadataSessionForReuse = null;
        CoreAssembly = _metadataSession.CoreAssembly;
        EmitCoreAssembly = HostRuntime.ResolveEmitCoreAssembly() ?? RuntimeCoreAssembly;
        HostRuntime.RegisterRuntimeAssembly(CoreAssembly, RuntimeCoreAssembly.Location);
        return new DotNetSemanticDataLoader(_compilation, _metadataSession, ReflectionTypeLoader);
    }

    internal string? GetResolvedConfigurationError()
        => RuntimeContract.GetResolvedConfigurationError(_compilation, CoreAssembly.GetName().Name);

    internal string? ResolveEmitOptions(EmitOptions? requested, out EmitOptions? effective)
    {
        effective = requested;
        if (GetResolvedConfigurationError() is { } error)
            return error;
        if (_compilation.Options.TargetCoreAssemblyName is null && !_compilation.Options.UsesDiscoveredTargetCore)
            return null;

        // The selected metadata core supplies the emitted identity; never infer it
        // from a runtime implementation loaded by the compiler host.
        var identity = CoreAssembly.GetName();
        if (requested?.TargetCoreLibraryIdentity is { } explicitIdentity && explicitIdentity.FullName != identity.FullName)
            return "explicit emission options conflict with the project's target core identity";

        effective = new EmitOptions(identity);
        return null;
    }

    internal void Emit(EmitOptions? options, Stream peStream, Stream? pdbStream)
        => new CodeGenerator(_compilation, options).Emit(peStream, pdbStream);
}
