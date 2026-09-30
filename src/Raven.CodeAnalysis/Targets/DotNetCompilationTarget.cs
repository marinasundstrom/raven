using System;
using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

// Composition for the .NET pipeline and the experimental neoCLR CLI bridge. Reflection
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
        RuntimeContract = compilation.Options.TargetPlatform == TargetPlatform.NeoCLR
            ? new NeoClrCliRuntimeContract(compilation.Options)
            : new DotNetRuntimeContract(compilation.Options);
        Emitter = new DotNetCompilationEmitter(compilation);
        // Allocation must not bind or load references: reflection queries may
        // request this projector before setup or during same-thread setup reentrancy.
        _reflectionTypeLoader = new(() => new ReflectionTypeLoader(compilation));
    }

    internal ICompilationEmitter Emitter { get; }
    internal ReflectionTypeLoader ReflectionTypeLoader => _reflectionTypeLoader.Value;
    internal CliRuntimeContract RuntimeContract { get; }
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

    internal ISemanticDataLoader InitializeSemanticData()
    {
        _metadataSession = DotNetSemanticDataLoader.OpenSession(
            _compilation.References, _compilation.Options.MetadataImportOptions, HostRuntime,
            _previousMetadataSessionForReuse);
        _previousMetadataSessionForReuse = null;
        CoreAssembly = _metadataSession.CoreAssembly;
        EmitCoreAssembly = HostRuntime.ResolveEmitCoreAssembly() ?? RuntimeCoreAssembly;
        HostRuntime.RegisterRuntimeAssembly(CoreAssembly, RuntimeCoreAssembly.Location);
        return new DotNetSemanticDataLoader(_metadataSession, ReflectionTypeLoader, HostRuntime);
    }

    internal Diagnostic? GetConfigurationDiagnostic()
        => _compilation.Options.TargetPlatform is not (TargetPlatform.DotNet or TargetPlatform.NeoCLR)
            ? TargetDiagnostics.UnsupportedPlatform(_compilation.Options.TargetPlatform)
            : TargetDiagnostics.InvalidConfiguration(RuntimeContract.GetConfigurationError());

    internal Diagnostic? GetResolvedConfigurationDiagnostic()
        => TargetDiagnostics.InvalidConfiguration(RuntimeContract.GetResolvedConfigurationError(_compilation, CoreAssembly.GetName().Name));
}
