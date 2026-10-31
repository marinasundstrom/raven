using System;
using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

// Per-compilation .NET metadata, reflection and host services. Platform policy and
// native semantic-loader selection belong to CompilationTarget, not this class.
internal sealed class DotNetCompilationServices
{
    private readonly Compilation _compilation;
    private readonly Lazy<ReflectionTypeLoader> _reflectionTypeLoader;
    private DotNetMetadataSession? _metadataSession;
    private DotNetMetadataSession? _previousMetadataSessionForReuse;

    internal DotNetCompilationServices(Compilation compilation)
    {
        _compilation = compilation;
        Emitter = new DotNetCompilationEmitter(compilation);
        // Allocation must not bind or load references: reflection queries may
        // request this projector before setup or during same-thread setup reentrancy.
        _reflectionTypeLoader = new(() => new ReflectionTypeLoader(compilation));
    }

    internal ICompilationEmitter Emitter { get; }
    internal ReflectionTypeLoader ReflectionTypeLoader => _reflectionTypeLoader.Value;
    internal DotNetHostRuntime HostRuntime { get; } = new();
    internal Assembly CoreAssembly { get; private set; } = null!;
    internal Assembly RuntimeCoreAssembly { get; private set; } = null!;
    internal Assembly EmitCoreAssembly { get; private set; } = null!;

    internal void AdoptMetadataReuseFrom(DotNetCompilationServices previous)
    {
        // Keep only compilation-independent state, never the earlier services,
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
}
