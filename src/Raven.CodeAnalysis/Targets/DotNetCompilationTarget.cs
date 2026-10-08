using System;
using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

// Target composition retains the .NET/CLI path and an explicit native-only semantic path.
// Reflection handles belong only to the former; host execution remains separate.
internal sealed class DotNetCompilationTarget
{
    private readonly Compilation _compilation;
    private readonly Lazy<ReflectionTypeLoader> _reflectionTypeLoader;
    private DotNetMetadataSession? _metadataSession;
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
    private Assembly? _coreAssembly;
    internal Assembly CoreAssembly => _compilation.Options.MetadataImportOptions?.UseNativeMetadata == true
        ? throw new InvalidOperationException("Native metadata mode has no reflection core assembly; use semantic assembly symbols.")
        : _coreAssembly!;
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
        if (_compilation.Options.MetadataImportOptions?.UseNativeMetadata == true)
        {
            if (_compilation.Options.TargetPlatform != TargetPlatform.NeoCLR ||
                _compilation.References.Any(reference => reference is not ISemanticMetadataReference))
                throw new TargetInitializationException("native metadata mode requires the NeoCLR target and semantic references exclusively");
            _previousMetadataSessionForReuse = null;
            return new NativeSemanticDataLoader(_compilation);
        }
        _metadataSession = DotNetSemanticDataLoader.OpenSession(
            _compilation.References, _compilation.Options.MetadataImportOptions, HostRuntime,
            _previousMetadataSessionForReuse);
        _previousMetadataSessionForReuse = null;
        _coreAssembly = _metadataSession.CoreAssembly;
        EmitCoreAssembly = HostRuntime.ResolveEmitCoreAssembly() ?? RuntimeCoreAssembly;
        HostRuntime.RegisterRuntimeAssembly(CoreAssembly, RuntimeCoreAssembly.Location);
        return new CompositeSemanticDataLoader(_compilation, new DotNetSemanticDataLoader(_metadataSession, ReflectionTypeLoader, HostRuntime));
    }

    internal Diagnostic? GetConfigurationDiagnostic()
        => _compilation.Options.TargetPlatform is not (TargetPlatform.DotNet or TargetPlatform.NeoCLR)
            ? TargetDiagnostics.UnsupportedPlatform(_compilation.Options.TargetPlatform)
            : TargetDiagnostics.InvalidConfiguration(RuntimeContract.GetConfigurationError() ??
                _compilation.References.OfType<ISemanticMetadataReference>().Select(r => r.Validate(_compilation)).FirstOrDefault(error => error is not null));

    internal Diagnostic? GetResolvedConfigurationDiagnostic()
        => TargetDiagnostics.InvalidConfiguration(RuntimeContract.GetResolvedConfigurationError(_compilation, _compilation.Options.MetadataImportOptions?.UseNativeMetadata == true
            ? _compilation.Options.MetadataImportOptions.CoreAssemblyName : CoreAssembly.GetName().Name));
}
