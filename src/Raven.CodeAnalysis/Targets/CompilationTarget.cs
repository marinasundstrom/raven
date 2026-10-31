using System;
using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

// Selects target policy and semantic inputs independently of compiler-host services.
// Existing reflection-facing APIs remain compatibility accessors, not native metadata.
internal sealed class CompilationTarget
{
    private readonly Compilation _compilation;
    private readonly DotNetCompilationServices _dotNet;
    private readonly bool _usesNativeMetadata;

    internal CompilationTarget(Compilation compilation)
    {
        _compilation = compilation;
        RuntimeContract = compilation.Options.TargetPlatform == TargetPlatform.NeoCLR
            ? new NeoClrCliRuntimeContract(compilation.Options)
            : new DotNetRuntimeContract(compilation.Options);
        _dotNet = new DotNetCompilationServices(compilation);
        _usesNativeMetadata = compilation.Options.MetadataImportOptions?.UseNativeMetadata == true;
    }

    internal ICompilationEmitter Emitter => _dotNet.Emitter;
    internal ReflectionTypeLoader ReflectionTypeLoader => _dotNet.ReflectionTypeLoader;
    internal CliRuntimeContract RuntimeContract { get; }
    internal DotNetHostRuntime HostRuntime => _dotNet.HostRuntime;
    internal Assembly CoreAssembly => _usesNativeMetadata
        ? throw new InvalidOperationException("Native metadata mode has no reflection core assembly; use semantic assembly symbols.")
        : _dotNet.CoreAssembly;
    internal Assembly RuntimeCoreAssembly => _dotNet.RuntimeCoreAssembly;
    internal Assembly EmitCoreAssembly => _dotNet.EmitCoreAssembly;

    internal void AdoptMetadataReuseFrom(CompilationTarget previous)
    {
        // Native semantic import has no .NET metadata session to reuse.
        if (!_usesNativeMetadata)
            _dotNet.AdoptMetadataReuseFrom(previous._dotNet);
    }

    internal void BeginSetup() => _dotNet.BeginSetup();

    internal ISemanticDataLoader InitializeSemanticData()
    {
        if (_usesNativeMetadata)
        {
            if (_compilation.Options.TargetPlatform != TargetPlatform.NeoCLR ||
                _compilation.References.Any(reference => reference is not ISemanticMetadataReference))
                throw new TargetInitializationException("native metadata mode requires the NeoCLR target and semantic references exclusively");
            return new NativeSemanticDataLoader(_compilation);
        }
        return new CompositeSemanticDataLoader(_compilation, _dotNet.InitializeSemanticData());
    }

    internal Diagnostic? GetConfigurationDiagnostic()
        => _compilation.Options.TargetPlatform is not (TargetPlatform.DotNet or TargetPlatform.NeoCLR)
            ? TargetDiagnostics.UnsupportedPlatform(_compilation.Options.TargetPlatform)
            : TargetDiagnostics.InvalidConfiguration(RuntimeContract.GetConfigurationError() ??
                _compilation.References.OfType<ISemanticMetadataReference>().Select(r => r.Validate(_compilation)).FirstOrDefault(error => error is not null));

    internal Diagnostic? GetResolvedConfigurationDiagnostic()
        => TargetDiagnostics.InvalidConfiguration(RuntimeContract.GetResolvedConfigurationError(_compilation, _usesNativeMetadata
            ? _compilation.Options.MetadataImportOptions.CoreAssemblyName : CoreAssembly.GetName().Name));
}
