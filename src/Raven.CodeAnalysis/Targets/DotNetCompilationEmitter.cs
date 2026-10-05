using System.IO;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Targets;

internal sealed class DotNetCompilationEmitter(Compilation compilation) : ICompilationEmitter
{
    public EmitResult Emit(Stream output, Stream? debugOutput, EmitOptions? options)
    {
        if (compilation.UsesSourceObjectRoot)
            return new EmitResult(false, [TargetDiagnostics.InvalidConfiguration("source Object root emission requires native root authoring support")!]);
        if (compilation.References.Any(reference => reference is ISemanticMetadataReference))
            return new EmitResult(false, [TargetDiagnostics.InvalidConfiguration("non-CLI semantic references require a compatible target emission backend")!]);

        if (ResolveEmitOptions(options, out var effectiveOptions) is { } diagnostic)
            return new EmitResult(false, [diagnostic]);

        new CodeGenerator(compilation, effectiveOptions).Emit(output, debugOutput);
        return new EmitResult(true, []);
    }

    private Diagnostic? ResolveEmitOptions(EmitOptions? requested, out EmitOptions? effective)
    {
        effective = requested;
        if (compilation.Options.TargetCoreAssemblyName is null && !compilation.Options.UsesDiscoveredTargetCore)
            return null;

        // The selected metadata core supplies the emitted identity; never infer it
        // from a runtime implementation loaded by the compiler host.
        var identity = compilation.CoreAssembly.GetName();
        if (requested?.TargetCoreLibraryIdentity is { } explicitIdentity && explicitIdentity.FullName != identity.FullName)
            return TargetDiagnostics.InvalidConfiguration("explicit emission options conflict with the project's target core identity");

        effective = new EmitOptions(identity);
        return null;
    }
}
