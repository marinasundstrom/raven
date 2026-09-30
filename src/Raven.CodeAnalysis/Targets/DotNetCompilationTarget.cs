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

    internal void Emit(Compilation compilation, EmitOptions? options, Stream peStream, Stream? pdbStream)
        => new CodeGenerator(compilation, options).Emit(peStream, pdbStream);
}
