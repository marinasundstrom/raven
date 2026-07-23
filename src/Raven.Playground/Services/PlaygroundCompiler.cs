using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.Playground.Services;

public sealed class PlaygroundCompiler(PlaygroundFrameworkReferences frameworkReferences)
{
    public PlaygroundCompilationResult Compile(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source, path: "playground.rav");
        var compilation = Compilation.Create(
            $"RavenPlaygroundProgram_{Guid.NewGuid():N}",
            [syntaxTree],
            frameworkReferences.GetReferences(),
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var assemblyStream = new MemoryStream();
        var emitResult = compilation.Emit(assemblyStream);
        var diagnostics = emitResult.Diagnostics
            .Select(static diagnostic => diagnostic.ToString())
            .ToArray();

        return emitResult.Success
            ? new PlaygroundCompilationResult(true, assemblyStream.ToArray(), diagnostics)
            : new PlaygroundCompilationResult(false, null, diagnostics);
    }
}

public sealed record PlaygroundCompilationResult(
    bool Success,
    byte[]? AssemblyImage,
    IReadOnlyList<string> Diagnostics);
