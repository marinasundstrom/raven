using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeRuntimeProjectChecks
{
    internal static void Run(string projectPath)
    {
        var provider = new NeoClrProjectMetadataProvider();
        var workspace = RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
            RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: provider));
        var id = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(id)!;
        if (!project.MetadataReferences.Any() || project.MetadataReferences.Any(r => r is not NeoClrMetadataReference))
            throw new Exception("Native runtime project loaded a non-native reference.");
        var compilation = workspace.GetCompilation(id);
        provider.GetConfiguration(projectPath).Validate(compilation);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        foreach (var (prefix, expected) in new[] { ("System.", "Int32"), ("System.Int32.", "Parse"), ("System.Introspection.", "TypeInfo") })
        {
            var source = "func Main() -> int { " + prefix + " }";
            var tree = SyntaxTree.ParseText(source);
            var completion = Compilation.Create("Completion", [tree], compilation.References.ToArray(), compilation.Options);
            var names = new CompletionService().GetCompletions(completion, tree, source.IndexOf(prefix, StringComparison.Ordinal) + prefix.Length)
                .Select(item => item.DisplayText).ToArray();
            if (!names.Any(n => n == expected || n.StartsWith(expected + "(", StringComparison.Ordinal)) ||
                prefix == "System." && names.Any(n => n.StartsWith("Action", StringComparison.Ordinal) || n.StartsWith("Func<", StringComparison.Ordinal)))
                throw new Exception("Native completion contract failed: " + prefix + expected);
        }
        Console.WriteLine("PASS native-only project references, compilation, runtime contracts and editor completion");
    }
}
