using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeFunctionInferenceChecks
{
    internal static void Run(string projectPath)
    {
        var provider = new NeoClrProjectMetadataProvider();
        var workspace = RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
            RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: provider));
        var id = workspace.OpenProject(projectPath);
        var original = workspace.GetCompilation(id);
        const string source = """
            import System.*
            import System.Concurrency.Tasks.*
            async func Main() -> Task<unit> {
                let operation = Task.Run(() => 41)
                let value = await operation
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("NativeFunctionInference", [tree], original.References.ToArray(), original.Options);
        var model = compilation.GetSemanticModel(tree);
        foreach (var declaration in tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>())
        {
            var local = (ILocalSymbol)model.GetDeclaredSymbol(declaration)!;
            Console.WriteLine(local.Name + ": " + local.Type.ToDisplayString());
            var type = local.Name == "operation" ? ((INamedTypeSymbol)local.Type).TypeArguments.Single() : local.Type;
            if (type.SpecialType != SpecialType.System_Int32)
                throw new Exception("Callback result was lost: " + local.Type.ToDisplayString());
        }
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        Console.WriteLine("PASS native function inference");
    }
}
