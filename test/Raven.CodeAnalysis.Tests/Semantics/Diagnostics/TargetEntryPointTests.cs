using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetEntryPointTests
{
    [Fact]
    public void HeapAsyncTargetSelectsSupportedMainWithoutHostBridge()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-entry-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "TargetTasks.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetTasks",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("public struct Result<T,E> {} namespace System.Tasks { public class Task<T> {} }")],
                paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
                Assert.True(declarations.Emit(stream).Success);

            foreach (var (returnType, valid) in new[] {
                ("int", true), ("Result<int, string>", true), ("Result<unit, string>", true),
                ("System.Tasks.Task<int>", true), ("System.Tasks.Task<unit>", true),
                ("System.Tasks.Task<Result<int, string>>", true),
                ("System.Tasks.Task<Result<unit, string>>", true),
                ("bool", false), ("System.Tasks.Task<string>", false),
                ("Result<string, string>", false), ("System.Tasks.Task<System.Tasks.Task<int>>", false)
            })
            {
                var value = returnType.StartsWith("System.Tasks.Task<") ? returnType + "()" : "default";
                var source = $"func Main() -> {returnType} {{ return {value} }}";
                var compilation = Compilation.Create("TargetEntry", [SyntaxTree.ParseText(source)],
                    paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                    new CompilationOptions(OutputKind.ConsoleApplication)
                        .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                        .WithTargetCoreAssemblyName("System.Runtime").WithHeapAsyncStateMachines(true));
                var entry = compilation.GetEntryPoint();
                if (valid)
                {
                    Assert.NotNull(entry);
                    Assert.Equal("Main", entry.Name);
                    Assert.False(entry.IsImplicitlyDeclared);
                    Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
                }
                else
                {
                    Assert.Null(entry);
                    Assert.Contains(compilation.GetDiagnostics(), d => d.Id is "RAV1014" or "RAV1022");
                }
            }
        }
        finally { Directory.Delete(directory, true); }
    }
}
