using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class FrameworkFacadeScopeTests
{
    [Theory]
    [InlineData("System.MemoryExtensions", "System.Memory", "return \"hello\".AsSpan().Length")]
    [InlineData("System.Threading.LazyInitializer", "System.Threading", "var value: string? = null\nreturn LazyInitializer.EnsureInitialized(ref value, () => \"ready\").Length")]
    public void DotNet_EmittedReferencesUseSelectedFrameworkDeclaration(string typeName, string assemblyName, string body)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var compilation = Compilation.Create("FacadeScopes", [SyntaxTree.ParseText($$"""
            import System.*
            import System.Threading.*
            public class Cases {
                static func Run() -> int {
                    {{body}}
                }
            }
            """)], paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        image.Position = 0;
        using (var metadata = AssemblyDefinition.ReadAssembly(image))
        {
            var reference = Assert.Single(metadata.MainModule.GetTypeReferences(), type => type.FullName == typeName);
            Assert.Equal(assemblyName, Assert.IsType<AssemblyNameReference>(reference.Scope).Name);
        }
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(5, loaded.Assembly.GetType("Cases")!.GetMethod("Run")!.Invoke(null, null));
    }
}
