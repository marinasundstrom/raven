using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class GenericSelfConstructionTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ExplicitOwnTypeParametersAreValidConstructorArguments(bool targetMetadata, bool qualified)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        if (targetMetadata)
            options = options.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                .WithTargetCoreAssemblyName("System.Runtime");
        var source = """
            namespace Examples
            public class Box<T> {
                private var stored: T
                public init(value: T) { stored = value }
                public func Read() -> T { return stored }
                public func Copy() -> Box<T> { return OWNER<T>(stored) }
            }
            public class Entry {
                public static func Run() -> int { return Box<int>(42).Copy().Read() }
            }
            """.Replace("OWNER", qualified ? "Examples.Box" : "Box");
        var compilation = Compilation.Create("GenericSelfConstruction", [SyntaxTree.ParseText(source)],
            references.Select(MetadataReference.CreateFromFile).ToArray(), options);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Examples.Entry")!.GetMethod("Run")!.Invoke(null, null));
    }
}
