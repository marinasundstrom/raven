using Mono.Cecil;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SourcePrimitiveMemberTests
{
    private const string Provider = """
        namespace System
        public class String {
            public func SourceLength() -> int => 42
            public val Custom: int => 7
            public static func CreateCount() -> int => 4
        }
        """;
    private const string Consumer = """
        public class Consumer {
            public static func Read(value: string) -> int => value.SourceLength() + value.Custom + string.CreateCount()
            public static func Literal() -> int => "hello".SourceLength()
        }
        """;

    private static Compilation Create(string consumer, bool selected, bool reversed = false)
    {
        var corePath = TestMetadataReferences.Default.OfType<PortableExecutableReference>()
            .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
        using var core = AssemblyDefinition.ReadAssembly(corePath);
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        using var image = new MemoryStream();
        core.Write(image);
        var trees = new[] { SyntaxTree.ParseText(Provider), SyntaxTree.ParseText(consumer) };
        if (reversed) Array.Reverse(trees);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithRuntimeIterationContract(null).WithRuntimePropagationContract(null)
            .WithMetadataImportOptions(new MetadataImportOptions("NeoCLR.CoreProbe", null,
                selected ? [SpecialType.System_String] : []));
        return Compilation.Create("SourcePrimitives", trees,
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(image.ToArray())], options);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitSourceProviderBindsMethodsPropertiesAndLiteralsWithoutReplacingScalarIdentity(bool reversed)
    {
        var compilation = Create(Consumer, true, reversed);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var primitive = compilation.GetSpecialType(SpecialType.System_String);
        Assert.Equal("NeoCLR.CoreProbe", primitive.ContainingAssembly.Name);
        Assert.Equal(SpecialType.System_String, primitive.SpecialType);
        var consumer = compilation.Assembly.GetTypeByMetadataName("Consumer")!;
        var read = Assert.Single(consumer.GetMembers("Read").OfType<IMethodSymbol>());
        Assert.Same(primitive, Assert.Single(read.Parameters).Type);
    }

    [Fact]
    public void SourceNameAloneDoesNotRedirectBootstrapMembers()
    {
        var compilation = Create(Consumer, false);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0117");
    }

    [Fact]
    public void SelectedSourceDoesNotFallBackToBootstrapOnlyMembers()
    {
        var compilation = Create("public class Consumer { public static func Read(value: string) -> string => value.Substring(0) }", true);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0117");
    }

    [Fact]
    public void SourceProvidersAreCopiedAndRejectUnsupportedOrConflictingOwners()
    {
        var source = new List<SpecialType> { SpecialType.System_String };
        var options = new MetadataImportOptions("Core", null, source);
        source.Clear();
        Assert.Contains(SpecialType.System_String, options.SourcePrimitiveTypes);
        Assert.Empty(new MetadataImportOptions("Core").SourcePrimitiveTypes);
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core", null, [SpecialType.System_Object]));
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_String] = "External" }, [SpecialType.System_String]));
    }

    [Fact]
    public void SourceProviderRequiresNeoClrTarget()
    {
        var compilation = Compilation.Create("Invalid", [], TestMetadataReferences.Default,
            new CompilationOptions().WithMetadataImportOptions(new MetadataImportOptions("System.Runtime", null, [SpecialType.System_String])));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003");
    }
}
