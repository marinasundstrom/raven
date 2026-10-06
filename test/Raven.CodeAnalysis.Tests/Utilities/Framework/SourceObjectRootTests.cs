using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class SourceObjectRootTests
{
    private const string Root = """
        namespace System
        public abstract class Object {
            protected init() { }
            virtual func ToString() -> string => "root"
            virtual func Equals(other: Object?) -> bool => false
            virtual func GetHashCode() -> int => 0
        }
        """;
    private const string Consumer = """
        public class Item {
            override func Equals(other: object?) -> bool => false
            static func Echo(value: object) -> System.Object => value
            static func EchoArray(value: object[]) -> System.Object[] => value
            static func Box<T>(value: T) -> object => value
        }
        """;

    private static Compilation Create(IEnumerable<string> sources, bool selected = true, bool dotnet = false)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net10.0"));
        using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"));
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        using var image = new MemoryStream();
        core.Write(image);
        var references = paths.Select(MetadataReference.CreateFromFile).Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray();
        var options = dotnet ? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            : CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null);
        return Compilation.Create("RootLibrary", sources.Select(source => SyntaxTree.ParseText(source)).ToArray(), references,
            options.WithMetadataImportOptions(new MetadataImportOptions(dotnet ? "System.Runtime" : "NeoCLR.CoreProbe", null, null, selected)));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    public void ReflectionMetadataResolutionSupportsEitherRootOwner(bool sourceRoot, bool dotnet)
    {
        var compilation = Create([Root], selected: sourceRoot, dotnet: dotnet);
        var root = compilation.GetSpecialType(SpecialType.System_Object);
        Assert.Equal(sourceRoot, root.ContainingAssembly!.Name == "RootLibrary");
        var metadataType = compilation.CoreAssembly.GetType("System.Attribute", throwOnError: true)!;
        var resolved = new ReflectionTypeLoader(compilation).ResolveType(metadataType);
        var attribute = Assert.IsAssignableFrom<INamedTypeSymbol>(resolved);
        Assert.Equal("Attribute", attribute.Name);
        Assert.NotEqual(TypeKind.Error, attribute.TypeKind);
        Assert.Same(root, attribute.BaseType);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void UnionToStringUsesSourceRootRegardlessOfDeclarationOrder(bool reverse, bool sameFile)
    {
        const string union = """
            public union Choice {
                case Some(value: int)
                case None
            }
            """;
        var sources = sameFile
            ? new[] { reverse ? "namespace System\n" + union + "\n" + Root.Replace("namespace System", "") : Root + "\n" + union }
            : reverse ? new[] { union, Root } : new[] { Root, union };
        var compilation = Create(sources);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var root = compilation.GetSpecialType(SpecialType.System_Object);
        var rootToString = Assert.Single(root.GetMembers("ToString").OfType<IMethodSymbol>());
        var choice = compilation.Assembly.GetTypeByMetadataName(sameFile ? "System.Choice" : "Choice")!;
        var toStrings = choice.GetMembers("ToString").OfType<SourceMethodSymbol>().ToArray();
        Assert.NotEmpty(toStrings);
        Assert.All(toStrings, method => Assert.Same(rootToString, method.OverriddenMethod));
        Assert.Equal("RootLibrary", rootToString.ContainingAssembly!.Name);
    }

    [Fact]
    public void SourceRootAndSlotsRequireExplicitEmissionCapabilities()
    {
        var compilation = Create([Root]);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var root = (INamedTypeSymbol)compilation.GetSpecialType(SpecialType.System_Object);
        var allowed = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), [],
            [EmissionDeclarationKind.ObjectRoot, EmissionDeclarationKind.ObjectRootSlot],
            [Accessibility.Public], [Accessibility.Public], allowsRootClassSignatures: true);
        Assert.True(SourceTypePlan.TryCreate(root, out var type, allowed));
        Assert.Equal(EmissionDeclarationKind.ObjectRoot, type!.DeclarationKind);
        Assert.False(SourceTypePlan.TryCreate(root, out _, ReflectionEmitCapabilities.Shared));
        foreach (var name in new[] { "ToString", "Equals", "GetHashCode" })
        {
            var method = Assert.Single(root.GetMembers(name).OfType<IMethodSymbol>());
            Assert.True(SourceCallablePlan.TryCreate(method, out var callable, allowed));
            Assert.Equal(EmissionDeclarationKind.ObjectRootSlot, callable!.DeclarationKind);
            Assert.False(SourceCallablePlan.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void SelectedRootUnifiesSignaturesBasesAndOverrides(bool reverse, bool queryFirst)
    {
        var compilation = Create(reverse ? [Consumer, Root] : [Root, Consumer]);
        if (queryFirst)
            Assert.Equal("RootLibrary", compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly!.Name);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var root = compilation.Assembly.GetTypeByMetadataName("System.Object")!;
        Assert.Same(root, compilation.GetSpecialType(SpecialType.System_Object));
        Assert.Same(root, compilation.GetTypeByMetadataName("System.Object"));
        Assert.Equal(SpecialType.System_Object, root.SpecialType);
        Assert.Null(root.BaseType);
        var item = compilation.Assembly.GetTypeByMetadataName("Item")!;
        Assert.Same(root, item.BaseType);
        var echo = Assert.Single(item.GetMembers("Echo").OfType<IMethodSymbol>());
        Assert.Same(root, echo.ReturnType);
        Assert.Same(root, echo.Parameters[0].Type);
        var array = Assert.Single(item.GetMembers("EchoArray").OfType<IMethodSymbol>());
        Assert.Same(root, Assert.IsAssignableFrom<IArrayTypeSymbol>(array.ReturnType).ElementType);
        var equality = Assert.Single(item.GetMembers("Equals").OfType<IMethodSymbol>());
        Assert.Same(root, Assert.IsType<SourceMethodSymbol>(equality).OverriddenMethod!.ContainingType);
        using var output = new MemoryStream();
        output.WriteByte(42);
        var emitted = compilation.Emit(output);
        Assert.False(emitted.Success);
        Assert.Contains(emitted.Diagnostics, d => d.GetMessage().Contains("native root authoring"));
        Assert.Equal(new byte[] { 42 }, output.ToArray());
        Assert.Equal(1, output.Position);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void UnselectedDeclarationsDoNotReplaceEitherTargetsCore(bool dotnet)
    {
        var compilation = Create([Root], selected: false, dotnet: dotnet);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var root = compilation.Assembly.GetTypeByMetadataName("System.Object")!;
        Assert.Equal(SpecialType.None, root.SpecialType);
        Assert.NotSame(root, compilation.GetSpecialType(SpecialType.System_Object));
        Assert.NotNull(root.BaseType);
    }

    [Theory]
    [InlineData("namespace Other\npublic abstract class Object { }")]
    [InlineData("namespace System\npublic struct Object { }")]
    [InlineData("namespace System\npublic abstract class Object<T> { }")]
    [InlineData("namespace System\npublic class Object { }")]
    [InlineData("namespace System\npublic abstract class Object : Exception { }")]
    [InlineData("namespace System\npublic abstract class Object { private var value: int = 0 }")]
    public void MissingOrInvalidSourceRootProducesConfigurationDiagnostic(string source)
    {
        var compilation = Create([source]);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003" && d.GetMessage().Contains("source Object ownership"));
        using var output = new MemoryStream();
        Assert.False(compilation.Emit(output).Success);
        Assert.Empty(output.ToArray());
    }

    [Fact]
    public async Task ConcurrentColdQueriesShareTheSelectedIdentity()
    {
        var compilation = Create([Consumer, Root]);
        var results = await Task.WhenAll(Enumerable.Range(0, 8).Select(_ => Task.Run(() => compilation.GetSpecialType(SpecialType.System_Object))));
        Assert.All(results, root => Assert.Same(results[0], root));
        Assert.Equal("RootLibrary", results[0].ContainingAssembly!.Name);
        Assert.Null(results[0].BaseType);
    }

    [Fact]
    public void ChangingSelectionOrRemovingRootDoesNotReuseAnotherSnapshotsIdentity()
    {
        var compilation = Create([Root, Consumer]);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var root = compilation.GetSpecialType(SpecialType.System_Object);
        var unselected = Compilation.Create(compilation.AssemblyName, compilation.SyntaxTrees, compilation.References.ToArray(),
            compilation.Options.WithMetadataImportOptions(new MetadataImportOptions("NeoCLR.CoreProbe")));
        unselected.AdoptIncrementalReuseFrom(compilation);
        Assert.NotEqual("RootLibrary", unselected.GetSpecialType(SpecialType.System_Object).ContainingAssembly!.Name);
        var removed = Compilation.Create(compilation.AssemblyName, [SyntaxTree.ParseText("public class Other { }")], compilation.References.ToArray(), compilation.Options);
        removed.AdoptIncrementalReuseFrom(compilation);
        Assert.Contains(removed.GetDiagnostics(), d => d.Id == "RAVT003");
        Assert.Equal(TypeKind.Error, removed.GetSpecialType(SpecialType.System_Object).TypeKind);
        Assert.Same(root, compilation.GetSpecialType(SpecialType.System_Object));
        Assert.Null(root.BaseType);
    }

    [Fact]
    public void DotNetRejectsSourceRootConfigurationWithoutChangingItsSpecialType()
    {
        var compilation = Create([Root], dotnet: true);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003");
        Assert.NotEqual("RootLibrary", compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly!.Name);
    }
}
