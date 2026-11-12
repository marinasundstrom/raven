using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Targets;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "NeoCLR")]
public sealed class NeoClrLegacyFaultContainerTests
{
    [Theory]
    [InlineData("System.Runtime.CompilerServices", "TopLevelAttribute", true)]
    [InlineData("Other", "TopLevelAttribute", false)]
    [InlineData("System.Runtime.CompilerServices", "TopLevel", false)]
    public void ImportedFaultRequiresExactRuntimeMarker(string attributeNamespace, string attributeName, bool expected)
    {
        var source = $$"""
            namespace {{attributeNamespace}} {
                public sealed class {{attributeName}} : System.Attribute { }
            }
            namespace System {
                [{{attributeNamespace}}.{{attributeName}}]
                public static class ArbitraryContainer {
                    public static void Panic(string message) { }
                }
            }
            """;
        var references = TestMetadataReferences.Default.OfType<PortableExecutableReference>()
            .Select(reference => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(reference.FilePath!));
        var fixture = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("NeoCLR.CoreProbe",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)], references,
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = fixture.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var compilation = Compilation.Create("Consumer", [],
            TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var container = compilation.GetTypeByMetadataName("System.ArbitraryContainer")!;
        // All three markers qualify for ordinary import; runtime fault ownership is stricter.
        Assert.True(Assert.IsAssignableFrom<INamespaceMemberContainer>(container).IsNamespaceMemberContainer);
        Assert.Equal(expected, Assert.IsAssignableFrom<ICliRuntimeNamespaceContainer>(container).IsRuntimeNamespaceContainer);
        var method = container.GetMembers("Panic").OfType<MethodSymbol>().Single();
        Assert.Equal(expected, NeoClrCliCompatibility.IsTerminalRuntimeFault(method));
        Assert.Equal(expected, BoundNodeFacts.IsTerminalRuntimeFault(method));
    }

    [Theory]
    [InlineData("NeoCLR.CoreProbe", true)]
    [InlineData("OrdinaryLibrary", false)]
    public void SourceNamespaceCarrierPreservesLegacyAssemblyRule(string assemblyName, bool expected)
    {
        var tree = SyntaxTree.ParseText("namespace System\npublic func Panic(message: string) { }");
        var compilation = Compilation.Create(assemblyName, [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var method = (MethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.True(Assert.IsAssignableFrom<ICliRuntimeNamespaceContainer>(method.ContainingType).IsRuntimeNamespaceContainer);
        Assert.Equal(expected, NeoClrCliCompatibility.IsTerminalRuntimeFault(method));
    }
}
