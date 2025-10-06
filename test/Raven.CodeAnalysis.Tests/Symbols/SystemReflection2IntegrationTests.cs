using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class SystemReflection2IntegrationTests
{
    [Fact]
    public void MetadataHostProducesSameFrameworkSymbols()
    {
        var references = TestMetadataReferences.Default;

        var runtimeCompilation = Compilation.Create(
            "RuntimeHost",
            Array.Empty<SyntaxTree>(),
            references,
            new CompilationOptions(OutputKind.ConsoleApplication, metadataReferenceHostKind: MetadataReferenceHostKind.Runtime));

        var metadataCompilation = Compilation.Create(
            "MetadataHost",
            Array.Empty<SyntaxTree>(),
            references,
            new CompilationOptions(OutputKind.ConsoleApplication, metadataReferenceHostKind: MetadataReferenceHostKind.SystemReflection2));

        var runtimeString = runtimeCompilation.GetTypeByMetadataName("System.String");
        var metadataString = metadataCompilation.GetTypeByMetadataName("System.String");

        Assert.NotNull(runtimeString);
        Assert.NotNull(metadataString);

        Assert.Equal(runtimeString!.ToDisplayString(), metadataString!.ToDisplayString());

        var runtimeSubstringCount = runtimeString!.GetMembers().OfType<IMethodSymbol>().Count(m => m.Name == "Substring");
        var metadataSubstringCount = metadataString!.GetMembers().OfType<IMethodSymbol>().Count(m => m.Name == "Substring");

        Assert.Equal(runtimeSubstringCount, metadataSubstringCount);
    }
}
