using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MetadataStaticExtensionMemberSemanticTests : CompilationTestBase
{
    [Fact]
    public void StaticExtensionMethod_FromMetadata_BindsToExtensionContainer()
    {
        const string source = """
import Raven.MetadataFixtures.StaticExtensions.*

val created = WidgetExtensions.Create(42)
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithExtensionMethods);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("WidgetExtensions", boundInvocation.Method.ContainingType?.Name);
        Assert.True(boundInvocation.Method.IsExtensionMethod);
        Assert.Null(boundInvocation.ExtensionReceiver);
    }

    [Fact]
    public void RavenStaticExtensionMethod_FromMetadata_BindsToReceiverType()
    {
        const string source = """
import System.*

val parsed = int.parse("42")
""";

        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var (compilation, tree) = CreateCompilation(source, references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Int32Extensions", boundInvocation.Method.ContainingType?.Name);
        Assert.Null(boundInvocation.ExtensionReceiver);
        Assert.Equal("union struct Result<int, ParseIntError>", boundInvocation.Type.ToDisplayString());
    }

    [Fact]
    public void RavenStaticExtensionMethod_MetadataSymbol_RecoversReceiverType()
    {
        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var compilation = CreateCompilation(references: references);
        compilation.EnsureSetup();

        var systemNamespace = compilation.GetNamespaceSymbol("System");
        Assert.NotNull(systemNamespace);

        var extensionType = systemNamespace!.LookupType("Int32Extensions") as INamedTypeSymbol;
        Assert.NotNull(extensionType);

        var parseMethod = extensionType!.GetMembers("parse").OfType<IMethodSymbol>().FirstOrDefault(method => method.Parameters.Length == 1);
        Assert.NotNull(parseMethod);

        var receiverType = parseMethod!.GetExtensionReceiverType();
        Assert.NotNull(receiverType);
        Assert.Equal("int", receiverType!.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    private static string GetRavenCorePath()
    {
        var outputPath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (File.Exists(outputPath))
            return outputPath;

        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        return Path.Combine(repoRoot, "src", "Raven.Core", "bin", "Debug", "net10.0", "Raven.Core.dll");
    }
}
