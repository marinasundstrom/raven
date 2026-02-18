using System;
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
}
