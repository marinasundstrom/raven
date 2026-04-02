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
        Assert.Equal(ExtensionMemberKind.Static, parseMethod!.ExtensionMemberKind);

        var receiverType = parseMethod.GetExtensionReceiverType();
        Assert.NotNull(receiverType);
        Assert.Equal("int", receiverType!.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void RavenInstanceExtensionMethod_MetadataSymbol_IsRecognizedAsExtensionMethod()
    {
        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var compilation = CreateCompilation(references: references);
        compilation.EnsureSetup();

        var systemNamespace = compilation.GetNamespaceSymbol("System");
        Assert.NotNull(systemNamespace);

        var extensionType = systemNamespace!.LookupType("OptionExtensions") as INamedTypeSymbol;
        Assert.NotNull(extensionType);

        var unwrapOrMethod = extensionType!.GetMembers("UnwrapOr").OfType<IMethodSymbol>().FirstOrDefault();
        Assert.NotNull(unwrapOrMethod);
        Assert.True(unwrapOrMethod!.IsExtensionMethod);
        Assert.Equal(ExtensionMemberKind.Instance, unwrapOrMethod.ExtensionMemberKind);

        var receiverType = unwrapOrMethod.GetExtensionReceiverType();
        Assert.NotNull(receiverType);
        Assert.Contains("Option", receiverType!.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat), StringComparison.Ordinal);
    }

    [Fact]
    public void RavenInstanceExtensionMethod_FromMetadata_BindsToResultReceiver()
    {
        const string source = """
import System.*

val wrapped = int.parse("42").WithMessage("wrapped")
""";

        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var (compilation, tree) = CreateCompilation(source, references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "WithMessage" });
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("ResultErrorContextExtensions", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal("union struct Result<int, ContextError<ParseIntError>>", boundInvocation.Type.ToDisplayString());
    }

    [Fact]
    public void MetadataType_AllInterfaces_IncludeTransitiveInterfaces()
    {
        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var compilation = CreateCompilation(references: references);
        compilation.EnsureSetup();

        var systemNamespace = compilation.GetNamespaceSymbol("System");
        Assert.NotNull(systemNamespace);

        var parseIntError = systemNamespace!.LookupType("ParseIntError") as INamedTypeSymbol;
        Assert.NotNull(parseIntError);

        var allInterfaces = parseIntError!.AllInterfaces
            .Select(i => i.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .ToArray();

        Assert.Contains("IParseError", allInterfaces);
        Assert.Contains("IError", allInterfaces);
    }

    [Fact]
    public void RavenStaticExtensionConversionOperator_FromMetadata_IsImported()
    {
        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var (compilation, tree) = CreateCompilation(
            """
import System.*

val value = Option<int>.Some(42)
val result: int? = value
""",
            references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var valueSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "value")));
        var resultSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "result")));
        var conversion = compilation.ClassifyConversion(valueSymbol.Type, resultSymbol.Type, includeUserDefined: true);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUserDefined);
        Assert.Equal("op_Implicit", conversion.MethodSymbol?.Name);
        Assert.Equal("OptionExtensions2", conversion.MethodSymbol?.ContainingType?.Name);
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
