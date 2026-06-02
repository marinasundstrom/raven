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

    [Fact]
    public void RavenStaticExtensionMethod_FromMetadata_BindsToReceiverType()
    {
        const string source = """
import System.*

val parsed = int.parse("42")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Int32Extensions", boundInvocation.Method.ContainingType?.Name);
        Assert.Null(boundInvocation.ExtensionReceiver);
        Assert.Equal("union class Result<int, ParseIntError>", boundInvocation.Type.ToDisplayString());
    }

    [Fact]
    public void RavenStaticExtensionMethod_MetadataSymbol_RecoversReceiverType()
    {
        var compilation = CreateCompilation(references: TestMetadataReferences.DefaultWithRavenCore);
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
    public void RavenOptionMember_MetadataSymbol_IsDeclaredOnOption()
    {
        var compilation = CreateCompilation(references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var systemNamespace = compilation.GetNamespaceSymbol("System");
        Assert.NotNull(systemNamespace);

        var optionType = systemNamespace!.LookupType("Option") as INamedTypeSymbol;
        Assert.NotNull(optionType);

        var unwrapOrMethod = optionType!.GetMembers("UnwrapOr").OfType<IMethodSymbol>().FirstOrDefault();
        Assert.NotNull(unwrapOrMethod);
        Assert.False(unwrapOrMethod!.IsExtensionMethod);
        Assert.Equal(ExtensionMemberKind.None, unwrapOrMethod.ExtensionMemberKind);
        Assert.Equal("Option", unwrapOrMethod.ContainingType?.Name);
    }

    [Fact]
    public void RavenInstanceExtensionMethod_FromMetadata_BindsToResultReceiver()
    {
        const string source = """
import System.*

val wrapped = int.parse("42").WithContext("wrapped")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "WithContext" });
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("ResultErrorContextExtensions", boundInvocation.Method.ContainingType?.Name);
        Assert.NotNull(boundInvocation.ExtensionReceiver);
        Assert.Equal("union class Result<int, ContextError<ParseIntError>>", boundInvocation.Type.ToDisplayString());
    }

    [Fact]
    public void MetadataType_AllInterfaces_IncludeTransitiveInterfaces()
    {
        var compilation = CreateCompilation(references: TestMetadataReferences.DefaultWithRavenCore);
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
        var (compilation, tree) = CreateCompilation(
            """
import System.*

val value = Option<int>.Some(42)
val result: int? = value
""",
            references: TestMetadataReferences.DefaultWithRavenCore);
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

    [Fact]
    public void RavenCoreMetadata_OptionAndResultCarrierHelperSurface_BindsTypes()
    {
        const string source = """
import System.*

val option: Option<int> = .Some(2)
val optionHasSome = option.HasSome
val optionHasNone = option.HasNone
val optionCarrierHasValue = option.HasValue
val optionCarrierValue = option.Value
val converted = option.IsOkOr(CustomError("Bang!"))

val result: Result<int, CustomError> = .Ok(2)
val resultHasOk = result.HasOk
val resultHasError = result.HasError
val resultCarrierHasValue = result.HasValue
val resultCarrierValue = result.Value
val resultIsOk = result.IsOk

record class CustomError(message: string)
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>()
            .ToDictionary(
                static declarator => declarator.Identifier.ValueText,
                declarator => Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator)));

        Assert.Equal("bool", locals["optionHasSome"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("bool", locals["optionHasNone"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("bool", locals["optionCarrierHasValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("object", locals["optionCarrierValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Result<int, CustomError>", locals["converted"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        Assert.Equal("bool", locals["resultHasOk"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("bool", locals["resultHasError"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("bool", locals["resultCarrierHasValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("object", locals["resultCarrierValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Option<int>", locals["resultIsOk"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void RavenCoreMetadata_ResultMemberAccessPropagationAndUnwrapOrDefault_BindsTypes()
    {
        const string source = """
import System.*

func GetUser() -> Result<User, Err> {
    return .Ok(User("Marina"))
}

func Test() -> Result<int, Err> {
    val wrapped = GetUser()?.Name
    val name = wrapped.UnwrapOrDefault()

    return .Ok(name.Length + 1)
}

union Err {
    case MissingUser
    case MissingName
}

record class User(Name: string)
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>()
            .ToDictionary(
                static declarator => declarator.Identifier.ValueText,
                declarator => Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator)));

        Assert.Equal("Result<string, Err>", locals["wrapped"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("string", locals["name"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var unwrapInvocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression.ToString() == "wrapped.UnwrapOrDefault");
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(unwrapInvocation));

        Assert.Equal("Result", boundInvocation.Method.ContainingType?.Name);
        Assert.Equal("string", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void RavenCoreMetadata_ResultCarrierConditionalAccess_ResolvesCarrierSymbols()
    {
        const string source = """
import System.*

func GetUser() -> Result<User, Err> {
    return .Ok(User("Marina", .Some(Item("Candy"))))
}

func GetItem() -> Result<string, Err> {
    val maybeItem = GetUser()?.Item?

    return maybeItem match {
        .Some(val item) => .Ok(item.Name)
        .None => .Error(Err.MissingName)
    }
}

union Err {
    case MissingUser
    case MissingName
}

record class User(Name: string, Item: Option<Item>)
record class Item(Name: string)
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var conditionalAccesses = tree.GetRoot().DescendantNodes().OfType<ConditionalAccessExpressionSyntax>().ToArray();
        var outerConditional = conditionalAccesses.Last();
        var bound = Assert.IsType<BoundCarrierConditionalAccessExpression>(model.GetBoundNode(outerConditional));

        Assert.NotNull(bound.ReceiverResultOkCaseType);
        Assert.NotNull(bound.ReceiverResultErrorCaseType);
        Assert.NotNull(bound.ResultTryGetValueForOkCaseMethod);
        Assert.NotNull(bound.ResultTryGetValueForErrorCaseMethod);
        Assert.NotNull(bound.ResultOkCaseType);
        Assert.NotNull(bound.ResultErrorCaseType);
        Assert.NotNull(bound.ResultOkCtor);
        Assert.NotNull(bound.ResultErrorCtor);
        Assert.NotNull(bound.ReceiverResultOkValueGetter);
        Assert.NotNull(bound.ReceiverResultErrorDataGetter);
    }

}
