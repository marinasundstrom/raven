using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MetadataStaticExtensionMemberSemanticTests : CompilationTestBase
{
    [Theory]
    [InlineData("int", "Option<int>", "Int32", "system.int32.tryparse.string.option.v1")]
    [InlineData("long", "Option<long>", "Int64", "system.int64.tryparse.string.option.v1")]
    [InlineData("double", "Option<double>", "Double", "system.double.tryparse.string.option.v1")]
    [InlineData("decimal", "Option<decimal>", "Decimal", "system.decimal.tryparse.string.option.v1")]
    [InlineData("Guid", "Option<Guid>", "Guid", "system.guid.tryparse.string.option.v1")]
    [InlineData("DateTime", "Option<DateTime>", "DateTime", "system.datetime.tryparse.string.option.v1")]
    [InlineData("DateTimeOffset", "Option<DateTimeOffset>", "DateTimeOffset", "system.datetimeoffset.tryparse.string.option.v1")]
    [InlineData("DateOnly", "Option<DateOnly>", "DateOnly", "system.dateonly.tryparse.string.option.v1")]
    [InlineData("TimeOnly", "Option<TimeOnly>", "TimeOnly", "system.timeonly.tryparse.string.option.v1")]
    [InlineData("TimeSpan", "Option<TimeSpan>", "TimeSpan", "system.timespan.tryparse.string.option.v1")]
    public void FrameworkTryParseProjection_IsEnabledByDefault(
        string typeName,
        string expectedReturnType,
        string expectedContainer,
        string expectedProjectionId)
    {
        var source = $$"""
import System.*

val parsed = {{typeName}}.TryParse("42")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal(expectedContainer, boundInvocation.Method.ContainingType?.Name);
        Assert.Equal(expectedReturnType, boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.False(boundInvocation.Method.IsExtensionMethod);
        Assert.Null(boundInvocation.Method.GetExtensionReceiverType());
        var projected = Assert.IsType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
        Assert.EndsWith("Extensions", projected.AdapterMethod.ContainingType?.Name, StringComparison.Ordinal);
        Assert.Equal(expectedProjectionId, GetFrameworkProjectionId(projected.AdapterMethod));
    }

    [Fact]
    public void FrameworkTryParseProjection_CanBeDisabled()
    {
        const string source = """
val parsed = int.TryParse("42", out var value)
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithFrameworkProjectionMode(FrameworkProjectionMode.None);
        var (compilation, tree) = CreateCompilation(source, options, TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));

        Assert.Equal("Int32", boundInvocation.Method.ContainingType?.Name);
        Assert.Equal("bool", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void FrameworkProjectionAdapter_DoesNotLeakThroughExtensionLookupWhenDisabled()
    {
        const string source = """
import System.*

val parsed = int.TryParse("42")
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithFrameworkProjectionMode(FrameworkProjectionMode.None);
        var (compilation, tree) = CreateCompilation(source, options, TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();
        _ = compilation.GetSemanticModel(tree).GetBoundNode(
            tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single());

        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }

    [Fact]
    public void FrameworkProjection_ReportsMissingBridgeWithProjectionId()
    {
        const string source = """
val parsed = int.TryParse("42")
""";
        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.Default);
        compilation.EnsureSetup();
        _ = compilation.GetSemanticModel(tree).GetBoundNode(
            tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single());

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(diagnostic =>
                diagnostic.Id == "RAV2803" &&
                diagnostic.GetMessage().Contains("system.int32.tryparse.string.option.v1", StringComparison.Ordinal)));
        Assert.Contains("system.int32.tryparse.string.option.v1", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("System.Int32Extensions", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void FrameworkTryParseProjection_PreservesTypeSpecificOptions()
    {
        const string source = """
import System.*
import System.Globalization.*

val parsed = int.TryParse("42", NumberStyles.Integer, CultureInfo.InvariantCulture)
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal(3, boundInvocation.Method.Parameters.Length);
        Assert.Equal("Option<int>", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.IsType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
    }

    [Fact]
    public void FrameworkDateTimeTryParseProjection_PreservesProviderAndStyles()
    {
        const string source = """
import System.*
import System.Globalization.*

val parsed = DateTime.TryParse("2026-07-22", CultureInfo.InvariantCulture, DateTimeStyles.None)
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Collection(
            boundInvocation.Method.Parameters,
            parameter => Assert.Equal("string?", parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)),
            parameter => Assert.Equal("IFormatProvider?", parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)),
            parameter => Assert.Equal("DateTimeStyles", parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)));
        Assert.Equal("Option<DateTime>", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.IsType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
    }

    [Fact]
    public void FrameworkDictionaryTryGetValueProjection_SubstitutesConstructedTypes()
    {
        const string source = """
import System.*
import System.Collections.Generic.*

val values = Dictionary<string, int>()
val found = values.TryGetValue("answer")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal("Dictionary", boundInvocation.Method.ContainingType?.Name);
        Assert.Equal("Option<int>", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Collection(
            boundInvocation.Method.Parameters,
            parameter => Assert.Equal("string", parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)));
        var projected = Assert.IsType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
        Assert.Equal(
            "system.collections.generic.dictionary.trygetvalue.option.v1",
            GetFrameworkProjectionId(projected.AdapterMethod));
    }

    [Fact]
    public void FrameworkDictionaryTryGetValueProjection_CanBeDisabled()
    {
        const string source = """
import System.Collections.Generic.*

val values = Dictionary<string, int>()
val found = values.TryGetValue("answer", out var value)
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithFrameworkProjectionMode(FrameworkProjectionMode.None);
        var (compilation, tree) = CreateCompilation(source, options, TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal("bool", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.IsNotType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
    }

    [Fact]
    public void FrameworkDictionaryTryGetValueProjection_PreservesNullableValueType()
    {
        const string source = """
import System.*
import System.Collections.Generic.*

val values = Dictionary<string, string?>()
val found = values.TryGetValue("answer")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal("Option<string?>", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void FrameworkParseProjection_ReplacesSameSignatureClrMethod()
    {
        const string source = """
import System.*

val parsed = int.Parse("42")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal("Int32", boundInvocation.Method.ContainingType?.Name);
        Assert.Equal("Result<int, FormatException | OverflowException>", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.False(boundInvocation.Method.IsExtensionMethod);
        var projected = Assert.IsType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
        Assert.Equal("system.int32.parse.string.result.v2", GetFrameworkProjectionId(projected.AdapterMethod));
    }

    [Fact]
    public void FrameworkGuidParseProjection_ReplacesSameSignatureClrMethod()
    {
        const string source = """
import System.*

val parsed = Guid.Parse("d2719b1e-88c5-4a06-aeba-69d19e70b9f7")
""";

        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal("Guid", boundInvocation.Method.ContainingType?.Name);
        Assert.Equal("Result<Guid, FormatException>", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        var projected = Assert.IsType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
        Assert.Equal("system.guid.parse.string.result.v2", GetFrameworkProjectionId(projected.AdapterMethod));
    }

    [Fact]
    public void FrameworkParseProjection_CanBeDisabled()
    {
        const string source = """
val parsed = int.Parse("42")
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithFrameworkProjectionMode(FrameworkProjectionMode.None);
        var (compilation, tree) = CreateCompilation(source, options, TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var boundInvocation = Assert.IsType<BoundInvocationExpression>(compilation.GetSemanticModel(tree).GetBoundNode(invocation));
        Assert.Equal("Int32", boundInvocation.Method.ContainingType?.Name);
        Assert.Equal("int", boundInvocation.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.IsNotType<Raven.CodeAnalysis.Symbols.ProjectedMethodSymbol>(boundInvocation.Method);
    }

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

    private static string? GetFrameworkProjectionId(IMethodSymbol method) =>
        method.GetAttributes()
            .Single(attribute => attribute.AttributeClass.Name == "FrameworkProjectionAttribute")
            .ConstructorArguments.Single().Value as string;

    [Fact]
    public void RavenCore_DoesNotExposeLowercaseParseHelper()
    {
        var compilation = CreateCompilation(references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var systemNamespace = compilation.GetNamespaceSymbol("System");
        Assert.NotNull(systemNamespace);

        var extensionType = systemNamespace!.LookupType("Int32Extensions") as INamedTypeSymbol;
        Assert.NotNull(extensionType);
        Assert.Empty(extensionType!.GetMembers("parse"));
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
    public void RavenStaticExtensionConversionOperator_FromMetadata_IsImported()
    {
        var (compilation, tree) = CreateCompilation(
            """
import System.*

val value = Option<int>.Some(42)
val result: int? = value
AcceptNumber(value)

val textValue = Option<string>.Some("OK")
val textResult: string? = textValue
AcceptText(textValue)

func AcceptNumber(value: int?) -> unit {
}

func AcceptText(value: string?) -> unit {
}
""",
            references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var valueSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "value")));
        var resultSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "result")));
        var textValueSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "textValue")));
        var textResultSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "textResult")));
        var valueConversion = compilation.ClassifyConversion(valueSymbol.Type, resultSymbol.Type, includeUserDefined: true);
        var textConversion = compilation.ClassifyConversion(textValueSymbol.Type, textResultSymbol.Type, includeUserDefined: true);

        AssertOptionConversion(valueConversion, "OptionExtensions2");
        AssertOptionConversion(textConversion, "OptionExtensions1");

        var invocations = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Where(static invocation => invocation.Expression is IdentifierNameSyntax)
            .ToArray();
        Assert.All(invocations, invocation =>
        {
            var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
            Assert.StartsWith("Accept", boundInvocation.Method.Name, StringComparison.Ordinal);
        });
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
        Assert.Equal("object?", locals["optionCarrierValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Result<int, CustomError>", locals["converted"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        Assert.Equal("bool", locals["resultHasOk"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("bool", locals["resultHasError"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("bool", locals["resultCarrierHasValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("object?", locals["resultCarrierValue"].Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
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

    return match maybeItem {
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

    private static void AssertOptionConversion(Conversion conversion, string expectedContainerName)
    {
        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUserDefined);
        Assert.Equal("op_Implicit", conversion.MethodSymbol?.Name);
        Assert.Equal(expectedContainerName, conversion.MethodSymbol?.ContainingType?.Name);
    }

}
