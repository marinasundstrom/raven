using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NameOfExpressionSemanticTests : CompilationTestBase
{
    [Fact]
    public void NameOfExpression_WithValueTypeAndMemberOperands_BindsToStringNames()
    {
        const string source = """
import System.*
import System.Collections.Generic.*

val value = 2
val localName = nameof(value)
val genericTypeName = nameof(List<int>)
val importedTypeName = nameof(Console)
val importedMethodName = nameof(Console.WriteLine)
val qualifiedMethodName = nameof(System.Console.WriteLine)
val sourceFieldName = nameof(Test.Label)

class Test {
    static val Label = ""
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var names = tree.GetRoot()
            .DescendantNodes()
            .OfType<NameOfExpressionSyntax>()
            .Select(nameOf =>
            {
                var bound = Assert.IsType<BoundNameOfExpression>(model.GetBoundNode(nameOf));
                var type = model.GetTypeInfo(nameOf).Type;
                Assert.Equal(SpecialType.System_String, type?.SpecialType);
                return bound.Name;
            })
            .ToArray();

        Assert.Equal(
            ["value", "List", "Console", "WriteLine", "WriteLine", "Label"],
            names);
    }

    [Fact]
    public void NameOfExpression_WithMacroOperands_PreservesResolvedSpelling()
    {
        var (compilation, tree) = CreateCompilation("""
            import Raven.Macros.*

            val aliasName = nameof(quote)
            val canonicalName = nameof(Raven.Macros.Quote)
            """);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var nameOfExpressions = tree.GetRoot()
            .DescendantNodes()
            .OfType<NameOfExpressionSyntax>()
            .ToArray();

        Assert.Equal(
            ["quote", "Quote"],
            nameOfExpressions.Select(expression =>
                Assert.IsType<BoundNameOfExpression>(model.GetBoundNode(expression)).Name));

        var symbols = nameOfExpressions
            .Select(expression => Assert.IsAssignableFrom<IMacroSymbol>(
                model.GetSymbolInfo(expression.Operand).Symbol))
            .ToArray();
        Assert.All(symbols, static symbol => Assert.Equal("Raven.Macros.Quote", symbol.CanonicalName));
        Assert.Equal("quote", symbols[0].Name);
        Assert.Equal("Quote", symbols[1].Name);
    }

    [Fact]
    public void TypeOfExpression_WithMacroOperand_ReportsMacroIsNotType()
    {
        var (compilation, _) = CreateCompilation("""
            import Raven.Macros.*

            val macroType = typeof(quote)
            """);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM015"));
        Assert.Equal("'quote' is a macro, not a type.", diagnostic.GetMessage());
    }
}
