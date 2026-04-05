using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

/// <summary>
/// Covers generic type inference through dictionary interface substitution when the callback is lambda-based.
/// </summary>
public sealed class ForEachDictionaryTypeInferenceTests : CompilationTestBase
{
    [Fact]
    public void ForEach_WithDictionaryArgument_InfersKeyValuePairTypeArgument()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

class Program {
    static func Run() -> () {
        val o = ["a", "bb"].ToDictionary(x => x, y => y.Length)
        ForEach(o, item => ())
    }

    static func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () {
        for item in source {
            callback(item)
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);

        var forEachInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(inv => inv.Expression is IdentifierNameSyntax id
                          && id.Identifier.Text == "ForEach");

        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(
            model.GetSymbolInfo(forEachInvocation).Symbol);

        Assert.Equal("ForEach", methodSymbol.Name);
        Assert.Single(methodSymbol.TypeArguments);

        var kvpDef = compilation.GetTypeByMetadataName("System.Collections.Generic.KeyValuePair`2")!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expectedKvp = (INamedTypeSymbol)kvpDef.Construct(stringType, intType);

        Assert.True(
            SymbolEqualityComparer.Default.Equals(methodSymbol.TypeArguments[0], expectedKvp),
            $"Expected T = KeyValuePair<string, int>, but got {methodSymbol.TypeArguments[0].ToDisplayString()}");
    }

    [Fact]
    public void ForEach_WithDictionaryAndTypedLambda_HasCorrectCallbackParameterType()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

class Program {
    static func Run() -> () {
        val o = ["a", "bb"].ToDictionary(x => x, y => y.Length)
        ForEach(o, item => ())
    }

    static func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () {
        for item in source {
            callback(item)
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);

        var lambda = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleFunctionExpressionSyntax>()
            .Last(); // last lambda; first is the y => y.Length inside ToDictionary

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var lambdaParam = Assert.Single(boundLambda.Parameters);

        var kvpDef = compilation.GetTypeByMetadataName("System.Collections.Generic.KeyValuePair`2")!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expectedKvp = (INamedTypeSymbol)kvpDef.Construct(stringType, intType);

        Assert.True(
            SymbolEqualityComparer.Default.Equals(lambdaParam.Type, expectedKvp),
            $"Expected lambda parameter type KeyValuePair<string, int>, but got {lambdaParam.Type.ToDisplayString()}");
    }

    [Fact]
    public void ForEach_WithDictionaryArgument_ProducesNoDiagnostics()
    {
        // Full compilation of the canonical ForEach(o, WriteLine) pattern must succeed
        // with no errors — specifically no BadImageFormatException from unresolved TKey/TValue
        // being emitted as class-level type parameters.
        const string source = """
import System.*
import System.Console.*
import System.Collections.Generic.*
import System.Linq.*

func Main() -> () {
    val o = ["1", "2", "3"].ToDictionary(x => x, y => int.Parse(y))
    ForEach(o, WriteLine)
}

func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () {
    for item in source {
        callback(item)
    }
}
""";
        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        var (compilation, _) = CreateCompilation(source, options: options);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            d => d.Severity == DiagnosticSeverity.Error);
    }
}
