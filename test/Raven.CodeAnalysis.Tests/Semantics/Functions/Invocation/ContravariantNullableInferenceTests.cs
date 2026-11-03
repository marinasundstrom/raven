using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ContravariantNullableInferenceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void HashCodeAdd_NullableInferenceMatchesCSharp(bool nullableValue, bool explicitType)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.*
            func Add(value: string{{(nullableValue ? "?" : "")}}) -> unit {
                var hash = HashCode()
                hash.Add{{(explicitType ? "<string?>" : "")}}(value, StringComparer.Ordinal)
            }
            """);
        var compilation = Compilation.Create("NullableComparer", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        var csharpTree = Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
            #nullable enable
            using System;
            class Probe {
                void Add(string{{(nullableValue ? "?" : "")}} value) {
                    var hash = new HashCode();
                    hash.Add{{(explicitType ? "<string?>" : "")}}(value, StringComparer.Ordinal);
                }
            }
            """);
        var oracle = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("ComparerOracle", [csharpTree],
            TestMetadataReferences.Default.OfType<PortableExecutableReference>()
                .Select(reference => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(reference.FilePath)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(oracle.GetDiagnostics(), d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error);
        var csharpInvocation = csharpTree.GetRoot().DescendantNodes()
            .OfType<Microsoft.CodeAnalysis.CSharp.Syntax.InvocationExpressionSyntax>().Single();
        var csharpMethod = Assert.IsAssignableFrom<Microsoft.CodeAnalysis.IMethodSymbol>(
            Microsoft.CodeAnalysis.CSharp.CSharpExtensions.GetSymbolInfo(oracle.GetSemanticModel(csharpTree), csharpInvocation).Symbol);
        Assert.Equal(csharpMethod.TypeArguments.Single().NullableAnnotation == Microsoft.CodeAnalysis.NullableAnnotation.Annotated,
            method.TypeArguments.Single().IsNullable);
    }

    [Theory]
    [InlineData("IComparer", "string", "string?", false)]
    [InlineData("IComparer", "string", "string?", true)]
    [InlineData("IComparer", "string?", "string?", false)]
    [InlineData("IComparer", "string?", "string?", true)]
    [InlineData("IComparer", "string", "object", false)]
    [InlineData("IComparer", "string", "object", true)]
    [InlineData("IComparer", "string", "object?", false)]
    [InlineData("IComparer", "string", "object?", true)]
    [InlineData("IEnumerable", "string", "string?", false)]
    [InlineData("IEnumerable", "string", "string?", true)]
    [InlineData("DoubleComparer", "string", "string?", false)]
    [InlineData("DoubleComparer", "string", "string?", true)]
    [InlineData("IList", "string", "string?", false)]
    [InlineData("IList", "string", "string?", true)]
    public void GenericBounds_MatchCSharpInEitherParameterOrder(string contract, string lower, string other, bool reverse)
    {
        string Container(string type) => contract == "DoubleComparer" ? $"IComparer<IComparer<{type}>>" : $"{contract}<{type}>";
        var ravenParameters = reverse ? $"other: {Container("T")}, value: T" : $"value: T, other: {Container("T")}";
        var csharpParameters = reverse ? $"{Container("T")} other, T value" : $"T value, {Container("T")} other";
        var arguments = reverse ? "other, value" : "value, other";
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            func Pick<T>({{ravenParameters}}) -> T => value
            func Test(value: {{lower}}, other: {{Container(other)}}) -> unit {
                let result = Pick({{arguments}})
            }
            """);
        var compilation = Compilation.Create("VarianceBounds", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        var csharpTree = Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
            #nullable enable
            using System.Collections.Generic;
            class Probe {
                static T Pick<T>({{csharpParameters}}) => value;
                void Test({{lower}} value, {{Container(other)}} other) {
                    var result = Pick({{arguments}});
                }
            }
            """);
        var oracle = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("VarianceOracle", [csharpTree],
            TestMetadataReferences.Default.OfType<PortableExecutableReference>()
                .Select(reference => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(reference.FilePath)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(oracle.GetDiagnostics(), d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error);
        var csharpInvocation = csharpTree.GetRoot().DescendantNodes()
            .OfType<Microsoft.CodeAnalysis.CSharp.Syntax.InvocationExpressionSyntax>().Single();
        var csharpMethod = Assert.IsAssignableFrom<Microsoft.CodeAnalysis.IMethodSymbol>(
            Microsoft.CodeAnalysis.CSharp.CSharpExtensions.GetSymbolInfo(oracle.GetSemanticModel(csharpTree), csharpInvocation).Symbol);
        var expected = csharpMethod.TypeArguments.Single();
        var actual = method.TypeArguments.Single();
        Assert.True((expected.NullableAnnotation == Microsoft.CodeAnalysis.NullableAnnotation.Annotated) == actual.IsNullable,
            $"C# inferred {expected.ToDisplayString()}, Raven inferred {actual.ToDisplayString()}");
        Assert.Equal(expected.Name, actual.GetNonNullableType().Name);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrContravariantNullableInferenceTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClrWithHostReferences_NonNullableLowerBoundWins(bool reverse)
    {
        var parameters = reverse ? "comparer: IComparer<T>, value: T" : "value: T, comparer: IComparer<T>";
        var arguments = reverse ? "comparer, value" : "value, comparer";
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            func Pick<T>({{parameters}}) -> T => value
            func Test(value: string, comparer: IComparer<string?>) -> unit {
                let result = Pick({{arguments}})
            }
            """);
        var compilation = Compilation.Create("NeoClrVariance", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.False(Assert.Single(method.TypeArguments).IsNullable);
    }
}
