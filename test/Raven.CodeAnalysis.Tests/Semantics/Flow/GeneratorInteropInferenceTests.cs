using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class GeneratorInteropInferenceTests
{
    [Theory]
    [InlineData("let numbers = System.Collections.Generic.List<int>(); let words = System.Collections.Generic.List<string>(); return numbers.Zip(words, (number, word) => word.Length + number)")]
    [InlineData("let value = (ExpressionSyntax)(if flag { IdentifierName(\"x\") } else { BinaryExpression(SyntaxKind.AddExpression, IdentifierName(\"x\"), IdentifierName(\"y\")) }); return value")]
    [InlineData("let values = System.Collections.Generic.List<ExpressionSyntax>(); let value = if flag { LiteralExpression(SyntaxKind.FalseLiteralExpression) } else { values.Aggregate((a, b) => BinaryExpression(SyntaxKind.LogicalOrExpression, a, b)) }; return value")]
    [InlineData("return AccessorList([AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)])")]
    [InlineData("let arguments = System.Collections.Generic.List<ArgumentSyntax>(); let properties = System.Collections.Generic.List<string>(); return arguments.Zip(properties, (arg, prop) => BinaryExpression(SyntaxKind.NotEqualsExpression, IdentifierName(prop), arg.Expression))")]
    public void GeneratorExpressionsBindAgainstRoslynMetadata(string body)
    {
        var source = $$"""
            import System.*
            import System.Linq.*
            import Microsoft.CodeAnalysis.CSharp.*
            import Microsoft.CodeAnalysis.CSharp.Syntax.*
            import Microsoft.CodeAnalysis.CSharp.SyntaxFactory.*
            class Probe {
                static func Run(flag: bool) -> object {
                    {{body}}
                }
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default.Concat([
            MetadataReference.CreateFromFile(typeof(Microsoft.CodeAnalysis.SyntaxNode).Assembly.Location),
            MetadataReference.CreateFromFile(typeof(Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree).Assembly.Location)]);
        var compilation = Compilation.Create("GeneratorInterop", [tree], references.ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.True(errors.Length == 0, string.Join(Environment.NewLine, errors.Select(d => d.ToString())));
    }
}
