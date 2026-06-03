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
}
