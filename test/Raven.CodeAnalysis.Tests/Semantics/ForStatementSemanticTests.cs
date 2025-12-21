using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ForStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void ForEach_WithDiscardIdentifier_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

let numbers = List<System.Int32>()

for each _ in numbers {
    let value = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Local.ShouldBeNull();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void ForEach_WithoutIdentifier_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

let numbers = List<System.Int32>()

for each in numbers {
    let value = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Local.ShouldBeNull();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void ForRange_UsesRangeIteration()
    {
        const string source = """
import System.Console.*

for i in 2..5 {
    WriteLine(i * 2)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Iteration.Kind.ShouldBe(ForIterationKind.Range);
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);

        boundFor.Iteration.Range.ShouldNotBeNull();

        var range = boundFor.Iteration.Range!;
        range.Left.ShouldNotBeNull();
        range.Right.ShouldNotBeNull();

        range.Left!.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(2);
        range.Right!.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(5);
    }

    [Fact]
    public void ForRange_FromEndBoundaryNotSupported()
    {
        const string source = """
for i in ^1..^0 {
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldBe(new[] { "RAV2603" });
    }

    [Fact]
    public void ForRange_MissingEndReportsDiagnostic()
    {
        const string source = """
for i in 1.. {
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldBe(new[] { "RAV2602" });
    }
}
