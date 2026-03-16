using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ForStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void For_WithDiscardTarget_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

val numbers = List<System.Int32>()

for _ in numbers {
    val value = 0
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
    public void For_WithExplicitValDiscardTarget_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

val numbers = List<System.Int32>()

for val _ in numbers {
    val value = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        forStatement.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Local.ShouldBeNull();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void For_WithoutTarget_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

val numbers = List<System.Int32>()

for in numbers {
    val value = 0
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
    public void For_WithExplicitValIdentifierTarget_CreatesLocal()
    {
        const string source = """
import System.Collections.Generic.*

val numbers = List<System.Int32>()

for val number in numbers {
    val copy = number
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        forStatement.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Local.ShouldNotBeNull();
        boundFor.Local!.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void For_WithOuterValPatternTarget_BindsImplicitPatternCaptures()
    {
        const string source = """
val persons = [(1, "Ada", 20)]

for val (1, name, _) in persons {
    name.Length
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var designation = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(d => d.Identifier.ValueText == "name");

        var symbol = model.GetDeclaredSymbol(designation).ShouldBeOfType<SourceLocalSymbol>();
        symbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
    }

    [Fact]
    public void For_WithExplicitVarIdentifierTarget_ReportsDiagnostic()
    {
        const string source = """
import System.Collections.Generic.*

val numbers = List<System.Int32>()

for var number in numbers {
    number
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldContain("RAV0343");
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
    public void ForRange_WithExclusiveUpperBound_UsesExclusiveIteration()
    {
        const string source = """
for i in 2..<5 {
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
        boundFor.Iteration.RangeUpperExclusive.ShouldBeTrue();
        boundFor.Iteration.Range.ShouldNotBeNull();
        boundFor.Iteration.Range!.IsUpperExclusive.ShouldBeTrue();
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
        diagnostics.Select(d => d.Id).ShouldContain("RAV1503");
    }

    [Fact]
    public void ForRange_WithCharBounds_UsesCharIterationType()
    {
        const string source = """
for c in 'a'..'z' {
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
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Char);
    }

    [Fact]
    public void ForRange_WithDecimalBounds_UsesDecimalIterationType()
    {
        const string source = """
val start: decimal = 1
val end: decimal = 3

for value in start..end {
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
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Decimal);
    }

    [Fact]
    public void ForRange_WithUnsupportedBoundaryType_ReportsDiagnostic()
    {
        const string source = """
for value in true..false {
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldContain("RAV1503");
    }

    [Fact]
    public void ForRange_WithByClause_BindsStepExpression()
    {
        const string source = """
for x in 0..10 by 2 {
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();

        boundFor.Iteration.RangeStep.ShouldNotBeNull();
        boundFor.Iteration.RangeStep.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(2);
    }

    [Fact]
    public void ForRange_ByClauseZeroStep_ReportsDiagnostic()
    {
        const string source = """
for x in 0..10 by 0 {
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldContain("RAV2604");
    }

    [Fact]
    public void For_WithByClause_ReportsDiagnostic()
    {
        const string source = """
import System.Collections.Generic.*

val items = List<int>()
for x in items by 2 {
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldContain("RAV2605");
    }

    [Fact]
    public void AwaitFor_BindsAsyncIteration()
    {
        const string source = """
import System.Collections.Generic.*
import System.Threading.Tasks.*

class C {
    async func Run(values: IAsyncEnumerable<int>) -> Task {
        await for item in values {
            _ = item
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Iteration.Kind.ShouldBe(ForIterationKind.Async);
        boundFor.Iteration.MoveNextAsyncMethod.ShouldNotBeNull();
        boundFor.Iteration.GetEnumeratorMethod.ShouldNotBeNull();
        boundFor.Iteration.CurrentGetter.ShouldNotBeNull();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void For_PrefersInstanceGetEnumeratorPattern()
    {
        const string source = """
import System.Collections.Generic.*

class Counter {
    private var _values = List<int>()

    func Add(value: int) {
        _values.Add(value)
    }

    func GetEnumerator() -> List<int>.Enumerator {
        return _values.GetEnumerator()
    }
}

class Runner {
    func Run() {
        val counter = Counter()
        counter.Add(1)
        counter.Add(2)

        for item in counter {
            val value = item
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();

        boundFor.Iteration.GetEnumeratorMethod.ShouldNotBeNull();
        boundFor.Iteration.GetEnumeratorMethod!.ContainingType.Name.ShouldBe("Counter");
        boundFor.Iteration.GetEnumeratorMethod.IsExtensionMethod.ShouldBeFalse();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void For_UsesExtensionGetEnumeratorPattern_WhenInstanceMissing()
    {
        const string source = """
import System.Collections.Generic.*

class Counter {
    private var _values = List<int>()

    func Add(value: int) {
        _values.Add(value)
    }

    func Values() -> List<int> {
        return _values
    }
}

extension CounterExt for Counter {
    func GetEnumerator() -> List<int>.Enumerator {
        return self.Values().GetEnumerator()
    }
}

class Runner {
    func Run() {
        val counter = Counter()
        counter.Add(1)
        counter.Add(2)

        for item in counter {
            val value = item
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();

        boundFor.Iteration.GetEnumeratorMethod.ShouldNotBeNull();
        boundFor.Iteration.GetEnumeratorMethod!.IsExtensionMethod.ShouldBeTrue();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void For_WithNonEnumerableCollection_ReportsDiagnostic()
    {
        const string source = """
val number = 42

for item in number {
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Select(d => d.Id).ShouldContain("RAV1503");
    }

    [Fact]
    public void For_WithPatternTarget_LowersBodyToPatternGuard()
    {
        const string source = """
val points = [(0, 0), (1, 0), (1, 1)]

for (val x, 0) in points {
    _ = x
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();

        boundFor.Local.ShouldNotBeNull();
        var guard = boundFor.Body.ShouldBeOfType<BoundIfStatement>();
        var isPattern = guard.Condition.ShouldBeOfType<BoundIsPatternExpression>();
        isPattern.Expression.ShouldBeOfType<BoundLocalAccess>()
            .Local.ShouldBe(boundFor.Local);
        isPattern.Pattern.ShouldBeOfType<BoundPositionalPattern>();
    }
}
