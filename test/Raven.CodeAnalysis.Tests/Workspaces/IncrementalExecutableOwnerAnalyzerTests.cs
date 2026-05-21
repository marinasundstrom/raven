using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class IncrementalExecutableOwnerAnalyzerTests
{
    [Fact]
    public void Analyze_ClassifiesMethodParameterNameEdit_AsSignatureOrDeclarationChange()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(value: int) -> int {
                    return value
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(item: int) -> int {
                    return item
                }
            }
            """));
        var currentMethod = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var currentDescriptor = new Compilation.ExecutableOwnerDescriptor(currentMethod.Span, currentMethod.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.OwnerChanges.TryGetValue(currentDescriptor, out var change).ShouldBeTrue();
        change.Kind.ShouldBe(Compilation.OwnerRelativeChangeKind.SignatureOrDeclaration);
    }

    [Fact]
    public void Analyze_ClassifiesMethodLocalDeclarationInitializerEdit_AsBodyDeclarationChange()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(value: int) -> int {
                    val before = value
                    val changed = 1
                    return before + changed
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(value: int) -> int {
                    val before = value
                    val changed = 10
                    return before + changed
                }
            }
            """));
        var currentMethod = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var currentDescriptor = new Compilation.ExecutableOwnerDescriptor(currentMethod.Span, currentMethod.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.OwnerChanges.TryGetValue(currentDescriptor, out var change).ShouldBeTrue();
        change.Kind.ShouldBe(Compilation.OwnerRelativeChangeKind.BodyDeclaration);
    }

    [Fact]
    public void Analyze_ClassifiesMethodReturnExpressionEdit_AsBodyExpressionChange()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(value: int) -> int {
                    val before = value
                    val changed = 1
                    return before + changed
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(value: int) -> int {
                    val before = value
                    val changed = 1
                    return before - changed
                }
            }
            """));
        var currentMethod = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var currentDescriptor = new Compilation.ExecutableOwnerDescriptor(currentMethod.Span, currentMethod.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.OwnerChanges.TryGetValue(currentDescriptor, out var change).ShouldBeTrue();
        change.Kind.ShouldBe(Compilation.OwnerRelativeChangeKind.BodyExpression);
    }

    [Fact]
    public void TransferPolicy_DropsAllOwnerRelativeDescriptors_ForSignatureOrDeclarationChange()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(value: int) -> int {
                    return value
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Stable(item: int) -> int {
                    return item
                }
            }
            """));
        var previousMethod = previousTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var currentMethod = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var previousParameterType = previousMethod
            .DescendantNodes()
            .OfType<PredefinedTypeSyntax>()
            .First();
        var previousOwner = new Compilation.ExecutableOwnerDescriptor(previousMethod.Span, previousMethod.Kind);
        var currentOwner = new Compilation.ExecutableOwnerDescriptor(currentMethod.Span, currentMethod.Kind);
        var descriptorKey = new Compilation.OwnerRelativeDescriptorKey(
            previousOwner,
            previousParameterType.Span.Start - previousOwner.Span.Start,
            previousParameterType.Span.Length,
            previousParameterType.Kind);
        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.OwnerChanges.TryGetValue(currentOwner, out var change).ShouldBeTrue();
        change.Kind.ShouldBe(Compilation.OwnerRelativeChangeKind.SignatureOrDeclaration);

        var remapped = IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey(
            descriptorKey,
            previousTree,
            currentTree,
            currentOwner,
            change,
            out _);

        remapped.ShouldBeFalse();
    }

    [Fact]
    public void Analyze_MatchesSameNameSameArityOverload_BySignatureShape()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Pick(value: int) -> int {
                    return value
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Pick(text: string) -> string {
                    return text
                }

                func Pick(value: int) -> int {
                    return value
                }
            }
            """));
        var previousMethod = previousTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var currentIntMethod = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.ParameterList!.Parameters.Single().TypeAnnotation!.Type.ToString() == "int");
        var currentStringMethod = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.ParameterList!.Parameters.Single().TypeAnnotation!.Type.ToString() == "string");
        var previousDescriptor = new Compilation.ExecutableOwnerDescriptor(previousMethod.Span, previousMethod.Kind);
        var currentIntDescriptor = new Compilation.ExecutableOwnerDescriptor(currentIntMethod.Span, currentIntMethod.Kind);
        var currentStringDescriptor = new Compilation.ExecutableOwnerDescriptor(currentStringMethod.Span, currentStringMethod.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.MatchedOwners.ShouldContain(match =>
            match.CurrentOwner == currentIntDescriptor &&
            match.PreviousOwner == previousDescriptor);
        result.MatchedOwners.ShouldNotContain(match => match.CurrentOwner == currentStringDescriptor);
        result.ChangedOwners.ShouldContain(currentStringDescriptor);
    }

    [Fact]
    public void Analyze_MatchesGlobalStatement_ByTopLevelOrdinalAcrossBodyEdit()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            val first = 1
            val second = first + 1
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            val first = 10
            val second = first + 1
            """));
        var previousFirstGlobal = previousTree.GetRoot()
            .DescendantNodes()
            .OfType<GlobalStatementSyntax>()
            .First();
        var currentFirstGlobal = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<GlobalStatementSyntax>()
            .First();
        var currentDescriptor = new Compilation.ExecutableOwnerDescriptor(currentFirstGlobal.Span, currentFirstGlobal.Kind);
        var previousDescriptor = new Compilation.ExecutableOwnerDescriptor(previousFirstGlobal.Span, previousFirstGlobal.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.MatchedOwners.ShouldContain(match =>
            match.CurrentOwner == currentDescriptor &&
            match.PreviousOwner == previousDescriptor);
        result.ChangedOwners.ShouldContain(currentDescriptor);
        result.OwnerChanges.TryGetValue(currentDescriptor, out var change).ShouldBeTrue();
        change.Kind.ShouldBe(Compilation.OwnerRelativeChangeKind.BodyDeclaration);
    }

    [Fact]
    public void Analyze_ReusesNestedFunctionExpression_WhenParentEditIsBeforeNestedOwner()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Main() {
                    val route = "/vehicles"
                    val map = func (value: int) => value + 1
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Main() {
                    val route = "/vehicles-test"
                    val map = func (value: int) => value + 1
                }
            }
            """));
        var previousFunction = previousTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var currentFunction = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var currentRoot = currentTree.GetRoot();
        var currentDescriptor = new Compilation.ExecutableOwnerDescriptor(currentFunction.Span, currentFunction.Kind);
        var previousDescriptor = new Compilation.ExecutableOwnerDescriptor(previousFunction.Span, previousFunction.Kind);
        var currentRootDescriptor = new Compilation.ExecutableOwnerDescriptor(currentRoot.Span, currentRoot.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.MatchedOwners.ShouldContain(match =>
            match.CurrentOwner == currentDescriptor &&
            match.PreviousOwner == previousDescriptor);
        result.ChangedOwners.ShouldNotContain(currentDescriptor);
        result.ChangedOwners.ShouldNotContain(currentRootDescriptor);
        result.OwnerChanges.ShouldNotContainKey(currentDescriptor);
    }

    [Fact]
    public void Analyze_MatchesFunctionExpressionAcrossBodyEdit_ByStableSyntaxPath()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Main(values: int[]) {
                    val doubled = values.Select(value => value + 1)
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Main(values: int[]) {
                    val doubled = values.Select(value => value + 2)
                }
            }
            """));
        var previousFunction = previousTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var currentFunction = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var currentDescriptor = new Compilation.ExecutableOwnerDescriptor(currentFunction.Span, currentFunction.Kind);
        var previousDescriptor = new Compilation.ExecutableOwnerDescriptor(previousFunction.Span, previousFunction.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.MatchedOwners.ShouldContain(match =>
            match.CurrentOwner == currentDescriptor &&
            match.PreviousOwner == previousDescriptor);
        result.ChangedOwners.ShouldContain(currentDescriptor);
        result.OwnerChanges.TryGetValue(currentDescriptor, out var change).ShouldBeTrue();
        change.Kind.ShouldBe(Compilation.OwnerRelativeChangeKind.BodyExpression);
    }

    [Fact]
    public void Analyze_MatchesParentFunctionExpressionAcrossNestedLambdaTriviaEdit()
    {
        var previousTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Main(app: WebApplication, values: int[]) {
                    app.MapGet("/", func () => values.Select(value => value + 1))
                }
            }
            """));
        var currentTree = SyntaxTree.ParseText(SourceText.From(
            """
            class Edited {
                func Main(app: WebApplication, values: int[]) {
                    app.MapGet("/", func () => values .Select(value => value + 1))
                }
            }
            """));
        var previousFunctions = previousTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .ToArray();
        var currentFunctions = currentTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .ToArray();
        var previousOuter = previousFunctions.OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var currentOuter = currentFunctions.OfType<ParenthesizedFunctionExpressionSyntax>().Single();
        var previousInner = previousFunctions.OfType<SimpleFunctionExpressionSyntax>().Single();
        var currentInner = currentFunctions.OfType<SimpleFunctionExpressionSyntax>().Single();
        var currentOuterDescriptor = new Compilation.ExecutableOwnerDescriptor(currentOuter.Span, currentOuter.Kind);
        var previousOuterDescriptor = new Compilation.ExecutableOwnerDescriptor(previousOuter.Span, previousOuter.Kind);
        var currentInnerDescriptor = new Compilation.ExecutableOwnerDescriptor(currentInner.Span, currentInner.Kind);
        var previousInnerDescriptor = new Compilation.ExecutableOwnerDescriptor(previousInner.Span, previousInner.Kind);

        var result = IncrementalExecutableOwnerAnalyzer.Analyze(previousTree, currentTree);

        result.MatchedOwners.ShouldContain(match =>
            match.CurrentOwner == currentOuterDescriptor &&
            match.PreviousOwner == previousOuterDescriptor);
        result.MatchedOwners.ShouldContain(match =>
            match.CurrentOwner == currentInnerDescriptor &&
            match.PreviousOwner == previousInnerDescriptor);
        result.ChangedOwners.ShouldNotContain(currentOuterDescriptor);
        result.ChangedOwners.ShouldNotContain(currentInnerDescriptor);
    }
}
