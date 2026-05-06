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
}
