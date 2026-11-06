using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Compilation;

namespace Raven.CodeAnalysis.Tests.Workspaces;

[Trait("Target", "Shared")]
public sealed class IncrementalPlanContractTests
{
    [Fact]
    public void EqualOwnerCoordinatesCanRetrieveAChange()
    {
        var span = new TextSpan(4, 12);
        var owner = new ExecutableOwnerDescriptor(span, SyntaxKind.MethodDeclaration);
        var change = new OwnerRelativeTextChange(new TextSpan(2, 3), new TextSpan(2, 5), OwnerRelativeChangeKind.BodyExpression);
        var changes = ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange>.Empty.Add(owner, change);
        Assert.Equal(change, changes[new ExecutableOwnerDescriptor(TextSpan.FromBounds(4, 16), SyntaxKind.MethodDeclaration)]);
        Assert.False(changes.ContainsKey(owner with { Kind = SyntaxKind.FunctionStatement }));
        Assert.False(changes.ContainsKey(owner with { Span = default }));
        var (previous, current, kind) = change;
        Assert.Equal((2, 3, 2, 5, OwnerRelativeChangeKind.BodyExpression),
            (previous.Start, previous.Length, current.Start, current.Length, kind));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ProjectionRetainsTreeIdentityAndOwnerMatches(bool blocksTransfer)
    {
        var previous = SyntaxTree.ParseText("class Old {}");
        var current = SyntaxTree.ParseText("class New {}");
        var oldOwner = new ExecutableOwnerDescriptor(new TextSpan(4, 3), SyntaxKind.ClassDeclaration);
        var newOwner = oldOwner with { Span = new TextSpan(5, 4) };
        var matches = ImmutableArray.Create(new MatchedExecutableOwner(previous, newOwner, oldOwner));
        var changes = ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange>.Empty
            .Add(newOwner, new(default, new TextSpan(0, 1), OwnerRelativeChangeKind.SignatureOrDeclaration));
        var changed = new IncrementalChangedSyntaxTree(current, previous, [newOwner], matches, changes, blocksTransfer, true);
        var matched = changed.ToMatchedSyntaxTree();
        Assert.Same(current, matched.CurrentTree);
        Assert.Same(previous, matched.PreviousTree);
        Assert.Equal(matches, matched.Matches);
        Assert.Same(changes, matched.OwnerChanges);
        Assert.Equal(blocksTransfer, matched.BlocksSemanticDiagnosticTransfer);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PlanFiltersUnmatchedTreesWithoutIgnoringTheirRebindFlag(bool rebind)
    {
        var previous = SyntaxTree.ParseText("class Old {}");
        var current = SyntaxTree.ParseText("class New {}");
        var owner = new ExecutableOwnerDescriptor(current.GetRoot().Span, SyntaxKind.CompilationUnit);
        var matches = ImmutableArray.Create(new MatchedExecutableOwner(previous, owner, owner));
        var changes = ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange>.Empty;
        var first = new IncrementalChangedSyntaxTree(current, previous, [], matches, changes, false, false);
        var second = first with { BlocksSemanticDiagnosticTransfer = true };
        var plan = new IncrementalCompilationPlan([previous], [
            first with { MatchedOwners = default, RequiresFullSemanticRebind = rebind },
            first, first with { MatchedOwners = [] }, second]);
        Assert.False(plan.BlocksSemanticDiagnosticTransfer);
        Assert.Equal(rebind, plan.RequiresFullSemanticRebind);
        Assert.Equal(new[] { first.ToMatchedSyntaxTree(), second.ToMatchedSyntaxTree() }, plan.MatchedSyntaxTrees.ToArray());
        Assert.Same(previous, Assert.Single(plan.ReusedSyntaxTrees));
    }

    [Fact]
    public void EmptyPlanHasNoMatchesOrRebind()
    {
        var plan = new IncrementalCompilationPlan([], []);
        Assert.Empty(plan.MatchedSyntaxTrees);
        Assert.False(plan.RequiresFullSemanticRebind);
    }
}
