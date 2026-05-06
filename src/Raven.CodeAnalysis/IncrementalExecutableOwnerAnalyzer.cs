using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class IncrementalExecutableOwnerAnalyzer
{
    public readonly record struct Result(
        ImmutableArray<Compilation.ExecutableOwnerDescriptor> ChangedOwners,
        ImmutableArray<Compilation.MatchedExecutableOwner> MatchedOwners,
        ImmutableDictionary<Compilation.ExecutableOwnerDescriptor, Compilation.OwnerRelativeTextChange> OwnerChanges,
        bool BlocksSemanticDiagnosticTransfer);

    public static Result Analyze(
        SyntaxTree previousTree,
        SyntaxTree currentTree)
    {
        var previousOwners = GetExecutableOwners(previousTree.GetRoot()).ToArray();
        var currentOwners = GetExecutableOwners(currentTree.GetRoot()).ToArray();
        var previousOwnersByKey = previousOwners
            .GroupBy(CreateReusableOwnerMatchKey)
            .ToDictionary(
                static group => group.Key,
                static group => new List<SyntaxNode>(group));
        var matchedCurrentToPrevious = new Dictionary<Compilation.ExecutableOwnerDescriptor, Compilation.ExecutableOwnerDescriptor>();
        var usedPreviousOwners = new HashSet<Compilation.ExecutableOwnerDescriptor>();
        var partiallyChangedMatchedOwners = new HashSet<Compilation.ExecutableOwnerDescriptor>();

        var changedBuilder = ImmutableArray.CreateBuilder<Compilation.ExecutableOwnerDescriptor>();
        var matchedBuilder = ImmutableArray.CreateBuilder<Compilation.MatchedExecutableOwner>();
        var ownerChanges = ImmutableDictionary.CreateBuilder<Compilation.ExecutableOwnerDescriptor, Compilation.OwnerRelativeTextChange>();

        foreach (var owner in currentOwners)
        {
            var descriptor = new Compilation.ExecutableOwnerDescriptor(owner.Span, owner.Kind);
            var matchKey = CreateReusableOwnerMatchKey(owner);
            if (!previousOwnersByKey.TryGetValue(matchKey, out var candidates))
            {
                changedBuilder.Add(descriptor);
                continue;
            }

            Compilation.ExecutableOwnerDescriptor? currentParentDescriptor =
                TryGetReusableContainingExecutableOwnerDescriptor(owner, out var parentDescriptor)
                    ? parentDescriptor
                    : null;
            SyntaxNode? previousOwner = null;

            foreach (var candidate in candidates)
            {
                var candidateDescriptor = new Compilation.ExecutableOwnerDescriptor(candidate.Span, candidate.Kind);
                if (usedPreviousOwners.Contains(candidateDescriptor))
                    continue;

                if (currentParentDescriptor is { } requiredParentDescriptor)
                {
                    if (!matchedCurrentToPrevious.TryGetValue(requiredParentDescriptor, out var matchedPreviousParentDescriptor))
                        continue;

                    if (!TryGetReusableContainingExecutableOwnerDescriptor(candidate, out var candidateParentDescriptor) ||
                        candidateParentDescriptor != matchedPreviousParentDescriptor)
                    {
                        continue;
                    }

                    if (ownerChanges.TryGetValue(requiredParentDescriptor, out var parentChange) &&
                        !CanReuseNestedOwnerAcrossChangedParent(
                            candidate,
                            owner,
                            candidateParentDescriptor,
                            requiredParentDescriptor,
                            parentChange))
                    {
                        continue;
                    }
                }

                previousOwner = candidate;
                usedPreviousOwners.Add(candidateDescriptor);
                break;
            }

            if (previousOwner is null)
            {
                changedBuilder.Add(descriptor);
                continue;
            }

            var previousDescriptor = new Compilation.ExecutableOwnerDescriptor(previousOwner.Span, previousOwner.Kind);
            matchedCurrentToPrevious[descriptor] = previousDescriptor;
            matchedBuilder.Add(new Compilation.MatchedExecutableOwner(
                previousTree,
                descriptor,
                previousDescriptor));

            if (TryComputeOwnerRelativeTextChange(previousOwner, owner, out var ownerChange))
            {
                ownerChanges[descriptor] = ownerChange;
                partiallyChangedMatchedOwners.Add(descriptor);
                changedBuilder.Add(descriptor);
            }
        }

        return new Result(
            changedBuilder.ToImmutable(),
            matchedBuilder.ToImmutable(),
            ownerChanges.ToImmutable(),
            HasChangeOutsideExecutableBody(previousTree, currentTree));
    }

    private static (SyntaxKind Kind, string Identity) CreateReusableOwnerMatchKey(SyntaxNode owner)
    {
        return owner switch
        {
            MethodDeclarationSyntax method => (owner.Kind, CreateMethodDeclarationMatchIdentity(method)),
            FunctionStatementSyntax function => (owner.Kind, CreateFunctionStatementMatchIdentity(function)),
            ConstructorDeclarationSyntax ctor => (owner.Kind, CreateConstructorDeclarationMatchIdentity(ctor)),
            ParameterlessConstructorDeclarationSyntax => (owner.Kind, "ctor:0"),
            PropertyDeclarationSyntax property => (owner.Kind, $"property:{property.Identifier.ValueText}"),
            EventDeclarationSyntax @event => (owner.Kind, $"event:{@event.Identifier.ValueText}"),
            AccessorDeclarationSyntax accessor => (owner.Kind, $"accessor:{accessor.Keyword.Kind}"),
            _ => (owner.Kind, owner.ToFullString())
        };
    }

    private static string CreateMethodDeclarationMatchIdentity(MethodDeclarationSyntax method)
    {
        var explicitInterface = method.ExplicitInterfaceSpecifier?.Name.ToString() ?? string.Empty;
        var parameterTypes = FormatParameterTypeIdentity(method.ParameterList?.Parameters);
        var returnType = method.ReturnType?.Type.ToString() ?? "?";
        return $"method:{explicitInterface}:{method.Identifier.ValueText}:{method.TypeParameterList?.Parameters.Count ?? 0}:({parameterTypes}):{returnType}";
    }

    private static string CreateFunctionStatementMatchIdentity(FunctionStatementSyntax function)
    {
        var parameterTypes = FormatParameterTypeIdentity(function.ParameterList?.Parameters);
        var returnType = function.ReturnType?.Type.ToString() ?? "?";
        return $"function:{function.Identifier.ValueText}:{function.TypeParameterList?.Parameters.Count ?? 0}:({parameterTypes}):{returnType}";
    }

    private static string CreateConstructorDeclarationMatchIdentity(ConstructorDeclarationSyntax ctor)
    {
        var parameterTypes = FormatParameterTypeIdentity(ctor.ParameterList?.Parameters);
        return $"ctor:({parameterTypes})";
    }

    private static string FormatParameterTypeIdentity(IEnumerable<ParameterSyntax>? parameters)
    {
        if (parameters is null)
            return string.Empty;

        return string.Join(
            ",",
            parameters.Select(static parameter =>
            {
                var refKind = ParameterSyntaxUtilities.GetRefKind(parameter);
                var type = parameter.TypeAnnotation?.Type.ToString() ?? "?";
                return $"{refKind}:{type}";
            }));
    }

    private static bool TryComputeOwnerRelativeTextChange(
        SyntaxNode previousOwner,
        SyntaxNode currentOwner,
        out Compilation.OwnerRelativeTextChange ownerChange)
    {
        var previousText = previousOwner.ToString();
        var currentText = currentOwner.ToString();

        var start = 0;
        while (start < previousText.Length &&
               start < currentText.Length &&
               previousText[start] == currentText[start])
        {
            start++;
        }

        if (start == previousText.Length && start == currentText.Length)
        {
            ownerChange = default;
            return false;
        }

        var previousEnd = previousText.Length - 1;
        var currentEnd = currentText.Length - 1;

        while (previousEnd >= start &&
               currentEnd >= start &&
               previousText[previousEnd] == currentText[currentEnd])
        {
            previousEnd--;
            currentEnd--;
        }

        var previousSpan = new Text.TextSpan(start, previousEnd - start + 1);
        var currentSpan = new Text.TextSpan(start, currentEnd - start + 1);
        ownerChange = new Compilation.OwnerRelativeTextChange(
            previousSpan,
            currentSpan,
            ClassifyOwnerRelativeChange(previousOwner, previousSpan, currentOwner, currentSpan));
        return true;
    }

    private static Compilation.OwnerRelativeChangeKind ClassifyOwnerRelativeChange(
        SyntaxNode previousOwner,
        Text.TextSpan previousChangedSpan,
        SyntaxNode currentOwner,
        Text.TextSpan currentChangedSpan)
    {
        if (!TryGetExecutableBodyRelativeStart(previousOwner, out var previousBodyStart) ||
            !TryGetExecutableBodyRelativeStart(currentOwner, out var currentBodyStart))
        {
            return Compilation.OwnerRelativeChangeKind.Unknown;
        }

        if (previousChangedSpan.Start < previousBodyStart ||
            currentChangedSpan.Start < currentBodyStart)
        {
            return Compilation.OwnerRelativeChangeKind.SignatureOrDeclaration;
        }

        return IntersectsBodyDeclarationChange(previousOwner, previousChangedSpan) ||
               IntersectsBodyDeclarationChange(currentOwner, currentChangedSpan)
            ? Compilation.OwnerRelativeChangeKind.BodyDeclaration
            : Compilation.OwnerRelativeChangeKind.BodyExpression;
    }

    private static bool IntersectsBodyDeclarationChange(SyntaxNode owner, Text.TextSpan ownerRelativeChangedSpan)
    {
        return owner.DescendantNodes()
            .Any(node => IsBodyDeclarationNode(node) &&
                         OverlapsOrContainsInsertion(ToOwnerRelativeSpan(owner, node), ownerRelativeChangedSpan));
    }

    private static bool OverlapsOrContainsInsertion(Text.TextSpan nodeSpan, Text.TextSpan changedSpan)
    {
        if (changedSpan.Start >= nodeSpan.Start && changedSpan.Start <= nodeSpan.End + 1)
            return true;

        return nodeSpan.IntersectsWith(changedSpan);
    }

    private static bool IsBodyDeclarationNode(SyntaxNode node)
    {
        return node is LocalDeclarationStatementSyntax
            or VariableDeclarationSyntax
            or VariableDeclaratorSyntax
            or FunctionStatementSyntax
            or TypeDeclarationSyntax;
    }

    private static Text.TextSpan ToOwnerRelativeSpan(SyntaxNode owner, SyntaxNode node)
        => new(node.Span.Start - owner.Span.Start, node.Span.Length);

    private static bool CanReuseNestedOwnerAcrossChangedParent(
        SyntaxNode previousOwner,
        SyntaxNode currentOwner,
        Compilation.ExecutableOwnerDescriptor previousParent,
        Compilation.ExecutableOwnerDescriptor currentParent,
        Compilation.OwnerRelativeTextChange parentChange)
    {
        var previousRelativeSpan = new Text.TextSpan(
            previousOwner.Span.Start - previousParent.Span.Start,
            previousOwner.Span.Length);
        var currentRelativeSpan = new Text.TextSpan(
            currentOwner.Span.Start - currentParent.Span.Start,
            currentOwner.Span.Length);

        if (previousRelativeSpan.End > parentChange.PreviousSpan.Start ||
            currentRelativeSpan.End > parentChange.CurrentSpan.Start)
        {
            return false;
        }

        return previousOwner.Kind == currentOwner.Kind &&
               string.Equals(previousOwner.ToString(), currentOwner.ToString(), StringComparison.Ordinal);
    }

    private static bool TryGetExecutableBodyRelativeStart(SyntaxNode owner, out int relativeStart)
    {
        SyntaxNode? body = owner switch
        {
            BaseMethodDeclarationSyntax method => (SyntaxNode?)method.Body ?? method.ExpressionBody,
            FunctionStatementSyntax function => (SyntaxNode?)function.Body ?? function.ExpressionBody,
            AccessorDeclarationSyntax accessor => (SyntaxNode?)accessor.Body ?? accessor.ExpressionBody,
            PropertyDeclarationSyntax property => (SyntaxNode?)property.ExpressionBody ?? (SyntaxNode?)property.AccessorList ?? property.Initializer,
            EventDeclarationSyntax @event => (SyntaxNode?)@event.AccessorList,
            FunctionExpressionSyntax function => function.ExpressionBody,
            GlobalStatementSyntax globalStatement => globalStatement.Statement,
            CompilationUnitSyntax compilationUnit => compilationUnit,
            _ => null
        };

        if (body is null)
        {
            relativeStart = default;
            return false;
        }

        relativeStart = body.Span.Start - owner.Span.Start;
        return relativeStart >= 0;
    }

    private static bool HasChangeOutsideExecutableBody(SyntaxTree previousTree, SyntaxTree currentTree)
    {
        var previousRoot = previousTree.GetRoot();
        var currentRoot = currentTree.GetRoot();

        foreach (var change in currentTree.GetChanges(previousTree))
        {
            if (!IsSpanWithinExecutableBody(previousRoot, change.Span))
                return true;

            var currentSpan = new Text.TextSpan(change.Span.Start, change.NewText.Length);
            if (!IsSpanWithinExecutableBody(currentRoot, currentSpan))
                return true;
        }

        return false;
    }

    private static bool IsSpanWithinExecutableBody(SyntaxNode root, Text.TextSpan span)
    {
        return GetExecutableOwners(root)
            .Where(static owner => owner is not CompilationUnitSyntax)
            .Any(owner => TryGetExecutableBodySpan(owner, out var bodySpan) &&
                          ContainsOrTouchesInsertion(bodySpan, span));
    }

    private static bool TryGetExecutableBodySpan(SyntaxNode owner, out Text.TextSpan bodySpan)
    {
        SyntaxNode? body = owner switch
        {
            BaseMethodDeclarationSyntax method => (SyntaxNode?)method.Body ?? method.ExpressionBody,
            FunctionStatementSyntax function => (SyntaxNode?)function.Body ?? function.ExpressionBody,
            AccessorDeclarationSyntax accessor => (SyntaxNode?)accessor.Body ?? accessor.ExpressionBody,
            PropertyDeclarationSyntax property => (SyntaxNode?)property.ExpressionBody ?? (SyntaxNode?)property.AccessorList ?? property.Initializer,
            EventDeclarationSyntax @event => @event.AccessorList,
            FunctionExpressionSyntax function => function.ExpressionBody,
            GlobalStatementSyntax globalStatement => globalStatement.Statement,
            _ => null
        };

        if (body is null)
        {
            bodySpan = default;
            return false;
        }

        bodySpan = body.Span;
        return true;
    }

    private static bool ContainsOrTouchesInsertion(Text.TextSpan container, Text.TextSpan span)
    {
        if (span.Length == 0)
            return span.Start >= container.Start && span.Start <= container.End + 1;

        return span.Start >= container.Start && span.End <= container.End;
    }

    private static IEnumerable<SyntaxNode> GetExecutableOwners(SyntaxNode root)
    {
        return root.DescendantNodesAndSelf().Where(static node =>
            node is FunctionExpressionSyntax
                or BaseMethodDeclarationSyntax
                or BaseConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or AccessorDeclarationSyntax
                or PropertyDeclarationSyntax
                or EventDeclarationSyntax
                or GlobalStatementSyntax
                or CompilationUnitSyntax);
    }

    private static bool TryGetReusableContainingExecutableOwnerDescriptor(
        SyntaxNode node,
        out Compilation.ExecutableOwnerDescriptor descriptor)
    {
        var parentOwner = node.Ancestors().FirstOrDefault(static current =>
            current is FunctionExpressionSyntax
                or BaseMethodDeclarationSyntax
                or BaseConstructorDeclarationSyntax
                or ParameterlessConstructorDeclarationSyntax
                or AccessorDeclarationSyntax
                or PropertyDeclarationSyntax
                or EventDeclarationSyntax
                or GlobalStatementSyntax);

        if (parentOwner is null)
        {
            descriptor = default;
            return false;
        }

        descriptor = new Compilation.ExecutableOwnerDescriptor(parentOwner.Span, parentOwner.Kind);
        return true;
    }
}
