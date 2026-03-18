using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public CompilationUnitSyntax GetExpandedRoot(CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        EnsureDiagnosticsCollected();

        var root = SyntaxTree.GetRoot(cancellationToken);
        var rewrittenMembers = RewriteMemberList(root.Members, this, cancellationToken);
        var expandedRoot = root.WithMembers(rewrittenMembers);
        expandedRoot = (CompilationUnitSyntax)RewriteFreestandingMacros(expandedRoot, this, cancellationToken);
        return Formatter.Format(expandedRoot);
    }

    private static SyntaxList<MemberDeclarationSyntax> RewriteMemberList(
        SyntaxList<MemberDeclarationSyntax> members,
        SemanticModel semanticModel,
        CancellationToken cancellationToken)
    {
        var rewritten = new List<MemberDeclarationSyntax>();

        foreach (var member in members)
            rewritten.AddRange(RewriteMember(member, semanticModel, cancellationToken));

        return SyntaxFactory.List(rewritten);
    }

    private static IEnumerable<MemberDeclarationSyntax> RewriteMember(
        MemberDeclarationSyntax member,
        SemanticModel semanticModel,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var rewrittenMember = RewriteMemberInternals(member, semanticModel, cancellationToken);
        var introducedMembers = new List<MemberDeclarationSyntax>();
        var peerDeclarations = new List<MemberDeclarationSyntax>();

        foreach (var attribute in member.AttributeLists.SelectMany(static list => list.Attributes))
        {
            if (!attribute.IsMacroAttribute())
                continue;

            var expansion = semanticModel.GetMacroExpansion(attribute, cancellationToken);
            if (expansion is null)
                continue;

            introducedMembers.AddRange(RewriteExpandedMembers(
                expansion.IntroducedMembers.Select(FormatExpandedMember),
                semanticModel.Compilation,
                cancellationToken));

            if (expansion.ReplacementDeclaration is MemberDeclarationSyntax replacementMember)
            {
                rewrittenMember = RewriteMemberInternals(
                    FormatExpandedMember(replacementMember),
                    GetSemanticModelForExpandedNode(semanticModel, replacementMember),
                    cancellationToken);
            }

            peerDeclarations.AddRange(RewriteExpandedMembers(
                expansion.PeerDeclarations.Select(FormatExpandedMember),
                semanticModel.Compilation,
                cancellationToken));
        }

        foreach (var expandedMember in IntegrateExpandedMembers(member, [
                     .. introducedMembers,
                     rewrittenMember,
                     .. peerDeclarations]))
        {
            yield return expandedMember;
        }
    }

    private static IEnumerable<MemberDeclarationSyntax> RewriteExpandedMembers(
        IEnumerable<MemberDeclarationSyntax> members,
        Compilation compilation,
        CancellationToken cancellationToken)
    {
        foreach (var member in members)
        {
            var semanticModel = GetSemanticModelForExpandedNode(compilation, member);
            foreach (var rewrittenMember in RewriteMember(member, semanticModel, cancellationToken))
                yield return rewrittenMember;
        }
    }

    private static MemberDeclarationSyntax RewriteMemberInternals(
        MemberDeclarationSyntax member,
        SemanticModel semanticModel,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var rewrittenMember = member switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.WithMembers(
                RewriteMemberList(classDeclaration.Members, semanticModel, cancellationToken)),
            StructDeclarationSyntax structDeclaration => structDeclaration.WithMembers(
                RewriteMemberList(structDeclaration.Members, semanticModel, cancellationToken)),
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.WithMembers(
                RewriteMemberList(recordDeclaration.Members, semanticModel, cancellationToken)),
            InterfaceDeclarationSyntax interfaceDeclaration => interfaceDeclaration.WithMembers(
                RewriteMemberList(interfaceDeclaration.Members, semanticModel, cancellationToken)),
            _ => member
        };

        return (MemberDeclarationSyntax)RewriteFreestandingMacros(rewrittenMember, semanticModel, cancellationToken);
    }

    private static SyntaxNode RewriteFreestandingMacros(
        SyntaxNode node,
        SemanticModel semanticModel,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var current = node;
        var macroExpressions = node.DescendantNodesAndSelf()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Where(expression => IsOwnedBy(expression, node))
            .OrderByDescending(GetDepth)
            .ToArray();

        foreach (var expression in macroExpressions)
        {
            var expansion = semanticModel.GetMacroExpansion(expression, cancellationToken);
            if (expansion?.Expression is not { } replacementExpression)
                continue;

            var preparedExpression = PrepareExpandedExpression(replacementExpression);
            var formattingScope = GetFormattingScope(expression);

            if (formattingScope is null)
            {
                current = current.ReplaceNode(
                    expression,
                    preparedExpression.WithAdditionalAnnotations(Formatter.Annotation));
                continue;
            }

            var rewrittenScope = formattingScope
                .ReplaceNode(expression, preparedExpression)
                .WithAdditionalAnnotations(Formatter.Annotation);

            current = current.ReplaceNode(formattingScope, rewrittenScope);
        }

        return current;
    }

    private static bool IsOwnedBy(SyntaxNode node, SyntaxNode owner)
        => ReferenceEquals(GetOwningDeclaration(node), owner) || ReferenceEquals(node, owner);

    private static SyntaxNode? GetOwningDeclaration(SyntaxNode node)
        => node.AncestorsAndSelf().FirstOrDefault(static ancestor =>
            ancestor is MemberDeclarationSyntax or CompilationUnitSyntax);

    private static int GetDepth(SyntaxNode node)
        => node.Ancestors().Count();

    private static SyntaxNode? GetFormattingScope(SyntaxNode node)
        => node.Ancestors().FirstOrDefault(static ancestor =>
            ancestor is StatementSyntax
                or AccessorDeclarationSyntax
                or ArrowExpressionClauseSyntax
                or MemberDeclarationSyntax);

    private static SemanticModel GetSemanticModelForExpandedNode(SemanticModel fallbackModel, SyntaxNode node)
        => node.SyntaxTree is { } syntaxTree
            ? fallbackModel.Compilation.GetSemanticModel(syntaxTree)
            : fallbackModel;

    private static SemanticModel GetSemanticModelForExpandedNode(Compilation compilation, SyntaxNode node)
        => node.SyntaxTree is { } syntaxTree
            ? compilation.GetSemanticModel(syntaxTree)
            : compilation.GetSemanticModel(compilation.SyntaxTrees[0]);

    private static TNode DetachNode<TNode>(TNode node)
        where TNode : SyntaxNode
        => node.Parent is null
            ? node
            : (TNode)node.Green.CreateRed();

    private static IEnumerable<MemberDeclarationSyntax> IntegrateExpandedMembers(
        MemberDeclarationSyntax originalMember,
        IReadOnlyList<MemberDeclarationSyntax> expandedMembers)
    {
        if (expandedMembers.Count == 0)
            yield break;

        var updatedMembers = expandedMembers.ToArray();
        updatedMembers[0] = updatedMembers[0].WithLeadingTrivia(originalMember.GetFirstToken(includeZeroWidth: true).LeadingTrivia);
        updatedMembers[^1] = updatedMembers[^1].WithTrailingTrivia(originalMember.GetLastToken(includeZeroWidth: true).TrailingTrivia);

        for (var i = 0; i < updatedMembers.Length; i++)
        {
            yield return i < updatedMembers.Length - 1
                ? EnsureTrailingLineBreaks(updatedMembers[i], lineBreakCount: 1)
                : updatedMembers[i];
        }
    }

    private static MemberDeclarationSyntax EnsureTrailingLineBreaks(
        MemberDeclarationSyntax member,
        int lineBreakCount)
    {
        var lastToken = member.GetLastToken(includeZeroWidth: true);
        var preservedTrivia = new List<SyntaxTrivia>();

        foreach (var trivia in lastToken.TrailingTrivia)
        {
            if (!IsFormattingTrivia(trivia.Kind))
                preservedTrivia.Add(trivia);
        }

        for (var i = 0; i < lineBreakCount; i++)
            preservedTrivia.Add(SyntaxFactory.LineFeed);

        return member.ReplaceToken(
            lastToken,
            lastToken.WithTrailingTrivia(SyntaxFactory.TriviaList(preservedTrivia)));
    }

    private static SyntaxTriviaList ToElasticTrivia(SyntaxTriviaList triviaList)
    {
        var rewritten = new List<SyntaxTrivia>(triviaList.Count);

        foreach (var trivia in triviaList)
        {
            rewritten.Add(trivia.Kind switch
            {
                SyntaxKind.WhitespaceTrivia => SyntaxFactory.ElasticWhitespace(trivia.Text),
                SyntaxKind.TabTrivia => SyntaxFactory.ElasticTab,
                SyntaxKind.LineFeedTrivia => SyntaxFactory.ElasticLineFeed,
                SyntaxKind.CarriageReturnTrivia => SyntaxFactory.ElasticCarriageReturn,
                SyntaxKind.CarriageReturnLineFeedTrivia => SyntaxFactory.ElasticCarriageReturnLineFeed,
                SyntaxKind.EndOfLineTrivia => SyntaxFactory.ElasticLineFeed,
                _ => trivia
            });
        }

        return SyntaxFactory.TriviaList(rewritten);
    }

    private static bool IsFormattingTrivia(SyntaxKind kind)
    {
        return kind is SyntaxKind.WhitespaceTrivia
            or SyntaxKind.TabTrivia
            or SyntaxKind.LineFeedTrivia
            or SyntaxKind.CarriageReturnTrivia
            or SyntaxKind.CarriageReturnLineFeedTrivia
            or SyntaxKind.EndOfLineTrivia;
    }

    private static MemberDeclarationSyntax FormatExpandedMember(MemberDeclarationSyntax member)
        => Formatter.Format(ElasticizeFormattingTrivia(DetachNode(member)));

    private static TNode PrepareExpandedExpression<TNode>(TNode node)
        where TNode : SyntaxNode
        => ElasticizeFormattingTrivia(DetachNode(node));

    private static TNode ElasticizeFormattingTrivia<TNode>(TNode node)
        where TNode : SyntaxNode
    {
        var tokens = node.DescendantTokens().ToArray();
        if (tokens.Length == 0)
            return node;

        return (TNode)node.ReplaceTokens(tokens, static (original, _) =>
            original
                .WithLeadingTrivia(ToElasticTrivia(original.LeadingTrivia))
                .WithTrailingTrivia(ToElasticTrivia(original.TrailingTrivia)));
    }
}
