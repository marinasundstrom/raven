using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public CompilationUnitSyntax GetExpandedRoot(CancellationToken cancellationToken = default)
    {
        using var semanticAccess = EnterSemanticAccess(cancellationToken);

        cancellationToken.ThrowIfCancellationRequested();
        InvalidateStaleFreestandingMacroExpansions();
        EnsureDiagnosticBindingCompleted();

        if (_expandedRoot is not null)
            return _expandedRoot;

        lock (_expandedRootGate)
        {
            if (_expandedRoot is not null)
                return _expandedRoot;

            var root = SyntaxTree.GetRoot(cancellationToken);
            var rewrittenMembers = RewriteMemberList(root.Members, this, cancellationToken);
            var expandedRoot = root.WithMembers(rewrittenMembers);
            expandedRoot = (CompilationUnitSyntax)RewriteFreestandingMacros(expandedRoot, this, cancellationToken);
            _expandedRoot = Formatter.Format(expandedRoot);
            return _expandedRoot;
        }
    }

    public ImmutableArray<SyntaxNode> GetExpandedDeclaration(
        AttributeSyntax attribute,
        CancellationToken cancellationToken = default)
    {
        ValidateSyntaxNode(attribute, nameof(attribute));

        using var semanticAccess = EnterSemanticAccess(cancellationToken);
        cancellationToken.ThrowIfCancellationRequested();
        EnsureDiagnosticBindingCompleted();

        if (_expandedDeclarationCache.TryGetValue(attribute, out var cached))
            return cached;

        var expandedDeclaration = ComputeExpandedDeclaration(attribute, cancellationToken);
        _expandedDeclarationCache.TryAdd(attribute, expandedDeclaration);
        return expandedDeclaration;
    }

    private ImmutableArray<SyntaxNode> ComputeExpandedDeclaration(
        AttributeSyntax attribute,
        CancellationToken cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(attribute);
        cancellationToken.ThrowIfCancellationRequested();
        EnsureDiagnosticBindingCompleted();
        if (TryGetMacroTarget(attribute) is not { } targetDeclaration)
            return ImmutableArray<SyntaxNode>.Empty;

        if (targetDeclaration is MemberDeclarationSyntax memberDeclaration)
            return [.. RewriteMember(memberDeclaration, this, cancellationToken)];

        var sections = new List<SyntaxNode>();
        foreach (var macroAttribute in targetDeclaration.ChildNodes().OfType<AttributeListSyntax>().SelectMany(static list => list.Attributes))
        {
            if (!macroAttribute.IsMacroAttribute())
                continue;

            var expansion = GetMacroExpansion(macroAttribute, cancellationToken);
            if (expansion is null)
                continue;

            sections.AddRange(expansion.IntroducedMembers);
            sections.Add(expansion.ReplacementDeclaration ?? targetDeclaration);
            sections.AddRange(expansion.PeerDeclarations);
        }

        return [.. sections];
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

        if (member is GlobalStatementSyntax globalStatement &&
            TryGetDirectFreestandingMacro(globalStatement, out var globalInvocation))
        {
            var expansion = semanticModel.GetMacroExpansion(globalInvocation, cancellationToken);
            if (expansion is null || (!expansion.HasMemberExpansion && expansion.Node is null))
            {
                yield return member;
                yield break;
            }

            if (expansion.HasMemberExpansion || expansion.Node is MemberDeclarationSyntax)
            {
                var expandedMembers = expansion.HasMemberExpansion
                    ? expansion.Members
                    : ImmutableArray.Create((MemberDeclarationSyntax)expansion.Node);
                var rewrittenMembers = RewriteExpandedMembers(
                        expandedMembers.Select(PrepareExpandedMember),
                        semanticModel.Compilation,
                        cancellationToken)
                    .ToArray();

                foreach (var generatedMember in IntegrateExpandedMembers(member, rewrittenMembers))
                    yield return generatedMember;
                yield break;
            }

            if (expansion.Statement is { } expandedStatement)
            {
                var rewrittenGlobal = globalStatement.WithStatement(expandedStatement);
                foreach (var generatedMember in IntegrateExpandedMembers(member, [rewrittenGlobal]))
                    yield return generatedMember;
                yield break;
            }
        }

        if (member is FreestandingMacroMemberDeclarationSyntax or FreestandingMacroDeclarationSyntax)
        {
            var expansion = member switch
            {
                FreestandingMacroMemberDeclarationSyntax invocation => semanticModel.GetMacroExpansion(invocation, cancellationToken),
                FreestandingMacroDeclarationSyntax declaration => semanticModel.GetMacroExpansion(declaration, cancellationToken),
                _ => null
            };
            if (expansion is null || (!expansion.HasMemberExpansion && expansion.Node is null))
            {
                yield return member;
                yield break;
            }

            var expandedMembers = expansion.HasMemberExpansion
                ? expansion.Members
                : expansion.Node is MemberDeclarationSyntax expandedMember
                    ? ImmutableArray.Create(expandedMember)
                    : ImmutableArray<MemberDeclarationSyntax>.Empty;
            var rewrittenMembers = RewriteExpandedMembers(
                    expandedMembers.Select(PrepareExpandedMember),
                    semanticModel.Compilation,
                    cancellationToken)
                .ToArray();

            foreach (var generatedMember in IntegrateExpandedMembers(member, rewrittenMembers))
                yield return generatedMember;
            yield break;
        }

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
                expansion.IntroducedMembers.Select(PrepareExpandedMember),
                semanticModel.Compilation,
                cancellationToken));

            if (expansion.ReplacementDeclaration is MemberDeclarationSyntax replacementMember)
            {
                rewrittenMember = RewriteMemberInternals(
                    PrepareExpandedMember(replacementMember),
                    GetSemanticModelForExpandedNode(semanticModel, replacementMember),
                    cancellationToken);
            }

            peerDeclarations.AddRange(RewriteExpandedMembers(
                expansion.PeerDeclarations.Select(PrepareExpandedMember),
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
            UnionDeclarationSyntax unionDeclaration => unionDeclaration.WithMembers(
                RewriteMemberList(unionDeclaration.Members, semanticModel, cancellationToken)),
            NamespaceDeclarationSyntax namespaceDeclaration => namespaceDeclaration.WithMembers(
                RewriteMemberList(namespaceDeclaration.Members, semanticModel, cancellationToken)),
            FileScopedNamespaceDeclarationSyntax fileScopedNamespace => fileScopedNamespace.WithMembers(
                RewriteMemberList(fileScopedNamespace.Members, semanticModel, cancellationToken)),
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

        var macroExpressions = node.DescendantNodesAndSelf()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Where(expression => IsOwnedBy(expression, node))
            .OrderByDescending(GetDepth)
            .ToArray();
        if (macroExpressions.Length == 0)
            return node;

        var scopes = new Dictionary<GreenNode, (SyntaxNode Scope, List<FreestandingMacroExpressionSyntax> Expressions)>(
            ReferenceEqualityComparer.Instance);
        foreach (var expression in macroExpressions)
        {
            var scope = GetFormattingScope(expression) ?? expression;
            if (!scopes.TryGetValue(scope.Green, out var entry))
            {
                entry = (scope, []);
                scopes.Add(scope.Green, entry);
            }

            entry.Expressions.Add(expression);
        }

        var scopeReplacements = new Dictionary<GreenNode, GreenNode>(ReferenceEqualityComparer.Instance);
        foreach (var (_, entry) in scopes)
        {
            var expressionReplacements = new Dictionary<GreenNode, GreenNode>(
                entry.Expressions.Count,
                ReferenceEqualityComparer.Instance);
            foreach (var expression in entry.Expressions)
            {
                var replacement = semanticModel.GetMacroExpansion(expression, cancellationToken)?.Expression is { } expandedExpression
                    ? PrepareExpandedExpression(expandedExpression, expression).Green
                    : expression.Green;
                expressionReplacements.Add(expression.Green, replacement);
            }

            var rewrittenScope = entry.Scope.Green.ReplaceNodes(
                expressionReplacements.ContainsKey,
                green => expressionReplacements[green]);
            var rewrittenScopeNode = rewrittenScope.CreateRed();
            if (entry.Expressions.Any(static expression => expression.TokenTree is not null))
            {
                rewrittenScopeNode = rewrittenScopeNode.WithTrailingTrivia(
                    entry.Scope.GetLastToken(includeZeroWidth: false).TrailingTrivia);
            }

            scopeReplacements.Add(
                entry.Scope.Green,
                rewrittenScopeNode.Green.WithAdditionalAnnotations(Formatter.Annotation));
        }

        var rewritten = node.Green.ReplaceNodes(
            scopeReplacements.ContainsKey,
            green => scopeReplacements[green]);
        return rewritten.CreateRed(node.Parent, node.Position);
    }

    private static bool IsOwnedBy(SyntaxNode node, SyntaxNode owner)
        => ReferenceEquals(GetOwningDeclaration(node), owner) || ReferenceEquals(node, owner);

    private static SyntaxNode? GetOwningDeclaration(SyntaxNode node)
        => node.AncestorsAndSelf().FirstOrDefault(static ancestor =>
            ancestor is MemberDeclarationSyntax or CompilationUnitSyntax);

    private static bool TryGetDirectFreestandingMacro(
        GlobalStatementSyntax globalStatement,
        out FreestandingMacroExpressionSyntax invocation)
    {
        if (globalStatement.Statement is ExpressionStatementSyntax
            {
                Expression: FreestandingMacroExpressionSyntax expression
            })
        {
            invocation = expression;
            return true;
        }

        invocation = null!;
        return false;
    }

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
        updatedMembers[0] = updatedMembers[0].WithLeadingTrivia(
            originalMember.GetFirstToken(includeZeroWidth: true).LeadingTrivia);
        updatedMembers[^1] = updatedMembers[^1].WithTrailingTrivia(
            originalMember.GetLastToken(includeZeroWidth: true).TrailingTrivia);

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

    private static MemberDeclarationSyntax PrepareExpandedMember(MemberDeclarationSyntax member)
        => (MemberDeclarationSyntax)ElasticizeFormattingTrivia(DetachNode(member))
            .WithAdditionalAnnotations(Formatter.Annotation);

    private static TNode PrepareExpandedExpression<TNode>(TNode node)
        where TNode : SyntaxNode
        => ElasticizeFormattingTrivia(DetachNode(node));

    private static TNode PrepareExpandedExpression<TNode>(
        TNode node,
        FreestandingMacroExpressionSyntax original)
        where TNode : SyntaxNode
    {
        var prepared = PrepareExpandedExpression(node);
        return original.TokenTree is null
            ? prepared
            : prepared
                .WithLeadingTrivia(
                    original.GetFirstToken(includeZeroWidth: true).LeadingTrivia)
                .WithTrailingTrivia(
                    original.GetLastToken(includeZeroWidth: true).TrailingTrivia);
    }

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
