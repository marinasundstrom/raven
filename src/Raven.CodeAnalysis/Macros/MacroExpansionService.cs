using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroExpansionService
{
    private static readonly DiagnosticDescriptor s_macroExpansionFailed = DiagnosticDescriptor.Create(
        "RAVM020",
        "Macro expansion failed",
        "",
        "",
        "Macro '{0}' failed during expansion: {1}",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    public static MacroExpansionResult? ExpandAttachedMacro(
        Compilation compilation,
        SemanticModel semanticModel,
        AttributeSyntax attribute,
        SyntaxNode targetDeclaration,
        DiagnosticBag diagnostics,
        CancellationToken cancellationToken = default)
    {
        if (!MacroSemanticValidator.TryResolveAttachedMacro(compilation, attribute, targetDeclaration, diagnostics, out var loaded))
            return null;

        try
        {
            var context = new AttachedMacroContext(compilation, semanticModel, attribute, targetDeclaration, cancellationToken);
            var result = loaded.Macro.Expand(context) ?? MacroExpansionResult.Empty;
            result = ContextualizeExpansionResult(targetDeclaration, result);
            RegisterGeneratedSyntaxTrees(compilation, semanticModel, result);

            foreach (var diagnostic in result.Diagnostics)
                diagnostics.Report(diagnostic);

            return result;
        }
        catch (Exception ex)
        {
            diagnostics.Report(Diagnostic.Create(
                s_macroExpansionFailed,
                attribute.Name.GetLocation(),
                loaded.Macro.Name,
                ex.Message));
            return null;
        }
    }

    private static MacroExpansionResult ContextualizeExpansionResult(
        SyntaxNode targetDeclaration,
        MacroExpansionResult result)
    {
        if (result == MacroExpansionResult.Empty)
            return result;

        if (targetDeclaration is not MemberDeclarationSyntax targetMember ||
            targetMember.Parent is not TypeDeclarationSyntax containingType)
        {
            return result;
        }

        var rewrittenMembers = new List<MemberDeclarationSyntax>(containingType.Members.Count +
            result.IntroducedMembers.Length +
            result.PeerDeclarations.Length);
        var introducedStartIndex = -1;
        var replacementIndex = -1;
        var peerStartIndex = -1;

        foreach (var member in containingType.Members)
        {
            if (!ReferenceEquals(member, targetMember))
            {
                rewrittenMembers.Add(member);
                continue;
            }

            introducedStartIndex = rewrittenMembers.Count;
            rewrittenMembers.AddRange(result.IntroducedMembers);

            replacementIndex = rewrittenMembers.Count;
            rewrittenMembers.Add(result.ReplacementDeclaration as MemberDeclarationSyntax ?? targetMember);

            peerStartIndex = rewrittenMembers.Count;
            rewrittenMembers.AddRange(result.PeerDeclarations);
        }

        if (introducedStartIndex < 0 || replacementIndex < 0)
            return result;

        var rewrittenContainingType = RewriteContainingTypeMembers(containingType, SyntaxFactory.List(rewrittenMembers));
        if (rewrittenContainingType is null)
            return result;

        var contextualContainingType = (TypeDeclarationSyntax)rewrittenContainingType.WithParent(containingType.Parent, containingType.Position);
        var contextualMembers = contextualContainingType.Members;

        return new MacroExpansionResult
        {
            ReplacementDeclaration = contextualMembers[replacementIndex],
            IntroducedMembers = SliceMembers(contextualMembers, introducedStartIndex, result.IntroducedMembers.Length),
            PeerDeclarations = SliceMembers(contextualMembers, peerStartIndex, result.PeerDeclarations.Length),
            Diagnostics = result.Diagnostics
        };
    }

    private static ImmutableArray<MemberDeclarationSyntax> SliceMembers(
        SyntaxList<MemberDeclarationSyntax> members,
        int startIndex,
        int count)
    {
        if (count <= 0 || startIndex < 0)
            return ImmutableArray<MemberDeclarationSyntax>.Empty;

        var builder = ImmutableArray.CreateBuilder<MemberDeclarationSyntax>(count);
        for (var i = 0; i < count; i++)
            builder.Add(members[startIndex + i]);

        return builder.ToImmutable();
    }

    private static TypeDeclarationSyntax? RewriteContainingTypeMembers(
        TypeDeclarationSyntax containingType,
        SyntaxList<MemberDeclarationSyntax> members)
    {
        return containingType switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.WithMembers(members),
            StructDeclarationSyntax structDeclaration => structDeclaration.WithMembers(members),
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.WithMembers(members),
            InterfaceDeclarationSyntax interfaceDeclaration => interfaceDeclaration.WithMembers(members),
            _ => null
        };
    }

    private static void RegisterGeneratedSyntaxTrees(
        Compilation compilation,
        SemanticModel semanticModel,
        MacroExpansionResult result)
    {
        RegisterSyntaxTree(compilation, semanticModel, result.ReplacementDeclaration);

        foreach (var member in result.IntroducedMembers)
            RegisterSyntaxTree(compilation, semanticModel, member);

        foreach (var declaration in result.PeerDeclarations)
            RegisterSyntaxTree(compilation, semanticModel, declaration);
    }

    private static void RegisterSyntaxTree(
        Compilation compilation,
        SemanticModel semanticModel,
        SyntaxNode? node)
    {
        if (node?.SyntaxTree is not { } syntaxTree)
            return;

        compilation.RegisterGeneratedSyntaxTree(syntaxTree, semanticModel);
    }
}
