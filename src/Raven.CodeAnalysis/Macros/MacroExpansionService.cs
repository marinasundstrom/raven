using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Runtime.ExceptionServices;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroExpansionService
{
    private static readonly DiagnosticDescriptor s_macroReportedDiagnostic = DiagnosticDescriptor.Create(
        "RAVM021",
        "Macro reported diagnostic",
        "",
        "",
        "Macro '{0}': {1}",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_macroExpansionFailed = DiagnosticDescriptor.Create(
        "RAVM020",
        "Macro expansion failed",
        "",
        "",
        "Macro '{0}' failed during expansion: {1}",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_macroExpansionCategoryMismatch = DiagnosticDescriptor.Create(
        "RAVM022",
        "Macro expansion has the wrong syntax category",
        "",
        "",
        "Macro '{0}' produced {1} syntax where {2} syntax is required.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    public static ImmutableDictionary<AttributeSyntax, MacroExpansionResult?> ExpandAttachedMacros(
        Compilation compilation,
        SemanticModel semanticModel,
        SyntaxNode targetDeclaration,
        DiagnosticBag diagnostics,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(targetDeclaration);

        var builder = ImmutableDictionary.CreateBuilder<AttributeSyntax, MacroExpansionResult?>();
        var currentDeclaration = targetDeclaration;

        foreach (var attribute in GetAttachedMacroAttributes(targetDeclaration))
        {
            cancellationToken.ThrowIfCancellationRequested();
            compilation.PerformanceInstrumentation.Macros.RecordAttachedExpansionInvocation();

            if (!MacroSemanticValidator.TryResolveAttachedMacro(compilation, attribute, targetDeclaration, diagnostics, out var loaded))
            {
                builder[attribute] = null;
                continue;
            }

            try
            {
                var context = new AttachedMacroContext(
                    compilation,
                    semanticModel,
                    attribute,
                    targetDeclaration,
                    currentDeclaration,
                    cancellationToken);
                if (!MacroParameterBinder.ValidateArguments(
                        loaded.Macro.Name,
                        attribute.Name.GetLocation(),
                        loaded.Descriptor.Parameters,
                        context.Arguments,
                        diagnostics))
                {
                    builder[attribute] = MacroExpansionResult.Empty;
                    continue;
                }

                var result = loaded.Executor.Expand(new MacroExecutionContext(
                        loaded.Executor,
                        context,
                        GetTypeArguments(semanticModel, attribute.Name),
                        context.Arguments,
                        diagnostics))
                    .AttachedResult ?? throw new InvalidOperationException(
                        $"Macro '{loaded.Executor.Name}' returned a freestanding result for an attached invocation.");
                result = AddReportedDiagnostics(result, context);
                result = ContextualizeExpansionResult(targetDeclaration, result);
                RegisterGeneratedSyntaxTrees(compilation, semanticModel, result);

                ReportMacroDiagnostics(diagnostics, loaded.Macro.Name, attribute.Name.GetLocation(), result.MacroDiagnostics);

                foreach (var diagnostic in result.Diagnostics)
                    diagnostics.Report(diagnostic);

                builder[attribute] = result;

                if (result.ReplacementDeclaration is { } replacementDeclaration)
                    currentDeclaration = replacementDeclaration;
            }
            catch (Exception ex)
            {
                var failure = UnwrapExpansionFailure(ex);
                RethrowCancellation(failure, cancellationToken);
                diagnostics.Report(Diagnostic.Create(
                    s_macroExpansionFailed,
                    attribute.Name.GetLocation(),
                    loaded.Macro.Name,
                    GetExpansionFailureMessage(failure)));
                builder[attribute] = null;
            }
        }

        return builder.ToImmutable();
    }

    public static FreestandingMacroExpansionResult? ExpandFreestandingMacro(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax expression,
        DiagnosticBag diagnostics,
        CancellationToken cancellationToken = default)
        => ExpandFreestandingMacro(
            compilation,
            semanticModel,
            FreestandingMacroInvocation.Create(expression),
            diagnostics,
            cancellationToken);

    public static FreestandingMacroExpansionResult? ExpandFreestandingMacro(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroMemberDeclarationSyntax member,
        DiagnosticBag diagnostics,
        CancellationToken cancellationToken = default)
        => ExpandFreestandingMacro(
            compilation,
            semanticModel,
            FreestandingMacroInvocation.Create(member),
            diagnostics,
            cancellationToken);

    private static FreestandingMacroExpansionResult? ExpandFreestandingMacro(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroInvocation invocation,
        DiagnosticBag diagnostics,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();
        compilation.PerformanceInstrumentation.Macros.RecordFreestandingExpansionInvocation();

        if (!MacroSemanticValidator.TryResolveFreestandingMacro(compilation, invocation, diagnostics, out var loaded))
            return null;

        try
        {
            FreestandingMacroExpansionResult result;
            if (invocation.TokenTree is not null)
            {
                var context = new TokenTreeMacroContext(
                    compilation,
                    semanticModel,
                    invocation,
                    loaded.Macro,
                    cancellationToken);
                result = ExecuteFreestanding(
                    loaded.Executor,
                    loaded.Descriptor,
                    context,
                    context.Arguments,
                    semanticModel,
                    invocation.Name,
                    diagnostics);
                result = AddReportedDiagnostics(result, context);
                result.FileDependencies = MergeFileDependencies(
                    result.FileDependencies,
                    context.GetFileDependencies());
            }
            else
            {
                var context = new FreestandingMacroContext(
                    compilation,
                    semanticModel,
                    invocation,
                    cancellationToken);
                result = ExecuteFreestanding(
                    loaded.Executor,
                    loaded.Descriptor,
                    context,
                    context.Arguments,
                    semanticModel,
                    invocation.Name,
                    diagnostics);
                result = AddReportedDiagnostics(result, context);
                result.FileDependencies = MergeFileDependencies(
                    result.FileDependencies,
                    context.GetFileDependencies());
            }

            result = ValidateExpansionCategory(loaded.Macro.Name, invocation, result, diagnostics);
            result = ContextualizeExpansionResult(invocation, result);
            RegisterGeneratedSyntaxTree(compilation, semanticModel, result.Node);
            foreach (var member in result.Members)
                RegisterGeneratedSyntaxTree(compilation, semanticModel, member);

            ReportMacroDiagnostics(diagnostics, loaded.Macro.Name, invocation.Name.GetLocation(), result.MacroDiagnostics);

            foreach (var diagnostic in result.Diagnostics)
                diagnostics.Report(diagnostic);

            return result;
        }
        catch (Exception ex)
        {
            var failure = UnwrapExpansionFailure(ex);
            RethrowCancellation(failure, cancellationToken);
            diagnostics.Report(Diagnostic.Create(
                s_macroExpansionFailed,
                invocation.Name.GetLocation(),
                loaded.Macro.Name,
                GetExpansionFailureMessage(failure)));
            return null;
        }
    }

    private static FreestandingMacroExpansionResult ExecuteFreestanding(
        IMacroExecutor executor,
        MacroDefinitionDescriptor descriptor,
        MacroContext context,
        ImmutableArray<MacroArgument> arguments,
        SemanticModel semanticModel,
        TypeSyntax name,
        DiagnosticBag diagnostics)
    {
        if (!MacroParameterBinder.ValidateArguments(
                executor.Name,
                name.GetLocation(),
                descriptor.Parameters,
                arguments,
                diagnostics))
        {
            return FreestandingMacroExpansionResult.Empty;
        }

        return executor.Expand(new MacroExecutionContext(
                executor,
                context,
                GetTypeArguments(semanticModel, name),
                arguments,
                diagnostics))
            .FreestandingResult ?? throw new InvalidOperationException(
                $"Macro '{executor.Name}' returned an attached result for a freestanding invocation.");
    }

    private static ImmutableArray<ITypeSymbol> GetTypeArguments(
        SemanticModel semanticModel,
        TypeSyntax name)
    {
        return semanticModel.ResolveMacroTypeArguments(name);
    }

    private static Exception UnwrapExpansionFailure(Exception exception)
    {
        while (exception is TargetInvocationException { InnerException: not null } invocationException)
            exception = invocationException.InnerException;

        return exception;
    }

    private static ImmutableArray<MacroFileDependency> MergeFileDependencies(
        ImmutableArray<MacroFileDependency> first,
        ImmutableArray<MacroFileDependency> second)
        => first
            .AddRange(second)
            .DistinctBy(static dependency => dependency.Path, StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

    private static void RethrowCancellation(Exception failure, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (failure is OperationCanceledException { CancellationToken.IsCancellationRequested: true })
            ExceptionDispatchInfo.Capture(failure).Throw();
    }

    private static string GetExpansionFailureMessage(Exception exception)
    {
        return string.IsNullOrWhiteSpace(exception.Message)
            ? exception.GetType().Name
            : exception.Message;
    }

    private static MacroExpansionResult AddReportedDiagnostics(
        MacroExpansionResult result,
        MacroContext context)
    {
        var diagnostics = context.GetReportedDiagnostics();
        var macroDiagnostics = context.GetReportedMacroDiagnostics();
        if (diagnostics.IsDefaultOrEmpty && macroDiagnostics.IsDefaultOrEmpty)
            return result;

        if (ReferenceEquals(result, MacroExpansionResult.Empty))
            result = new MacroExpansionResult();

        result.Diagnostics = Append(result.Diagnostics, diagnostics);
        result.MacroDiagnostics = Append(result.MacroDiagnostics, macroDiagnostics);
        return result;
    }

    private static FreestandingMacroExpansionResult AddReportedDiagnostics(
        FreestandingMacroExpansionResult result,
        MacroContext context)
    {
        var diagnostics = context.GetReportedDiagnostics();
        var macroDiagnostics = context.GetReportedMacroDiagnostics();
        if (diagnostics.IsDefaultOrEmpty && macroDiagnostics.IsDefaultOrEmpty)
            return result;

        if (ReferenceEquals(result, FreestandingMacroExpansionResult.Empty))
            result = new FreestandingMacroExpansionResult();

        result.Diagnostics = Append(result.Diagnostics, diagnostics);
        result.MacroDiagnostics = Append(result.MacroDiagnostics, macroDiagnostics);
        return result;
    }

    private static ImmutableArray<T> Append<T>(
        ImmutableArray<T> existing,
        ImmutableArray<T> additions)
        => existing.IsDefault ? additions : existing.AddRange(additions);

    private static MacroExpansionResult ContextualizeExpansionResult(
        SyntaxNode targetDeclaration,
        MacroExpansionResult result)
    {
        if (result == MacroExpansionResult.Empty)
            return result;

        if (targetDeclaration is not MemberDeclarationSyntax targetMember)
        {
            return result;
        }

        if (targetMember.Parent is not BaseTypeDeclarationSyntax containingType)
            return ContextualizeTopLevelReplacement(targetMember, result);

        var containingMembers = GetContainingTypeMembers(containingType);
        var rewrittenMembers = new List<MemberDeclarationSyntax>(containingMembers.Count +
            result.IntroducedMembers.Length +
            result.PeerDeclarations.Length);
        var introducedStartIndex = -1;
        var replacementIndex = -1;
        var peerStartIndex = -1;

        foreach (var member in containingMembers)
        {
            if (!IsTargetMember(member, targetMember))
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

        var contextualContainingType = (BaseTypeDeclarationSyntax)rewrittenContainingType.WithParent(containingType.Parent, containingType.Position);
        var contextualMembers = GetContainingTypeMembers(contextualContainingType);

        return new MacroExpansionResult
        {
            ReplacementDeclaration = contextualMembers[replacementIndex],
            IntroducedMembers = SliceMembers(contextualMembers, introducedStartIndex, result.IntroducedMembers.Length),
            PeerDeclarations = SliceMembers(contextualMembers, peerStartIndex, result.PeerDeclarations.Length),
            MacroDiagnostics = result.MacroDiagnostics,
            Diagnostics = result.Diagnostics
        };
    }

    private static MacroExpansionResult ContextualizeTopLevelReplacement(
        MemberDeclarationSyntax targetMember,
        MacroExpansionResult result)
    {
        var replacement = result.ReplacementDeclaration as MemberDeclarationSyntax;
        var contextualReplacement = replacement?.SyntaxTree is null && replacement is not null
            ? (MemberDeclarationSyntax)replacement.WithParent(
                targetMember.Parent,
                targetMember.Position)
            : replacement;
        var containingType = contextualReplacement as BaseTypeDeclarationSyntax ??
            targetMember as BaseTypeDeclarationSyntax;
        var introducedMembers = containingType is null
            ? result.IntroducedMembers
            : ContextualizeDetachedMembers(
                result.IntroducedMembers,
                containingType,
                containingType.OpenBraceToken.Span.End);
        var peerDeclarations = ContextualizeDetachedMembers(
            result.PeerDeclarations,
            targetMember.Parent,
            targetMember.Position);

        return new MacroExpansionResult
        {
            ReplacementDeclaration = contextualReplacement,
            IntroducedMembers = introducedMembers,
            PeerDeclarations = peerDeclarations,
            MacroDiagnostics = result.MacroDiagnostics,
            Diagnostics = result.Diagnostics
        };
    }

    private static ImmutableArray<MemberDeclarationSyntax> ContextualizeDetachedMembers(
        ImmutableArray<MemberDeclarationSyntax> members,
        SyntaxNode? parent,
        int position)
    {
        if (members.IsDefaultOrEmpty || parent is null)
            return members;

        var builder = ImmutableArray.CreateBuilder<MemberDeclarationSyntax>(members.Length);
        for (var index = 0; index < members.Length; index++)
        {
            var member = members[index];
            builder.Add(member.SyntaxTree is null
                ? (MemberDeclarationSyntax)member.WithParent(parent, position + index)
                : member);
        }

        return builder.MoveToImmutable();
    }

    private static bool IsTargetMember(MemberDeclarationSyntax candidate, MemberDeclarationSyntax target)
    {
        if (ReferenceEquals(candidate, target))
            return true;

        if (candidate.Kind != target.Kind)
            return false;

        if (candidate.SyntaxTree is not null &&
            target.SyntaxTree is not null &&
            ReferenceEquals(candidate.SyntaxTree, target.SyntaxTree) &&
            candidate.Span == target.Span)
        {
            return true;
        }

        return candidate.Position == target.Position &&
               candidate.FullSpan == target.FullSpan &&
               string.Equals(candidate.ToFullString(), target.ToFullString(), StringComparison.Ordinal);
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

    private static BaseTypeDeclarationSyntax? RewriteContainingTypeMembers(
        BaseTypeDeclarationSyntax containingType,
        SyntaxList<MemberDeclarationSyntax> members)
    {
        return containingType switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.WithMembers(members),
            StructDeclarationSyntax structDeclaration => structDeclaration.WithMembers(members),
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.WithMembers(members),
            InterfaceDeclarationSyntax interfaceDeclaration => interfaceDeclaration.WithMembers(members),
            UnionDeclarationSyntax unionDeclaration => unionDeclaration.WithMembers(members),
            _ => null
        };
    }

    private static SyntaxList<MemberDeclarationSyntax> GetContainingTypeMembers(
        BaseTypeDeclarationSyntax containingType)
        => containingType switch
        {
            TypeDeclarationSyntax typeDeclaration => typeDeclaration.Members,
            UnionDeclarationSyntax unionDeclaration => unionDeclaration.Members,
            _ => default
        };

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

    private static FreestandingMacroExpansionResult ContextualizeExpansionResult(
        FreestandingMacroInvocation invocation,
        FreestandingMacroExpansionResult result)
    {
        if (result.HasMemberExpansion)
        {
            var memberParent = GetMemberExpansionParent(invocation) ?? invocation.Syntax.Parent;
            var memberPosition = GetMemberExpansionPosition(invocation);
            var members = result.Members
                .Select(member => (MemberDeclarationSyntax)MacroSyntaxOrigin
                    .MarkGeneratedSyntaxHidden(member, invocation.Syntax)
                    .WithParent(memberParent, memberPosition))
                .ToImmutableArray();
            return CopyExpansionMetadata(result, new FreestandingMacroExpansionResult
            {
                Members = members
            });
        }

        if (result.Node is null)
            return result;

        var expansionNode = MacroSyntaxOrigin.MarkGeneratedSyntaxHidden(
            result.Node,
            invocation.Syntax);
        var isStatementPosition = invocation.Syntax is FreestandingMacroExpressionSyntax expression &&
            IsStatementPosition(expression);
        var isMemberResult = result.Node is MemberDeclarationSyntax;
        var parent = isMemberResult
            ? GetMemberExpansionParent(invocation) ?? invocation.Syntax.Parent
            : isStatementPosition
                ? invocation.Syntax.Parent?.Parent
                : invocation.Syntax.Parent;
        var position = isMemberResult
            ? GetMemberExpansionPosition(invocation)
            : isStatementPosition
                ? invocation.Syntax.Parent?.Position ?? invocation.Syntax.Position
                : invocation.Syntax.Position;
        var contextualNode = expansionNode.WithParent(parent, position);
        return CopyExpansionMetadata(result, new FreestandingMacroExpansionResult
        {
            Node = contextualNode
        });
    }

    private static FreestandingMacroExpansionResult ValidateExpansionCategory(
        string macroName,
        FreestandingMacroInvocation invocation,
        FreestandingMacroExpansionResult result,
        DiagnosticBag diagnostics)
    {
        if (invocation.Syntax is FreestandingMacroMemberDeclarationSyntax)
        {
            if (result.HasMemberExpansion || result.Node is null or MemberDeclarationSyntax)
                return result;

            diagnostics.Report(Diagnostic.Create(
                s_macroExpansionCategoryMismatch,
                invocation.Name.GetLocation(),
                macroName,
                DescribeExpansionCategory(result.Node),
                "member"));

            return WithoutExpansion(result);
        }

        var macroExpression = (FreestandingMacroExpressionSyntax)invocation.Syntax;
        if (result.HasMemberExpansion)
        {
            if (IsNamespaceMemberPosition(macroExpression))
                return result;

            diagnostics.Report(Diagnostic.Create(
                s_macroExpansionCategoryMismatch,
                macroExpression.Name.GetLocation(),
                macroName,
                "member-list",
                IsStatementPosition(macroExpression) ? "statement" : "expression"));

            return WithoutExpansion(result);
        }

        if (result.Node is null)
            return result;

        if (IsNamespaceMemberPosition(macroExpression) && result.Node is MemberDeclarationSyntax)
            return result;

        var requiresStatement = IsStatementPosition(macroExpression);
        var valid = requiresStatement
            ? result.Node is StatementSyntax
            : result.Node is ExpressionSyntax;
        if (valid)
            return result;

        diagnostics.Report(Diagnostic.Create(
            s_macroExpansionCategoryMismatch,
            macroExpression.Name.GetLocation(),
            macroName,
            result.Node is StatementSyntax ? "statement" : "non-expression",
            requiresStatement ? "statement" : "expression"));

        return WithoutExpansion(result);
    }

    private static string DescribeExpansionCategory(SyntaxNode node)
        => node switch
        {
            ExpressionSyntax => "expression",
            StatementSyntax => "statement",
            MemberDeclarationSyntax => "member",
            _ => "incompatible"
        };

    private static FreestandingMacroExpansionResult CopyExpansionMetadata(
        FreestandingMacroExpansionResult source,
        FreestandingMacroExpansionResult destination)
    {
        destination.MacroDiagnostics = source.MacroDiagnostics;
        destination.Diagnostics = source.Diagnostics;
        destination.FragmentRegions = source.FragmentRegions;
        destination.TokenInfos = source.TokenInfos;
        destination.FileDependencies = source.FileDependencies;
        return destination;
    }

    private static FreestandingMacroExpansionResult WithoutExpansion(
        FreestandingMacroExpansionResult result)
        => new()
        {
            Diagnostics = result.Diagnostics,
            MacroDiagnostics = result.MacroDiagnostics,
            FragmentRegions = result.FragmentRegions,
            TokenInfos = result.TokenInfos,
            FileDependencies = result.FileDependencies
        };

    private static bool IsStatementPosition(FreestandingMacroExpressionSyntax expression)
        => expression.TokenTree is not null &&
           expression.Parent is ExpressionStatementSyntax statement &&
           ReferenceEquals(statement.Expression, expression);

    private static bool IsNamespaceMemberPosition(FreestandingMacroExpressionSyntax expression)
        => GetNamespaceMemberCarrier(expression) is not null;

    private static GlobalStatementSyntax? GetNamespaceMemberCarrier(
        FreestandingMacroExpressionSyntax expression)
        => expression.Parent is ExpressionStatementSyntax statement &&
           ReferenceEquals(statement.Expression, expression) &&
           statement.Parent is GlobalStatementSyntax globalStatement &&
           globalStatement.Parent is CompilationUnitSyntax or BaseNamespaceDeclarationSyntax
            ? globalStatement
            : null;

    private static SyntaxNode? GetMemberExpansionParent(FreestandingMacroInvocation invocation)
        => invocation.Syntax switch
        {
            FreestandingMacroMemberDeclarationSyntax member => member.Parent,
            FreestandingMacroExpressionSyntax expression => GetNamespaceMemberCarrier(expression)?.Parent,
            _ => null
        };

    private static int GetMemberExpansionPosition(FreestandingMacroInvocation invocation)
        => invocation.Syntax is FreestandingMacroExpressionSyntax expression &&
           GetNamespaceMemberCarrier(expression) is { } globalStatement
            ? globalStatement.Position
            : invocation.Syntax.Position;

    private static void ReportMacroDiagnostics(
        DiagnosticBag diagnostics,
        string macroName,
        Location fallbackLocation,
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
    {
        foreach (var macroDiagnostic in macroDiagnostics)
        {
            var location = macroDiagnostic.Location ?? fallbackLocation;
            var message = macroDiagnostic.Code is { Length: > 0 }
                ? $"{macroDiagnostic.Code}: {macroDiagnostic.Message}"
                : macroDiagnostic.Message;

            diagnostics.Report(Diagnostic.Create(
                s_macroReportedDiagnostic,
                location,
                macroDiagnostic.Severity,
                macroName,
                message));
        }
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

    private static void RegisterGeneratedSyntaxTree(
        Compilation compilation,
        SemanticModel semanticModel,
        SyntaxNode? node)
    {
        RegisterSyntaxTree(compilation, semanticModel, node);
    }

    private static IEnumerable<AttributeSyntax> GetAttachedMacroAttributes(SyntaxNode targetDeclaration)
        => targetDeclaration.ChildNodes()
            .OfType<AttributeListSyntax>()
            .SelectMany(static list => list.Attributes)
            .Where(static attribute => attribute.IsMacroAttribute());
}
