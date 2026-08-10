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
                var result = ExpandWithTypedParametersIfAvailable(loaded.Macro, context, diagnostics)
                    ?? loaded.Macro.Expand(context)
                    ?? MacroExpansionResult.Empty;
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

    public static InvocableMacroExpansionResult? ExpandInvocableMacro(
        Compilation compilation,
        SemanticModel semanticModel,
        InvocableMacroExpressionSyntax expression,
        DiagnosticBag diagnostics,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        compilation.PerformanceInstrumentation.Macros.RecordInvocableExpansionInvocation();

        if (!MacroSemanticValidator.TryResolveInvocableMacro(compilation, expression, diagnostics, out var loaded))
            return null;

        try
        {
            InvocableMacroExpansionResult result;
            if (expression.TokenTree is not null)
            {
                var tokenTreeMacro = (ITokenTreeMacro)loaded.Macro;
                var context = new TokenTreeMacroContext(
                    compilation,
                    semanticModel,
                    expression,
                    tokenTreeMacro,
                    cancellationToken);
                result = ExpandWithTypedParametersIfAvailable(tokenTreeMacro, context, diagnostics)
                    ?? tokenTreeMacro.Expand(context)
                    ?? InvocableMacroExpansionResult.Empty;
                result = AddReportedDiagnostics(result, context);
                result.FileDependencies = MergeFileDependencies(
                    result.FileDependencies,
                    context.GetFileDependencies());
            }
            else
            {
                var invocableMacro = (IInvocableMacro)loaded.Macro;
                var context = new InvocableMacroContext(
                    compilation,
                    semanticModel,
                    expression,
                    cancellationToken);
                result = ExpandWithTypedParametersIfAvailable(invocableMacro, context, diagnostics)
                    ?? invocableMacro.Expand(context)
                    ?? InvocableMacroExpansionResult.Empty;
                result = AddReportedDiagnostics(result, context);
                result.FileDependencies = MergeFileDependencies(
                    result.FileDependencies,
                    context.GetFileDependencies());
            }

            result = ValidateExpansionCategory(loaded.Macro.Name, expression, result, diagnostics);
            result = ContextualizeExpansionResult(expression, result);
            RegisterGeneratedSyntaxTree(compilation, semanticModel, result.Node);

            ReportMacroDiagnostics(diagnostics, loaded.Macro.Name, expression.Name.GetLocation(), result.MacroDiagnostics);

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
                expression.Name.GetLocation(),
                loaded.Macro.Name,
                GetExpansionFailureMessage(failure)));
            return null;
        }
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

    private static MacroExpansionResult? ExpandWithTypedParametersIfAvailable(
        IAttachedDeclarationMacro macro,
        AttachedMacroContext context,
        DiagnosticBag diagnostics)
    {
        var typedMacroInterface = macro.GetType()
            .GetInterfaces()
            .FirstOrDefault(static i =>
                i.IsGenericType &&
                i.GetGenericTypeDefinition() == typeof(IAttachedDeclarationMacro<>));

        if (typedMacroInterface is null)
            return null;

        var parametersType = typedMacroInterface.GetGenericArguments()[0];
        if (!MacroParameterBinder.TryBind(macro.Name, parametersType, context, diagnostics, out var parameters))
            return MacroExpansionResult.Empty;

        var typedContextType = typeof(AttachedMacroContext<>).MakeGenericType(parametersType);
        var typedContext = Activator.CreateInstance(
            typedContextType,
            context.Compilation,
            context.SemanticModel,
            context.Syntax,
            context.TargetDeclaration,
            context.CurrentDeclaration,
            parameters!,
            context.CancellationToken);

        var expandMethod = typedMacroInterface.GetMethod(
            nameof(IAttachedDeclarationMacro.Expand),
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            [typedContextType],
            modifiers: null);

        var result = (MacroExpansionResult?)expandMethod?.Invoke(macro, [typedContext!]);
        context.AddReportedDiagnostics((AttachedMacroContext)typedContext!);
        return result;
    }

    private static InvocableMacroExpansionResult? ExpandWithTypedParametersIfAvailable(
        IInvocableMacro macro,
        InvocableMacroContext context,
        DiagnosticBag diagnostics)
    {
        var typedMacroInterface = macro.GetType()
            .GetInterfaces()
            .FirstOrDefault(static i =>
                i.IsGenericType &&
                i.GetGenericTypeDefinition() == typeof(IInvocableMacro<>));

        if (typedMacroInterface is null)
            return null;

        var parametersType = typedMacroInterface.GetGenericArguments()[0];
        if (!MacroParameterBinder.TryBind(macro.Name, parametersType, context, diagnostics, out var parameters))
            return InvocableMacroExpansionResult.Empty;

        var typedContextType = typeof(InvocableMacroContext<>).MakeGenericType(parametersType);
        var typedContext = Activator.CreateInstance(
            typedContextType,
            context.Compilation,
            context.SemanticModel,
            context.Syntax,
            parameters!,
            context.CancellationToken);

        var expandMethod = typedMacroInterface.GetMethod(
            nameof(IInvocableMacro.Expand),
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            [typedContextType],
            modifiers: null);

        var result = (InvocableMacroExpansionResult?)expandMethod?.Invoke(macro, [typedContext!]);
        context.AddReportedDiagnostics((InvocableMacroContext)typedContext!);
        context.AddFileDependencies(
            ((InvocableMacroContext)typedContext!).GetFileDependencies());
        return result;
    }

    private static InvocableMacroExpansionResult? ExpandWithTypedParametersIfAvailable(
        ITokenTreeMacro macro,
        TokenTreeMacroContext context,
        DiagnosticBag diagnostics)
    {
        var typedMacroInterface = macro.GetType()
            .GetInterfaces()
            .FirstOrDefault(static i =>
                i.IsGenericType &&
                i.GetGenericTypeDefinition() == typeof(ITokenTreeMacro<>));

        if (typedMacroInterface is null)
            return null;

        var parametersType = typedMacroInterface.GetGenericArguments()[0];
        if (!MacroParameterBinder.TryBind(macro.Name, parametersType, context, diagnostics, out var parameters))
            return InvocableMacroExpansionResult.Empty;

        var typedContextType = typeof(TokenTreeMacroContext<>).MakeGenericType(parametersType);
        var typedContext = Activator.CreateInstance(
            typedContextType,
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            [
                context.Compilation,
                context.SemanticModel,
                context.Syntax,
                macro,
                parameters!,
                context.CancellationToken
            ],
            culture: null);

        var expandMethod = typedMacroInterface.GetMethod(
            nameof(ITokenTreeMacro.Expand),
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            [typedContextType],
            modifiers: null);

        var result = (InvocableMacroExpansionResult?)expandMethod?.Invoke(macro, [typedContext!]);
        context.AddReportedDiagnostics((TokenTreeMacroContext)typedContext!);
        context.AddFileDependencies(
            ((TokenTreeMacroContext)typedContext!).GetFileDependencies());
        return result;
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

    private static InvocableMacroExpansionResult AddReportedDiagnostics(
        InvocableMacroExpansionResult result,
        MacroContext context)
    {
        var diagnostics = context.GetReportedDiagnostics();
        var macroDiagnostics = context.GetReportedMacroDiagnostics();
        if (diagnostics.IsDefaultOrEmpty && macroDiagnostics.IsDefaultOrEmpty)
            return result;

        if (ReferenceEquals(result, InvocableMacroExpansionResult.Empty))
            result = new InvocableMacroExpansionResult();

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

        var contextualContainingType = (TypeDeclarationSyntax)rewrittenContainingType.WithParent(containingType.Parent, containingType.Position);
        var contextualMembers = contextualContainingType.Members;

        return new MacroExpansionResult
        {
            ReplacementDeclaration = contextualMembers[replacementIndex],
            IntroducedMembers = SliceMembers(contextualMembers, introducedStartIndex, result.IntroducedMembers.Length),
            PeerDeclarations = SliceMembers(contextualMembers, peerStartIndex, result.PeerDeclarations.Length),
            MacroDiagnostics = result.MacroDiagnostics,
            Diagnostics = result.Diagnostics
        };
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

    private static InvocableMacroExpansionResult ContextualizeExpansionResult(
        InvocableMacroExpressionSyntax macroExpression,
        InvocableMacroExpansionResult result)
    {
        if (result.Node is null)
            return result;

        var expansionNode = MacroSyntaxOrigin.MarkGeneratedSyntaxHidden(
            result.Node,
            macroExpression);
        var isStatementPosition = IsStatementPosition(macroExpression);
        var parent = isStatementPosition ? macroExpression.Parent?.Parent : macroExpression.Parent;
        var position = isStatementPosition ? macroExpression.Parent?.Position ?? macroExpression.Position : macroExpression.Position;
        var contextualNode = expansionNode.WithParent(parent, position);
        return new InvocableMacroExpansionResult
        {
            Node = contextualNode,
            MacroDiagnostics = result.MacroDiagnostics,
            Diagnostics = result.Diagnostics,
            FragmentRegions = result.FragmentRegions,
            TokenInfos = result.TokenInfos,
            FileDependencies = result.FileDependencies
        };
    }

    private static InvocableMacroExpansionResult ValidateExpansionCategory(
        string macroName,
        InvocableMacroExpressionSyntax macroExpression,
        InvocableMacroExpansionResult result,
        DiagnosticBag diagnostics)
    {
        if (result.HasMemberExpansion)
        {
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

    private static InvocableMacroExpansionResult WithoutExpansion(
        InvocableMacroExpansionResult result)
        => new()
        {
            Diagnostics = result.Diagnostics,
            MacroDiagnostics = result.MacroDiagnostics,
            FragmentRegions = result.FragmentRegions,
            TokenInfos = result.TokenInfos,
            FileDependencies = result.FileDependencies
        };

    private static bool IsStatementPosition(InvocableMacroExpressionSyntax expression)
        => expression.TokenTree is not null &&
           expression.Parent is ExpressionStatementSyntax statement &&
           ReferenceEquals(statement.Expression, expression);

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
