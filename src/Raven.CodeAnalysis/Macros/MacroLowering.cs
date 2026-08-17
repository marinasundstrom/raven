using System.Text;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroLowering
{
    public static SyntaxTree Lower(
        SyntaxTree syntaxTree,
        SemanticModel semanticModel)
    {
        var declarations = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .OrderByDescending(static declaration => declaration.Span.Start)
            .ToArray();
        if (declarations.Length == 0)
            return syntaxTree;

        var source = syntaxTree.GetText()!.ToString();
        var lowered = new StringBuilder(source);
        foreach (var declaration in declarations)
        {
            lowered.Remove(declaration.Span.Start, declaration.Span.Length);
            lowered.Insert(
                declaration.Span.Start,
                LowerDeclaration(source, declaration, semanticModel));
        }

        return SyntaxTree.ParseText(
            SourceText.From(lowered.ToString(), syntaxTree.Encoding),
            syntaxTree.Options,
            syntaxTree.FilePath);
    }

    private static string LowerDeclaration(
        string source,
        MacroDeclarationSyntax declaration,
        SemanticModel semanticModel)
    {
        var suffix = declaration.Span.Start.ToString(System.Globalization.CultureInfo.InvariantCulture);
        var providerName = $"__RavenMacro_{declaration.Identifier.ValueText}_{suffix}";
        var symbol = semanticModel.GetDeclaredSymbol(declaration) as IMacroDeclarationSymbol;
        var isAttached = symbol?.ApplicationKind == MacroApplicationKind.Attached ||
            declaration.ParameterList.Parameters.Any(static parameter =>
                parameter.OnKeyword.Kind != SyntaxKind.None);
        var isPublic = symbol?.DeclaredAccessibility == Accessibility.Public;
        var parameters = declaration.ParameterList.Parameters
            .Select((syntax, index) => (
                Syntax: syntax,
                Role: symbol?.Parameters[index].MacroRole ?? MacroParameterRole.Value,
                Parameter: symbol?.Parameters[index],
                InvocationOrdinal: symbol?.ParameterBindings[index].InvocationArgumentOrdinal,
                ContextKind: symbol is null
                    ? MacroContextKind.None
                    : MacroParameterRoleFacts.GetContextKind(symbol.Parameters[index].Type)))
            .ToArray();
        var tokenStreamParameters = parameters
            .Where(static parameter =>
                parameter.Role == MacroParameterRole.TokenBody)
            .ToArray();
        var contextParameters = parameters
            .Where(static parameter =>
                parameter.ContextKind == MacroContextKind.TokenTree)
            .ToArray();
        var invocableContextParameters = parameters
            .Where(static parameter =>
                parameter.ContextKind == MacroContextKind.Invocable)
            .ToArray();
        var attachedContextParameters = parameters
            .Where(static parameter =>
                parameter.ContextKind == MacroContextKind.Attached)
            .ToArray();
        var valueParameters = parameters
            .Where(static parameter =>
                parameter.Role is not (
                    MacroParameterRole.TokenBody or
                    MacroParameterRole.Context or
                    MacroParameterRole.AttachedTarget))
            .ToArray();
        var hasTokenTreeBody = tokenStreamParameters.Length > 0 || contextParameters.Length > 0;
        var hasEditorMetadataContributions = declaration.DescendantNodes()
            .OfType<MacroExpansionStatementSyntax>()
            .Any(static statement => statement.Keyword.ValueText is "fragment" or "token");
        var hasParameters = valueParameters.Length > 0;
        var usedNames = declaration.DescendantTokens()
            .Where(static token => token.Kind == SyntaxKind.IdentifierToken)
            .Select(static token => token.ValueText)
            .ToHashSet(StringComparer.Ordinal);
        var contextVariableName = AllocateGeneratedName(usedNames, "__macroContext");
        var executionVariableName = AllocateGeneratedName(usedNames, "__macroExecution");
        var resultBuilderName = AllocateGeneratedName(usedNames, "__macroResultBuilder");
        var interfaceName = "Raven.CodeAnalysis.Macros.IMacroExecutor";
        var contextName = hasTokenTreeBody
            ? "Raven.CodeAnalysis.Macros.TokenTreeMacroContext"
            : isAttached
            ? "Raven.CodeAnalysis.Macros.AttachedMacroContext"
            : "Raven.CodeAnalysis.Macros.InvocableMacroContext";
        var categoryResultName = hasTokenTreeBody
            ? "Raven.CodeAnalysis.Macros.InvocableMacroExpansionResult"
            : isAttached
            ? "Raven.CodeAnalysis.Macros.MacroExpansionResult"
            : "Raven.CodeAnalysis.Macros.InvocableMacroExpansionResult";
        var buildMethod = isAttached && !hasTokenTreeBody ? "BuildAttached" : "BuildInvocable";
        var resultFactory = isAttached && !hasTokenTreeBody ? "Attached" : "Invocable";
        if (hasEditorMetadataContributions)
            interfaceName += ", Raven.CodeAnalysis.Macros.IMacroExpansionMetadataProvider";

        var builder = new StringBuilder();
        builder.AppendLine($"{(isPublic ? "public " : string.Empty)}class {providerName} : {interfaceName} {{");
        builder.AppendLine(
            $"    val Namespace: string => \"{EscapeString(GetDeclaredNamespace(declaration))}\"");
        builder.AppendLine(
            $"    val Name: string => \"{EscapeString(declaration.Identifier.ValueText)}\"");
        if (symbol?.GetDocumentationComment() is { } documentation &&
            !string.IsNullOrWhiteSpace(documentation.Content))
        {
            builder.AppendLine(
                $"    val Documentation: string? => \"{EscapeString(documentation.Content)}\"");
            builder.AppendLine(
                $"    val DocumentationFormat: Raven.CodeAnalysis.DocumentationFormat => Raven.CodeAnalysis.DocumentationFormat.{documentation.Format}");
        }
        if (GetMacroAlias(declaration) is { } alias)
        {
            builder.AppendLine(
                $"    val Alias: string? => \"{EscapeString(alias)}\"");
        }
        if (!isAttached)
        {
            builder.AppendLine(
                $"    val InvocationTargets: Raven.CodeAnalysis.Macros.MacroInvocationTargets => {GetInvocationTargetsExpression(symbol?.InvocationTargets ?? MacroInvocationTargets.Expression)}");
        }
        if (isAttached)
        {
            builder.AppendLine(
                $"    val Targets: Raven.CodeAnalysis.Macros.MacroTarget => Raven.CodeAnalysis.Macros.MacroTarget.{symbol?.Targets ?? MacroTarget.None}");
        }

        builder.AppendLine(
            $"    val ApplicationKind: Raven.CodeAnalysis.Macros.MacroApplicationKind => Raven.CodeAnalysis.Macros.MacroApplicationKind.{(isAttached ? "Attached" : "Invocable")}");
        if (hasTokenTreeBody)
            builder.AppendLine("    val HasTokenBody: bool => true");
        if (hasParameters)
            builder.AppendLine("    val AcceptsArguments: bool => true");

        builder.AppendLine(
            $"    func Expand({executionVariableName}: Raven.CodeAnalysis.Macros.MacroExecutionContext) -> Raven.CodeAnalysis.Macros.MacroExecutionResult {{");
        builder.AppendLine(
            $"        let {contextVariableName} = {executionVariableName}.GetContext<{contextName}>()");
        builder.AppendLine(
            $"        let {resultBuilderName} = Raven.CodeAnalysis.Macros.MacroExpansionResultBuilder()");

        foreach (var parameter in valueParameters)
        {
            var parameterType = GetParameterType(parameter);
            var argumentAccessor = parameter.Syntax.DefaultValue is null
                ? "GetArgument"
                : "GetArgumentOrDefault";
            var defaultArgument = parameter.Syntax.DefaultValue is null
                ? string.Empty
                : $", {parameter.Syntax.DefaultValue.Value}";
            builder.AppendLine(
                $"        let {parameter.Syntax.Identifier.ValueText}: {parameterType} = {executionVariableName}.{argumentAccessor}<{parameterType}>({parameter.InvocationOrdinal ?? 0}, \"{EscapeString(parameter.Syntax.Identifier.ValueText)}\"{defaultArgument})");
        }

        if (tokenStreamParameters.Length > 0)
        {
            var tokenStreamParameter = tokenStreamParameters[0];
            builder.AppendLine(
                $"        let {tokenStreamParameter.Syntax.Identifier.ValueText}: Raven.CodeAnalysis.Macros.IMacroTokenStream = {contextVariableName}.CreateTokenStream()");
        }
        foreach (var contextParameter in contextParameters)
        {
            builder.AppendLine(
                $"        let {contextParameter.Syntax.Identifier.ValueText}: Raven.CodeAnalysis.Macros.TokenTreeMacroContext = {contextVariableName}");
        }
        foreach (var contextParameter in invocableContextParameters)
        {
            builder.AppendLine(
                $"        let {contextParameter.Syntax.Identifier.ValueText}: Raven.CodeAnalysis.Macros.InvocableMacroContext = {contextVariableName}");
        }
        foreach (var contextParameter in attachedContextParameters)
        {
            builder.AppendLine(
                $"        let {contextParameter.Syntax.Identifier.ValueText}: Raven.CodeAnalysis.Macros.AttachedMacroContext = {contextVariableName}");
        }

        if (!hasTokenTreeBody && parameters.FirstOrDefault(static parameter =>
                parameter.Role == MacroParameterRole.AttachedTarget) is { Syntax: { } targetParameter })
        {
            AppendTargetBinding(
                builder,
                targetParameter,
                categoryResultName,
                resultFactory,
                contextVariableName);
        }

        AppendLoweredBody(
            builder,
            source,
            declaration,
            resultBuilderName,
            buildMethod,
            resultFactory);
        if (!EndsWithExpand(declaration))
            builder.AppendLine(
                $"        return Raven.CodeAnalysis.Macros.MacroExecutionResult.{resultFactory}({resultBuilderName}.{buildMethod}())");
        builder.AppendLine("    }");
        builder.AppendLine("}");
        return builder.ToString();
    }

    private static string GetParameterType(
        (
            ParameterSyntax Syntax,
            MacroParameterRole Role,
            IParameterSymbol? Parameter,
            int? InvocationOrdinal,
            MacroContextKind ContextKind) parameter)
        => parameter.Parameter?.Type.TypeKind == TypeKind.TypeParameter
            ? "object"
            : MacroParameterRoleFacts.GetLoweredTypeName(
                parameter.Syntax,
                parameter.Role);

    private static void AppendTargetBinding(
        StringBuilder builder,
        ParameterSyntax targetParameter,
        string resultName,
        string resultFactory,
        string contextVariableName)
    {
        var targetName = targetParameter.Identifier.ValueText;
        var syntaxType = targetParameter.TypeAnnotation?.Type.ToString() ??
            "Raven.CodeAnalysis.Syntax.SyntaxNode";

        builder.AppendLine(
            $"        let {targetName}: {syntaxType} = {contextVariableName}.CurrentDeclaration else {{");
        builder.AppendLine(
            $"            return Raven.CodeAnalysis.Macros.MacroExecutionResult.{resultFactory}({resultName}.Empty)");
        builder.AppendLine("        }");
    }

    private static void AppendLoweredBody(
        StringBuilder builder,
        string source,
        MacroDeclarationSyntax declaration,
        string resultBuilderName,
        string buildMethod,
        string resultFactory)
    {
        if (declaration.Body is { } body)
        {
            var contentSpan = TextSpan.FromBounds(
                body.OpenBraceToken.Span.End,
                body.CloseBraceToken.Span.Start);
            var content = new StringBuilder(
                source.Substring(contentSpan.Start, contentSpan.Length));
            foreach (var contribution in GetContributions(body)
                .OrderByDescending(static contribution => contribution.Node.Span.Start))
            {
                var relativeStart = contribution.Node.Span.Start - contentSpan.Start;
                var expression = source.Substring(
                    contribution.Expression.Span.Start,
                    contribution.Expression.Span.Length);
                var lineStart = source.LastIndexOf(
                    '\n',
                    Math.Max(0, contribution.Node.Span.Start - 1));
                var indentationStart = lineStart < 0 ? 0 : lineStart + 1;
                var linePrefix = source.Substring(
                    indentationStart,
                    contribution.Node.Span.Start - indentationStart);
                var indentation = linePrefix.All(char.IsWhiteSpace)
                    ? linePrefix
                    : string.Empty;
                var instruction = contribution.Keyword.ValueText;
                var method = GetContributionMethod(instruction);
                content.Remove(relativeStart, contribution.Node.Span.Length);
                var loweredContribution = $"{resultBuilderName}.{method}({expression})";
                if (instruction == "expand")
                {
                    loweredContribution = contribution.IsExpression
                        ? $"{{ {loweredContribution}; return Raven.CodeAnalysis.Macros.MacroExecutionResult.{resultFactory}({resultBuilderName}.{buildMethod}()) }}"
                        : $"{loweredContribution}\n{indentation}return Raven.CodeAnalysis.Macros.MacroExecutionResult.{resultFactory}({resultBuilderName}.{buildMethod}())";
                }

                content.Insert(relativeStart, loweredContribution);
            }

            foreach (var line in content.ToString().Split('\n'))
                builder.AppendLine($"        {line}");
        }
        else if (declaration.ExpressionBody is { } expressionBody)
        {
            if (expressionBody.Expression is MacroExpansionExpressionSyntax contribution)
            {
                builder.AppendLine(
                    $"        {resultBuilderName}.{GetContributionMethod(contribution.Keyword.ValueText)}({contribution.Expression})");
            }
            else
            {
                builder.AppendLine(
                    $"        {resultBuilderName}.Expand({expressionBody.Expression})");
            }
        }
    }

    private static string GetContributionMethod(string instruction)
        => instruction switch
        {
            "expand" => "Expand",
            "replace" => "Replace",
            "introduce" => "Introduce",
            "fragment" => "Fragment",
            "token" => "Token",
            _ => throw new InvalidOperationException()
        };

    private static IEnumerable<MacroContribution> GetContributions(SyntaxNode node)
    {
        foreach (var descendant in node.DescendantNodes())
        {
            switch (descendant)
            {
                case MacroExpansionStatementSyntax statement:
                    yield return new MacroContribution(
                        statement,
                        statement.Keyword,
                        statement.Expression,
                        IsExpression: false);
                    break;
                case MacroExpansionExpressionSyntax expression:
                    yield return new MacroContribution(
                        expression,
                        expression.Keyword,
                        expression.Expression,
                        IsExpression: true);
                    break;
            }
        }
    }

    private readonly record struct MacroContribution(
        SyntaxNode Node,
        SyntaxToken Keyword,
        ExpressionSyntax Expression,
        bool IsExpression);

    private static string AllocateGeneratedName(
        ISet<string> usedNames,
        string baseName)
    {
        var candidate = baseName;
        var suffix = 0;
        while (!usedNames.Add(candidate))
            candidate = $"{baseName}{++suffix}";

        return candidate;
    }

    private static string GetInvocationTargetsExpression(MacroInvocationTargets targets)
    {
        const string prefix = "Raven.CodeAnalysis.Macros.MacroInvocationTargets.";
        if (targets == MacroInvocationTargets.None)
            return prefix + nameof(MacroInvocationTargets.None);

        return string.Join(
            " | ",
            Enum.GetValues<MacroInvocationTargets>()
                .Where(static target =>
                    target != MacroInvocationTargets.None &&
                    ((int)target & ((int)target - 1)) == 0)
                .Where(target => targets.HasFlag(target))
                .Select(target => prefix + target));
    }

    private static bool EndsWithExpand(MacroDeclarationSyntax declaration)
        => declaration.Body?.Statements.LastOrDefault() is MacroExpansionStatementSyntax statement &&
           statement.Keyword.ValueText == "expand";

    internal static string? GetMacroAlias(MacroDeclarationSyntax declaration)
    {
        foreach (var attribute in declaration.AttributeLists.SelectMany(static list => list.Attributes))
        {
            var attributeName = attribute.Name switch
            {
                IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
                QualifiedNameSyntax { Right: IdentifierNameSyntax identifier } =>
                    identifier.Identifier.ValueText,
                _ => string.Empty
            };
            if (attributeName is not ("MacroAlias" or "MacroAliasAttribute") ||
                attribute.ArgumentList?.Arguments.Count != 1 ||
                attribute.ArgumentList.Arguments[0].Expression is not LiteralExpressionSyntax literal ||
                literal.Token.Kind != SyntaxKind.StringLiteralToken ||
                string.IsNullOrWhiteSpace(literal.Token.ValueText))
            {
                continue;
            }

            return literal.Token.ValueText;
        }

        return null;
    }

    private static string GetDeclaredNamespace(MacroDeclarationSyntax declaration)
        => string.Join(
            ".",
            declaration.Ancestors()
                .OfType<BaseNamespaceDeclarationSyntax>()
                .Reverse()
                .Select(static namespaceDeclaration => namespaceDeclaration.Name.ToString()));

    private static string EscapeString(string value)
        => value.Replace("\\", "\\\\", StringComparison.Ordinal)
            .Replace("\"", "\\\"", StringComparison.Ordinal)
            .Replace("$", "\\$", StringComparison.Ordinal)
            .Replace("\r", "\\r", StringComparison.Ordinal)
            .Replace("\n", "\\n", StringComparison.Ordinal)
            .Replace("\t", "\\t", StringComparison.Ordinal);
}
