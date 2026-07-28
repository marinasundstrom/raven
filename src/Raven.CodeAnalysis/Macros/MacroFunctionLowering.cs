using System.Text;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroFunctionLowering
{
    public static SyntaxTree Lower(
        SyntaxTree syntaxTree,
        SemanticModel semanticModel)
    {
        var declarations = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
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
        MacroFunctionDeclarationSyntax declaration,
        SemanticModel semanticModel)
    {
        var suffix = declaration.Span.Start.ToString(System.Globalization.CultureInfo.InvariantCulture);
        var providerName = $"__RavenMacroFunction_{declaration.Identifier.ValueText}_{suffix}";
        var parametersName = $"{providerName}_Parameters";
        var isAttached = declaration.TargetClause is not null;
        var symbol = semanticModel.GetDeclaredSymbol(declaration) as IMacroFunctionSymbol;
        var parameters = declaration.ParameterList.Parameters
            .Select((syntax, index) => (
                Syntax: syntax,
                Role: symbol?.Parameters[index].MacroRole ?? MacroParameterRole.Value))
            .ToArray();
        var tokenStreamParameters = parameters
            .Where(static parameter =>
                parameter.Role == MacroParameterRole.TokenStream)
            .ToArray();
        var valueParameters = parameters
            .Where(static parameter =>
                parameter.Role != MacroParameterRole.TokenStream)
            .ToArray();
        var hasTokenTreeBody = tokenStreamParameters.Length > 0;
        var hasParameters = valueParameters.Length > 0;
        var interfaceName = hasTokenTreeBody
            ? "Raven.CodeAnalysis.Macros.ITokenTreeExpressionMacro"
            : isAttached
            ? "Raven.CodeAnalysis.Macros.IAttachedDeclarationMacro"
            : "Raven.CodeAnalysis.Macros.IFreestandingExpressionMacro";
        var contextName = hasTokenTreeBody
            ? "Raven.CodeAnalysis.Macros.TokenTreeMacroContext"
            : isAttached
            ? "Raven.CodeAnalysis.Macros.AttachedMacroContext"
            : "Raven.CodeAnalysis.Macros.FreestandingMacroContext";
        var resultName = hasTokenTreeBody
            ? "Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult"
            : isAttached
            ? "Raven.CodeAnalysis.Macros.MacroExpansionResult"
            : "Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult";
        var buildMethod = isAttached && !hasTokenTreeBody ? "BuildAttached" : "BuildFreestanding";

        if (hasParameters)
        {
            interfaceName += $"<{parametersName}>";
            contextName += $"<{parametersName}>";
        }

        var builder = new StringBuilder();
        if (hasParameters)
            AppendParametersClass(builder, valueParameters, parametersName);

        builder.AppendLine($"class {providerName} : {interfaceName} {{");
        builder.AppendLine(
            $"    val Name: string => \"{EscapeString(declaration.Identifier.ValueText)}\"");
        if (isAttached)
        {
            builder.AppendLine(
                $"    val Targets: Raven.CodeAnalysis.Macros.MacroTarget => Raven.CodeAnalysis.Macros.MacroTarget.{declaration.TargetClause!.Target}");
        }

        builder.AppendLine(
            $"    func Expand(context: {contextName}) -> {resultName} {{");
        builder.AppendLine(
            "        let __macroResult = Raven.CodeAnalysis.Macros.MacroExpansionResultBuilder()");

        foreach (var parameter in valueParameters)
        {
            builder.AppendLine(
                $"        let {parameter.Syntax.Identifier.ValueText} = context.Parameters.{parameter.Syntax.Identifier.ValueText}");
        }

        if (tokenStreamParameters.Length > 0)
        {
            var tokenStreamParameter = tokenStreamParameters[0];
            builder.AppendLine(
                $"        let {tokenStreamParameter.Syntax.Identifier.ValueText}: Raven.CodeAnalysis.Macros.IMacroTokenStream = context.CreateTokenStream()");
        }
        else if (declaration.TargetClause is { } targetClause)
            AppendTargetBinding(builder, targetClause, resultName);

        AppendLoweredBody(builder, source, declaration);
        builder.AppendLine($"        return __macroResult.{buildMethod}()");
        builder.AppendLine("    }");
        builder.AppendLine("}");
        return builder.ToString();
    }

    private static void AppendParametersClass(
        StringBuilder builder,
        IReadOnlyList<(ParameterSyntax Syntax, MacroParameterRole Role)> parameters,
        string parametersName)
    {
        builder.AppendLine($"class {parametersName} {{");
        foreach (var parameter in parameters)
        {
            builder.AppendLine(
                $"    var {parameter.Syntax.Identifier.ValueText}: {GetParameterType(parameter)}");
        }

        builder.Append($"    init(");
        builder.Append(string.Join(
            ", ",
            parameters.Select(parameter =>
            {
                var defaultValue = parameter.Syntax.DefaultValue is null
                    ? string.Empty
                    : $" = {parameter.Syntax.DefaultValue.Value}";
                return $"{parameter.Syntax.Identifier.ValueText}: {GetParameterType(parameter)}{defaultValue}";
            })));
        builder.AppendLine(") {");
        foreach (var parameter in parameters)
        {
            builder.AppendLine(
                $"        self.{parameter.Syntax.Identifier.ValueText} = {parameter.Syntax.Identifier.ValueText}");
        }
        builder.AppendLine("    }");
        builder.AppendLine("}");
    }

    private static string GetParameterType(
        (ParameterSyntax Syntax, MacroParameterRole Role) parameter)
        => MacroParameterRoleFacts.GetLoweredTypeName(
            parameter.Syntax,
            parameter.Role);

    private static void AppendTargetBinding(
        StringBuilder builder,
        MacroTargetClauseSyntax targetClause,
        string resultName)
    {
        var targetName = targetClause.Identifier.Kind == SyntaxKind.None
            ? "target"
            : targetClause.Identifier.ValueText;
        var syntaxType = targetClause.Target.ToString() switch
        {
            "Type" => "Raven.CodeAnalysis.Syntax.BaseTypeDeclarationSyntax",
            "Method" => "Raven.CodeAnalysis.Syntax.MethodDeclarationSyntax",
            "Property" => "Raven.CodeAnalysis.Syntax.PropertyDeclarationSyntax",
            "Field" => "Raven.CodeAnalysis.Syntax.FieldDeclarationSyntax",
            "Event" => "Raven.CodeAnalysis.Syntax.EventDeclarationSyntax",
            "Parameter" => "Raven.CodeAnalysis.Syntax.ParameterSyntax",
            "Accessor" => "Raven.CodeAnalysis.Syntax.AccessorDeclarationSyntax",
            "Constructor" => "Raven.CodeAnalysis.Syntax.ConstructorDeclarationSyntax",
            _ => "Raven.CodeAnalysis.Syntax.SyntaxNode"
        };

        builder.AppendLine(
            $"        let {targetName}: {syntaxType} = context.CurrentDeclaration else {{");
        builder.AppendLine($"            return {resultName}.Empty");
        builder.AppendLine("        }");
    }

    private static void AppendLoweredBody(
        StringBuilder builder,
        string source,
        MacroFunctionDeclarationSyntax declaration)
    {
        if (declaration.Body is { } body)
        {
            var contentSpan = TextSpan.FromBounds(
                body.OpenBraceToken.Span.End,
                body.CloseBraceToken.Span.Start);
            var content = new StringBuilder(
                source.Substring(contentSpan.Start, contentSpan.Length));
            foreach (var contribution in body.DescendantNodes()
                .OfType<MacroExpansionStatementSyntax>()
                .OrderByDescending(static statement => statement.Span.Start))
            {
                var relativeStart = contribution.Span.Start - contentSpan.Start;
                var expression = source.Substring(
                    contribution.Expression.Span.Start,
                    contribution.Expression.Span.Length);
                var method = contribution.Keyword.ValueText switch
                {
                    "expand" => "Expand",
                    "replace" => "Replace",
                    "introduce" => "Introduce",
                    _ => throw new InvalidOperationException()
                };
                content.Remove(relativeStart, contribution.Span.Length);
                content.Insert(relativeStart, $"__macroResult.{method}({expression})");
            }

            foreach (var line in content.ToString().Split('\n'))
                builder.AppendLine($"        {line}");
        }
        else if (declaration.ExpressionBody is { } expressionBody)
        {
            builder.AppendLine(
                $"        __macroResult.Expand({expressionBody.Expression})");
        }
    }

    private static string EscapeString(string value)
        => value.Replace("\\", "\\\\", StringComparison.Ordinal)
            .Replace("\"", "\\\"", StringComparison.Ordinal);
}
