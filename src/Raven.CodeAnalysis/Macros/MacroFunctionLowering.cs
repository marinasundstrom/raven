using System.Text;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroFunctionLowering
{
    public static SyntaxTree Lower(SyntaxTree syntaxTree)
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
                LowerDeclaration(source, declaration));
        }

        return SyntaxTree.ParseText(
            SourceText.From(lowered.ToString(), syntaxTree.Encoding),
            syntaxTree.Options,
            syntaxTree.FilePath);
    }

    private static string LowerDeclaration(
        string source,
        MacroFunctionDeclarationSyntax declaration)
    {
        var suffix = declaration.Span.Start.ToString(System.Globalization.CultureInfo.InvariantCulture);
        var providerName = $"__RavenMacroFunction_{declaration.Identifier.ValueText}_{suffix}";
        var parametersName = $"{providerName}_Parameters";
        var isAttached = declaration.TargetClause is not null;
        var hasParameters = declaration.ParameterList.Parameters.Count > 0;
        var interfaceName = isAttached
            ? "Raven.CodeAnalysis.Macros.IAttachedDeclarationMacro"
            : "Raven.CodeAnalysis.Macros.IFreestandingExpressionMacro";
        var contextName = isAttached
            ? "Raven.CodeAnalysis.Macros.AttachedMacroContext"
            : "Raven.CodeAnalysis.Macros.FreestandingMacroContext";
        var resultName = isAttached
            ? "Raven.CodeAnalysis.Macros.MacroExpansionResult"
            : "Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult";
        var buildMethod = isAttached ? "BuildAttached" : "BuildFreestanding";

        if (hasParameters)
        {
            interfaceName += $"<{parametersName}>";
            contextName += $"<{parametersName}>";
        }

        var builder = new StringBuilder();
        if (hasParameters)
            AppendParametersClass(builder, declaration, parametersName);

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

        foreach (var parameter in declaration.ParameterList.Parameters)
        {
            builder.AppendLine(
                $"        let {parameter.Identifier.ValueText} = context.Parameters.{parameter.Identifier.ValueText}");
        }

        if (declaration.TargetClause is { } targetClause)
            AppendTargetBinding(builder, targetClause, resultName);

        AppendLoweredBody(builder, source, declaration);
        builder.AppendLine($"        return __macroResult.{buildMethod}()");
        builder.AppendLine("    }");
        builder.AppendLine("}");
        return builder.ToString();
    }

    private static void AppendParametersClass(
        StringBuilder builder,
        MacroFunctionDeclarationSyntax declaration,
        string parametersName)
    {
        builder.AppendLine($"class {parametersName} {{");
        foreach (var parameter in declaration.ParameterList.Parameters)
        {
            builder.AppendLine(
                $"    var {parameter.Identifier.ValueText}: {GetParameterType(parameter)}");
        }

        builder.Append($"    init(");
        builder.Append(string.Join(
            ", ",
            declaration.ParameterList.Parameters.Select(parameter =>
            {
                var defaultValue = parameter.DefaultValue is null
                    ? string.Empty
                    : $" = {parameter.DefaultValue.Value}";
                return $"{parameter.Identifier.ValueText}: {GetParameterType(parameter)}{defaultValue}";
            })));
        builder.AppendLine(") {");
        foreach (var parameter in declaration.ParameterList.Parameters)
        {
            builder.AppendLine(
                $"        self.{parameter.Identifier.ValueText} = {parameter.Identifier.ValueText}");
        }
        builder.AppendLine("    }");
        builder.AppendLine("}");
    }

    private static string GetParameterType(ParameterSyntax parameter)
        => parameter.TypeAnnotation?.Type.ToString() ?? "object";

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
