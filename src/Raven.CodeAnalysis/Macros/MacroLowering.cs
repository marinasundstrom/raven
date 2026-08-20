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
        var macroDeclarations = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Cast<SyntaxNode>();
        var methodShapedClasses = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Where(LocalMacroSyntaxClassifier.IsMethodShapedMacroClass)
            .Cast<SyntaxNode>();
        var declarations = macroDeclarations
            .Concat(methodShapedClasses)
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
                declaration switch
                {
                    MacroDeclarationSyntax macro => LowerDeclaration(source, macro, semanticModel),
                    ClassDeclarationSyntax methodClass => LowerMethodShapedClass(source, methodClass, semanticModel),
                    _ => throw new InvalidOperationException(),
                });
        }

        return SyntaxTree.ParseText(
            SourceText.From(lowered.ToString(), syntaxTree.Encoding),
            syntaxTree.Options,
            syntaxTree.FilePath);
    }

    private static string LowerMethodShapedClass(
        string source,
        ClassDeclarationSyntax declaration,
        SemanticModel semanticModel)
    {
        var expand = declaration.Members
            .OfType<MethodDeclarationSyntax>()
            .Single(static method => method.Identifier.ValueText == "Expand");
        var methodSymbol = semanticModel.GetDeclaredSymbol(expand) as IMethodSymbol;
        var parameters = expand.ParameterList.Parameters
            .Select((syntax, index) =>
            {
                var parameter = methodSymbol?.Parameters[index];
                var role = parameter is null
                    ? MacroParameterRole.Value
                    : MacroParameterRoleFacts.GetRole(parameter.Type);
                return (
                    Syntax: syntax,
                    Parameter: parameter,
                    Role: role,
                    ContextKind: parameter is null
                        ? MacroContextKind.None
                        : MacroParameterRoleFacts.GetContextKind(parameter.Type));
            })
            .ToArray();
        var isAttached = parameters.Any(static parameter => parameter.ContextKind == MacroContextKind.Attached) ||
            methodSymbol?.ReturnType.Name == nameof(MacroExpansionResult);
        var hasTokenTreeBody = parameters.Any(static parameter =>
            parameter.Role == MacroParameterRole.TokenBody ||
            parameter.ContextKind == MacroContextKind.TokenTree);
        var invocationOrdinal = 0;
        var parameterMetadata = parameters
            .Select((parameter, declarationOrdinal) =>
            {
                var source = isAttached &&
                    parameter.Parameter is not null &&
                    MacroParameterRoleFacts.IsAttachedTargetType(parameter.Parameter.Type)
                        ? MacroParameterSource.AttachedTarget
                        : parameter.Role switch
                        {
                            MacroParameterRole.SyntaxInput => MacroParameterSource.SyntaxInput,
                            MacroParameterRole.Context => MacroParameterSource.Context,
                            MacroParameterRole.TokenBody => MacroParameterSource.TokenBody,
                            _ => MacroParameterSource.Value,
                        };
                return (
                    parameter.Syntax,
                    parameter.Parameter,
                    parameter.Role,
                    Source: source,
                    DeclarationOrdinal: declarationOrdinal,
                    InvocationOrdinal: source is MacroParameterSource.Value or MacroParameterSource.SyntaxInput
                        ? invocationOrdinal++
                        : -1);
            })
            .ToArray();
        var usedNames = declaration.DescendantTokens()
            .Where(static token => token.Kind == SyntaxKind.IdentifierToken)
            .Select(static token => token.ValueText)
            .ToHashSet(StringComparer.Ordinal);
        var executionName = AllocateGeneratedName(usedNames, "__macroExecution");
        var resultName = AllocateGeneratedName(usedNames, "__macroResult");
        var helperName = AllocateGeneratedName(usedNames, "__ExpandAuthored");
        var declaredName = declaration.Identifier.ValueText.EndsWith("Macro", StringComparison.Ordinal)
            ? declaration.Identifier.ValueText[..^"Macro".Length]
            : declaration.Identifier.ValueText;
        var namespaceName = GetDeclaredNamespace(declaration);
        var builder = new StringBuilder();

        foreach (var attributeList in declaration.AttributeLists)
            builder.AppendLine(source.Substring(attributeList.Span.Start, attributeList.Span.Length));

        foreach (var typeParameter in (declaration.TypeParameterList?.Parameters ?? []).Select((syntax, ordinal) => (syntax, ordinal)))
        {
            builder.AppendLine(
                $"[Raven.CodeAnalysis.Macros.MacroExecutorTypeParameter(\"{EscapeString(typeParameter.syntax.Identifier.ValueText)}\", {typeParameter.ordinal})]");
        }
        foreach (var parameter in parameterMetadata)
        {
            var runtimeType = parameter.Parameter?.Type.TypeKind == TypeKind.TypeParameter
                ? "object"
                : MacroParameterRoleFacts.GetLoweredTypeName(parameter.Syntax, parameter.Role);
            var typeDisplay = parameter.Syntax.TypeAnnotation?.Type.ToString() ?? "object";
            var isRequired = parameter.InvocationOrdinal >= 0 && parameter.Syntax.DefaultValue is null;
            var defaultDisplay = parameter.Syntax.DefaultValue?.Value.ToString() ?? string.Empty;
            builder.AppendLine(
                $"[Raven.CodeAnalysis.Macros.MacroExecutorParameter(\"{EscapeString(parameter.Syntax.Identifier.ValueText)}\", typeof({runtimeType}), \"{EscapeString(typeDisplay)}\", Raven.CodeAnalysis.Macros.MacroParameterSource.{parameter.Source}, {parameter.DeclarationOrdinal}, {parameter.InvocationOrdinal}, {isRequired.ToString().ToLowerInvariant()}, \"{EscapeString(defaultDisplay)}\")]");
        }

        var visibility = declaration.Modifiers.Any(static modifier => modifier.Kind == SyntaxKind.PublicKeyword)
            ? "public "
            : string.Empty;
        var capabilityInterfaces = declaration.BaseList?.Types
            .Where(static baseType => !IsInterfaceNamed(baseType.Type, nameof(IMacroDefinition)))
            .Select(static baseType => baseType.Type.ToString()) ?? [];
        var executorInterfaces = string.Join(
            ", ",
            new[] { "Raven.CodeAnalysis.Macros.IMacroExecutor" }.Concat(capabilityInterfaces));
        builder.AppendLine($"{visibility}class {declaration.Identifier.ValueText} : {executorInterfaces} {{");
        if (!HasDeclaredMember(declaration, nameof(IMacroDefinition.Namespace)))
            builder.AppendLine($"    val Namespace: string => \"{EscapeString(namespaceName)}\"");
        if (!HasDeclaredMember(declaration, nameof(IMacroDefinition.Name)))
            builder.AppendLine($"    val Name: string => \"{EscapeString(declaredName)}\"");
        builder.AppendLine(
            $"    val ApplicationKind: Raven.CodeAnalysis.Macros.MacroApplicationKind => Raven.CodeAnalysis.Macros.MacroApplicationKind.{(isAttached ? "Attached" : "Freestanding")}");
        if (parameterMetadata.Any(static parameter => parameter.InvocationOrdinal >= 0))
            builder.AppendLine("    val AcceptsArguments: bool => true");
        if (hasTokenTreeBody)
            builder.AppendLine("    val HasTokenBody: bool => true");

        builder.AppendLine($"    func Expand({executionName}: Raven.CodeAnalysis.Macros.MacroExecutionContext) -> Raven.CodeAnalysis.Macros.MacroExecutionResult {{");
        foreach (var parameter in parameterMetadata)
        {
            var runtimeType = parameter.Parameter?.Type.TypeKind == TypeKind.TypeParameter
                ? "object"
                : MacroParameterRoleFacts.GetLoweredTypeName(parameter.Syntax, parameter.Role);
            var value = parameter.Source switch
            {
                MacroParameterSource.Context => $"{executionName}.GetContext<{runtimeType}>()",
                MacroParameterSource.TokenBody => $"{executionName}.GetContext<Raven.CodeAnalysis.Macros.TokenTreeMacroContext>().CreateTokenStream()",
                MacroParameterSource.AttachedTarget => $"{executionName}.GetAttachedTarget<{runtimeType}>()",
                _ when parameter.Syntax.DefaultValue is { } defaultValue =>
                    $"{executionName}.GetArgumentOrDefault<{runtimeType}>({parameter.InvocationOrdinal}, \"{EscapeString(parameter.Syntax.Identifier.ValueText)}\", {defaultValue.Value})",
                _ => $"{executionName}.GetArgument<{runtimeType}>({parameter.InvocationOrdinal}, \"{EscapeString(parameter.Syntax.Identifier.ValueText)}\")",
            };
            builder.AppendLine($"        let {parameter.Syntax.Identifier.ValueText}: {runtimeType} = {value}");
        }
        builder.AppendLine($"        let {resultName} = {helperName}({string.Join(", ", parameterMetadata.Select(static parameter => parameter.Syntax.Identifier.ValueText))})");
        AppendMethodResult(builder, methodSymbol?.ReturnType, expand.ReturnType?.Type, resultName, isAttached);
        builder.AppendLine("    }");

        var helperParameters = string.Join(", ", parameterMetadata.Select(parameter =>
        {
            var runtimeType = parameter.Parameter?.Type.TypeKind == TypeKind.TypeParameter
                ? "object"
                : MacroParameterRoleFacts.GetLoweredTypeName(parameter.Syntax, parameter.Role);
            return $"{parameter.Syntax.Identifier.ValueText}: {runtimeType}";
        }));
        var helperReturnType = methodSymbol?.ReturnType.TypeKind == TypeKind.TypeParameter
            ? "object"
            : expand.ReturnType?.Type.ToString() ?? "object";
        if (expand.Body is { } body)
        {
            builder.AppendLine($"    func {helperName}({helperParameters}) -> {helperReturnType} {source.Substring(body.Span.Start, body.Span.Length)}");
        }
        else if (expand.ExpressionBody is { } expressionBody)
        {
            builder.AppendLine($"    func {helperName}({helperParameters}) -> {helperReturnType} => {expressionBody.Expression}");
        }

        var typeParameterNames = declaration.TypeParameterList?.Parameters
            .Select(static parameter => parameter.Identifier.ValueText)
            .ToHashSet(StringComparer.Ordinal) ?? [];
        foreach (var member in declaration.Members.Where(member => !ReferenceEquals(member, expand)))
        {
            builder.AppendLine(EraseTypeParameters(source, member, typeParameterNames));
        }
        builder.AppendLine("}");
        return builder.ToString();
    }

    private static bool IsInterfaceNamed(TypeSyntax type, string name)
        => string.Equals(
            type.DescendantTokens()
                .LastOrDefault(static token => token.Kind == SyntaxKind.IdentifierToken)
                .ValueText,
            name,
            StringComparison.Ordinal);

    private static bool HasDeclaredMember(ClassDeclarationSyntax declaration, string name)
        => declaration.Members.Any(member => member switch
        {
            PropertyDeclarationSyntax property => property.Identifier.ValueText == name,
            MethodDeclarationSyntax method => method.Identifier.ValueText == name,
            _ => false,
        });

    private static string EraseTypeParameters(
        string source,
        MemberDeclarationSyntax member,
        ISet<string> typeParameterNames)
    {
        var text = new StringBuilder(source.Substring(member.Span.Start, member.Span.Length));
        foreach (var token in member.DescendantTokens()
            .Where(static token => token.Kind == SyntaxKind.IdentifierToken)
            .Where(token => typeParameterNames.Contains(token.ValueText))
            .OrderByDescending(static token => token.Span.Start))
        {
            text.Remove(token.Span.Start - member.Span.Start, token.Span.Length);
            text.Insert(token.Span.Start - member.Span.Start, "object");
        }

        return text.ToString();
    }

    private static void AppendMethodResult(
        StringBuilder builder,
        ITypeSymbol? returnType,
        TypeSyntax? returnTypeSyntax,
        string resultName,
        bool isAttached)
    {
        var returnTypeName = returnType?.ToDisplayString() ?? returnTypeSyntax?.ToString() ?? "object";
        if (returnTypeName.EndsWith(nameof(MacroExecutionResult), StringComparison.Ordinal))
            builder.AppendLine($"        return {resultName}");
        else if (returnTypeName.EndsWith(nameof(FreestandingMacroExpansionResult), StringComparison.Ordinal))
            builder.AppendLine($"        return Raven.CodeAnalysis.Macros.MacroExecutionResult.Freestanding({resultName})");
        else if (returnTypeName.EndsWith(nameof(MacroExpansionResult), StringComparison.Ordinal))
            builder.AppendLine($"        return Raven.CodeAnalysis.Macros.MacroExecutionResult.Attached({resultName})");
        else if (isAttached)
            builder.AppendLine($"        return Raven.CodeAnalysis.Macros.MacroExecutionResult.Attached(Raven.CodeAnalysis.Macros.MacroExpansionResult.FromReplacement({resultName}))");
        else
            builder.AppendLine($"        return Raven.CodeAnalysis.Macros.MacroExecutionResult.Freestanding(Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult.FromNode({resultName}))");
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
                Source: symbol?.ParameterBindings[index].Source ?? MacroParameterSource.Value,
                DeclarationOrdinal: index,
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
        var freestandingContextParameters = parameters
            .Where(static parameter =>
                parameter.ContextKind == MacroContextKind.Freestanding)
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
            : "Raven.CodeAnalysis.Macros.FreestandingMacroContext";
        var categoryResultName = hasTokenTreeBody
            ? "Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult"
            : isAttached
            ? "Raven.CodeAnalysis.Macros.MacroExpansionResult"
            : "Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult";
        var buildMethod = isAttached && !hasTokenTreeBody ? "BuildAttached" : "BuildFreestanding";
        var resultFactory = isAttached && !hasTokenTreeBody ? "Attached" : "Freestanding";
        if (hasEditorMetadataContributions)
            interfaceName += ", Raven.CodeAnalysis.Macros.IMacroExpansionMetadataProvider";

        var builder = new StringBuilder();
        if (declaration.TypeParameterList is { Parameters.Count: > 0 } typeParameterList)
        {
            for (var index = 0; index < typeParameterList.Parameters.Count; index++)
            {
                builder.AppendLine(
                    $"[Raven.CodeAnalysis.Macros.MacroExecutorTypeParameter(\"{EscapeString(typeParameterList.Parameters[index].Identifier.ValueText)}\", {index})]");
            }
        }
        foreach (var parameter in parameters)
        {
            var parameterType = GetParameterType(parameter);
            var typeDisplayName = parameter.Syntax.TypeAnnotation?.Type.ToString() ?? "object";
            var invocationOrdinal = parameter.InvocationOrdinal is { } ordinal
                ? ordinal.ToString(System.Globalization.CultureInfo.InvariantCulture)
                : "-1";
            var isRequired = parameter.InvocationOrdinal is not null &&
                parameter.Syntax.DefaultValue is null;
            var defaultValueDisplay = parameter.Syntax.DefaultValue?.Value.ToString() ?? string.Empty;
            builder.AppendLine(
                $"[Raven.CodeAnalysis.Macros.MacroExecutorParameter(\"{EscapeString(parameter.Syntax.Identifier.ValueText)}\", typeof({parameterType}), \"{EscapeString(typeDisplayName)}\", Raven.CodeAnalysis.Macros.MacroParameterSource.{parameter.Source}, {parameter.DeclarationOrdinal}, {invocationOrdinal}, {isRequired.ToString().ToLowerInvariant()}, \"{EscapeString(defaultValueDisplay)}\")]");
        }
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
            $"    val ApplicationKind: Raven.CodeAnalysis.Macros.MacroApplicationKind => Raven.CodeAnalysis.Macros.MacroApplicationKind.{(isAttached ? "Attached" : "Freestanding")}");
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
        foreach (var contextParameter in freestandingContextParameters)
        {
            builder.AppendLine(
                $"        let {contextParameter.Syntax.Identifier.ValueText}: Raven.CodeAnalysis.Macros.FreestandingMacroContext = {contextVariableName}");
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
            MacroParameterSource Source,
            int DeclarationOrdinal,
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

    private static string GetDeclaredNamespace(SyntaxNode declaration)
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
