using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Implements low-level expansion mechanics used by the standard Raven macro library.
/// </summary>
/// <remarks>
/// This is a transitional compiler-side implementation. As the Raven macro
/// API gains higher-level syntax transformation helpers, this behavior should
/// move wholly or partly into Raven.Macros.
/// </remarks>
public static partial class StandardMacroExpansions
{
    private const string ErrorExpansionFailedCode = "ERROR001";
    private const string ErrorMessageDuplicateCode = "ERRORMESSAGE003";

    /// <summary>
    /// Adds Raven's standard error contract to a union carrier.
    /// </summary>
    public static MacroExpansionResult ExpandError(
        AttachedMacroContext context,
        UnionDeclarationSyntax target)
    {
        var messageExpression = CreateMessageExpression(context, target);
        var template = SyntaxFactory.ParseMemberDeclaration(
            "union __GeneratedError: System.IError { " +
            "case Placeholder " +
            $"val Message: string => {messageExpression} " +
            "val Cause: System.IError? => null " +
            "}") as UnionDeclarationSyntax;
        var generatedProperties = template?.Members
            .OfType<PropertyDeclarationSyntax>()
            .ToDictionary(static property => property.Identifier.ValueText, StringComparer.Ordinal);
        if (template?.BaseList is null ||
            generatedProperties is null ||
            !generatedProperties.TryGetValue("Message", out var message) ||
            !generatedProperties.TryGetValue("Cause", out var cause))
        {
            return MacroExpansionResult.FromDiagnostic(
                context.CreateDiagnostic(
                    "The Error macro could not construct its expansion.",
                    code: ErrorExpansionFailedCode));
        }

        var replacement = HasErrorInterface(target.BaseList)
            ? target
            : target.WithBaseList(AddBaseType(target.BaseList, template.BaseList.Types[0]));
        var introducedMembers = ImmutableArray.CreateBuilder<MemberDeclarationSyntax>();
        if (!HasProperty(target, "Message"))
            introducedMembers.Add(message);
        if (!HasProperty(target, "Cause"))
            introducedMembers.Add(cause);

        return MacroExpansionResult.FromReplacement(replacement, introducedMembers.ToImmutable());
    }

    private static string CreateMessageExpression(
        AttachedMacroContext context,
        UnionDeclarationSyntax target)
    {
        var arms = new List<string>();
        foreach (var caseDeclaration in target.Members.OfType<CaseDeclarationSyntax>())
        {
            var messageAttributes = GetMacroAttributes(caseDeclaration.AttributeLists, "ErrorMessage").ToArray();
            if (messageAttributes.Length == 0)
                continue;

            if (messageAttributes.Length > 1)
            {
                foreach (var duplicate in messageAttributes.Skip(1))
                {
                    context.ReportDiagnostic(
                        "A union case can declare only one ErrorMessage.",
                        syntax: duplicate,
                        code: ErrorMessageDuplicateCode);
                }
            }

            var expression = messageAttributes[0].ArgumentList?.Arguments.FirstOrDefault()?.Expression;
            if (expression is null || !IsMessageExpression(expression))
                continue;

            arms.Add($"{CreateCasePattern(caseDeclaration)} => {expression.ToFullString().Trim()}");
        }

        if (arms.Count == 0)
            return "self.ToString()";

        arms.Add("_ => self.ToString()");
        return $"self match {{\n{string.Join("\n", arms)}\n}}";
    }

    private static string CreateCasePattern(CaseDeclarationSyntax declaration)
    {
        var payloadNames = declaration.ParameterList?.Parameters
            .Select(static parameter => parameter.Identifier.ValueText)
            .Concat(declaration.FieldClause?.Fields.Select(static field => field.Identifier.ValueText) ?? [])
            .Where(static name => !string.IsNullOrWhiteSpace(name))
            .ToArray() ?? declaration.FieldClause?.Fields
                .Select(static field => field.Identifier.ValueText)
                .Where(static name => !string.IsNullOrWhiteSpace(name))
                .ToArray() ?? [];
        var payloadPattern = payloadNames.Length == 0
            ? string.Empty
            : $"({string.Join(", ", payloadNames.Select(static name => $"let {name}"))})";
        return $"{declaration.Identifier.ValueText}{payloadPattern}";
    }

    private static bool IsMessageExpression(ExpressionSyntax expression)
        => expression is InterpolatedStringExpressionSyntax ||
           expression is LiteralExpressionSyntax { Token.Kind: SyntaxKind.StringLiteralToken or SyntaxKind.MultiLineStringLiteralToken };

    private static IEnumerable<AttributeSyntax> GetMacroAttributes(
        SyntaxList<AttributeListSyntax> attributeLists,
        string name)
        => attributeLists
            .SelectMany(static list => list.Attributes)
            .Where(attribute =>
                attribute.TryGetMacroName(out var macroName) &&
                (macroName == name || macroName.EndsWith($".{name}", StringComparison.Ordinal)));

    private static bool HasErrorInterface(BaseListSyntax? baseList)
        => baseList?.Types.Any(static baseType =>
            baseType.Type.ToString() is "IError" or "System.IError") == true;

    private static bool HasProperty(UnionDeclarationSyntax target, string name)
        => target.Members.OfType<BasePropertyDeclarationSyntax>().Any(property =>
            property.Identifier.ValueText == name);

    private static BaseListSyntax AddBaseType(
        BaseListSyntax? existing,
        BaseTypeSyntax baseType)
    {
        if (existing is null)
        {
            return SyntaxFactory.BaseList(
                SyntaxFactory.SingletonSeparatedList<BaseTypeSyntax>(
                    new SyntaxNodeOrToken(baseType)));
        }

        var items = existing.Types.GetWithSeparators().ToList();
        items.Add(new SyntaxNodeOrToken(SyntaxFactory.CommaToken));
        items.Add(new SyntaxNodeOrToken(baseType));
        return existing.WithTypes(SyntaxFactory.SeparatedList<BaseTypeSyntax>(items.ToArray()));
    }
}
