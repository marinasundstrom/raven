using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private bool TryCreateCallerInfoArgument(
        ParameterSymbol parameter,
        ImmutableArray<ParameterSymbol> parameters,
        BoundArgument?[] mappedArguments,
        ParameterSymbol? receiverParameter,
        SyntaxNode? receiverSyntax,
        BoundArgument[] paramsArguments,
        int paramsParameterIndex,
        SyntaxNode? callSyntax,
        out BoundExpression argument)
    {
        argument = null!;
        if (!parameter.HasExplicitDefaultValue)
            return false;

        if (CallerInfoAttributeFacts.GetEffectiveAttribute(parameter.GetAttributes()) is not { } attribute)
            return false;

        object? value = CallerInfoAttributeFacts.GetKind(attribute) switch
        {
            CallerInfoKind.LineNumber when callSyntax is not null => callSyntax.GetLocation().GetLineSpan().StartLinePosition.Line + 1,
            CallerInfoKind.FilePath when callSyntax is not null => callSyntax.SyntaxTree.FilePath ?? string.Empty,
            CallerInfoKind.MemberName when callSyntax is not null => GetCallerMemberName(callSyntax),
            CallerInfoKind.ArgumentExpression when attribute.ConstructorArguments is [{ Value: string name }] && name != parameter.Name =>
                GetCallerArgumentText(name, parameters, mappedArguments, receiverParameter, receiverSyntax,
                    paramsArguments, paramsParameterIndex, callSyntax),
            _ => null
        };
        if (value is null)
            return false;

        var isLineNumber = value is int;
        var type = Compilation.GetSpecialType(isLineNumber ? SpecialType.System_Int32 : SpecialType.System_String);
        var conversion = Compilation.ClassifyConversion(type, parameter.Type, includeUserDefined: false);
        if (!conversion.Exists || !conversion.IsImplicit)
            return false;

        var literalKind = isLineNumber ? BoundLiteralExpressionKind.NumericLiteral : BoundLiteralExpressionKind.StringLiteral;
        argument = ApplyConversion(new BoundLiteralExpression(literalKind, value, type), parameter.Type, conversion);
        return true;
    }

    private string? GetCallerMemberName(SyntaxNode callSyntax)
    {
        // Initializers and local functions may bind without the field/property
        // as their containing symbol. Use the nearest lexical member first.
        foreach (var ancestor in callSyntax.AncestorsAndSelf())
        {
            if (ancestor is VariableDeclaratorSyntax variable &&
                variable.Parent?.Parent is FieldDeclarationSyntax)
                return variable.Identifier.ValueText;
            if (ancestor is not MemberDeclarationSyntax member)
                continue;
            var name = member switch
            {
                MethodDeclarationSyntax method => method.Identifier.ValueText,
                PropertyDeclarationSyntax property => property.Identifier.ValueText,
                EventDeclarationSyntax @event => @event.Identifier.ValueText,
                _ => null
            };
            if (name is not null)
                return name;
            break;
        }

        for (var symbol = ContainingSymbol; symbol is not null; symbol = symbol.ContainingSymbol)
        {
            if (symbol is MethodSymbol method)
            {
                if (method.MethodKind == MethodKind.LambdaMethod ||
                    method.MethodKind == MethodKind.Function && method.ContainingSymbol is MethodSymbol)
                    continue;
                return method.AssociatedSymbol?.Name ?? method.Name;
            }
            if (symbol is PropertySymbol or FieldSymbol or EventSymbol)
                return symbol.Name;
        }
        return null;
    }

    private string? GetCallerArgumentText(
        string name,
        ImmutableArray<ParameterSymbol> parameters,
        BoundArgument?[] mappedArguments,
        ParameterSymbol? receiverParameter,
        SyntaxNode? receiverSyntax,
        BoundArgument[] paramsArguments,
        int paramsParameterIndex,
        SyntaxNode? callSyntax)
    {
        SyntaxNode? syntax = null;
        if (receiverParameter?.Name == name)
            syntax = receiverSyntax;
        else
        {
            for (var i = 0; i < parameters.Length; i++)
            {
                if (parameters[i].Name != name)
                    continue;
                syntax = mappedArguments[i]?.Syntax;
                if (syntax is null && i == paramsParameterIndex && paramsArguments.Length > 0)
                {
                    // A normal-form array keeps its expression. C# captures the
                    // invocation for a synthesized expanded params array.
                    syntax = paramsArguments.Length == 1 && !paramsArguments[0].IsSpread &&
                        paramsArguments[0].Type is not null &&
                        TryClassifyParameterArgument(parameters[i].Type, paramsArguments[0].Expression, out _, out _)
                        ? paramsArguments[0].Syntax
                        : callSyntax;
                }
                break;
            }
        }

        if (syntax is ArgumentSyntax argumentSyntax)
            syntax = argumentSyntax.Expression;
        while (syntax is ParenthesizedExpressionSyntax parenthesized)
            syntax = parenthesized.Expression;
        return syntax?.ToString();
    }
}
