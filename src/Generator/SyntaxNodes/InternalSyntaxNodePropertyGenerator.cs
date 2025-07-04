namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using System.Collections.Generic;
using System.Linq;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public static class InternalSyntaxNodePropertyGenerator
{
    public static List<PropertyDeclarationSyntax> GenerateSlotProperties(
        ConstructorDeclarationSyntax ctorDecl,
        IMethodSymbol ctorSymbol)
    {
        var result = new List<PropertyDeclarationSyntax>();

        if (ctorDecl.Initializer is null)
            return result;

        var baseArgs = ctorDecl.Initializer.ArgumentList.Arguments;
        if (baseArgs.Count < 2)
            return result;

        var slotExpr = baseArgs[1].Expression;

        // Recognize array-style and collection expressions
        IEnumerable<ExpressionSyntax>? expressions = slotExpr switch
        {
            ImplicitArrayCreationExpressionSyntax ia => ia.Initializer?.Expressions,
            ArrayCreationExpressionSyntax ea => ea.Initializer?.Expressions,
            InitializerExpressionSyntax direct => direct.Expressions,
            CollectionExpressionSyntax collection => collection.Elements
                .OfType<ExpressionElementSyntax>()
                .Select(e => e.Expression),
            _ => null
        };

        if (expressions is null)
            return result;

        // Map parameters (excluding diagnostics)
        var paramMap = ctorSymbol.Parameters
            .Where(p => p.Name != "diagnostics")
            .ToDictionary(p => p.Name, p => p);

        int index = 0;
        foreach (var expr in expressions)
        {
            var paramName = TryGetParameterName(expr);
            if (paramName == null || !paramMap.TryGetValue(paramName, out var paramSymbol))
            {
                index++;
                continue;
            }

            var isNullable = paramSymbol.Type.NullableAnnotation == NullableAnnotation.Annotated;
            var hasNullCheck = ContainsNullCheck(expr);

            var displayType = paramSymbol.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

            // If nullable and no null-check in expr, append '?'
            if (isNullable && !hasNullCheck && !displayType.EndsWith("?"))
                displayType += "?";

            var propertyType = ParseTypeName(displayType);
            var propertyName = ToPascalCase(paramName);

            var property = PropertyDeclaration(propertyType, Identifier(propertyName))
                .AddModifiers(Token(SyntaxKind.PublicKeyword))
                .WithExpressionBody(
                    ArrowExpressionClause(
                        InvocationExpression(
                            GenericName(Identifier("GetSlot"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SingletonSeparatedList<TypeSyntax>(propertyType))))
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        Literal(index))))))))
                .WithSemicolonToken(Token(SyntaxKind.SemicolonToken));

            result.Add(property);
            index++;
        }

        return result;
    }

    private static string? TryGetParameterName(ExpressionSyntax expr)
    {
        return expr switch
        {
            IdentifierNameSyntax ident => ident.Identifier.Text,
            MemberAccessExpressionSyntax member => member.Name.Identifier.Text,
            BinaryExpressionSyntax binary when binary.IsKind(SyntaxKind.CoalesceExpression)
                => TryGetParameterName(binary.Left),
            ParenthesizedExpressionSyntax paren => TryGetParameterName(paren.Expression),
            ConditionalExpressionSyntax cond => TryGetParameterName(cond.WhenTrue),
            InvocationExpressionSyntax invocation when invocation.Expression is IdentifierNameSyntax ident
                => ident.Identifier.Text,
            _ => null
        };
    }

    private static bool ContainsNullCheck(ExpressionSyntax expr)
    {
        return expr is BinaryExpressionSyntax binary && binary.IsKind(SyntaxKind.CoalesceExpression);
    }

    private static string ToPascalCase(string name)
    {
        if (string.IsNullOrEmpty(name))
            return name;

        return char.ToUpper(name[0]) + name.Substring(1);
    }
}