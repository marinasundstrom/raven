using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public static class UpdateMethodGenerator
{
    private static string FixIdentifier(PropOrParamType property)
    {
        var name = property.Name.ToCamelCase();

        var x = SyntaxFacts.IsKeywordKind(SyntaxFacts.GetKeywordKind(name));
        if (x)
        {
            return $"@{name}";
        }
        return name;
    }

    public static MethodDeclarationSyntax GenerateUpdateMethod(string className, IEnumerable<PropOrParamType> parameters)
    {
        var paramDef = parameters.Select(property =>
        {
            var propertyType = ParseTypeName(property.Type);
            var propertyName = Identifier(property.Name);

            return Parameter(Identifier(FixIdentifier(property)))
                            .WithType(propertyType);
        }).ToList();

        var typeName = ParseTypeName(className);

        ExpressionSyntax condition = null!;

        foreach (var p in parameters)
        {
            var expr1 = BinaryExpression(SyntaxKind.NotEqualsExpression,
                IdentifierName(p.Name), IdentifierName(FixIdentifier(p)));

            if (condition is null)
            {
                condition = expr1;
            }
            else
            {
                condition = BinaryExpression(SyntaxKind.LogicalOrExpression,
                    condition, expr1);
            }
        }

        var expr = ObjectCreationExpression(
                    typeName)
                .WithArgumentList(
                    ArgumentList(
                        SeparatedList<ArgumentSyntax>(
                            paramDef.Select(p =>
                            {
                                return Argument(IdentifierName(p.Identifier));
                            }))));

        var updateMethodDeclaration = MethodDeclaration(typeName, "Update")
                .WithModifiers([Token(SyntaxKind.PublicKeyword)])
                .WithParameterList(
                    ParameterList(
                        SeparatedList(
                            paramDef
                ))).WithBody(
                    Block(
                        IfStatement(condition,
                            Block(ReturnStatement(expr))),
                        ReturnStatement(ThisExpression())));

        return updateMethodDeclaration;
    }
}