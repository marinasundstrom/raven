using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public static class UpdateMethodGenerator
{
    public static MethodDeclarationSyntax GenerateUpdateMethod(string className, IEnumerable<PropOrParamType> parameters)
    {
        var paramDef = parameters.Select(property =>
        {
            var propertyType = ParseTypeName(property.Type);
            var propertyName = Identifier(property.Name);

            return Parameter(Identifier(HelperExtensions.FixIdentifier(property.Name)))
                            .WithType(propertyType);
        }).ToList();

        var nodeTypeName = ParseTypeName(className);

        ExpressionSyntax condition = null!;

        foreach (var p in parameters)
        {
            var expr1 = BinaryExpression(SyntaxKind.NotEqualsExpression,
                IdentifierName(p.Name), IdentifierName(Identifier(HelperExtensions.FixIdentifier(p.Name))));

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
                    nodeTypeName)
                .WithArgumentList(
                    ArgumentList(
                        SeparatedList(
                            paramDef.Select(p =>
                            {
                                return Argument(IdentifierName(p.Identifier));
                            }))));

        var updateMethodDeclaration = MethodDeclaration(nodeTypeName, "Update")
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