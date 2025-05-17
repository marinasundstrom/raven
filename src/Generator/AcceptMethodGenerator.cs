using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public static class AcceptMethodGenerator
{
    public static IEnumerable<MethodDeclarationSyntax> GenerateAcceptMethods(string nodeClassName, bool makeInternal = false)
    {
        var methodName = $"Visit{nodeClassName.Replace("Syntax", string.Empty)}";

        return [
            MethodDeclaration(
                PredefinedType(
                    Token(SyntaxKind.VoidKeyword)),
                Identifier("Accept"))
            .WithModifiers(
                TokenList(
                    [
                        Token(makeInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                        Token(SyntaxKind.OverrideKeyword)]))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            Identifier("visitor"))
                        .WithType(
                            IdentifierName("SyntaxVisitor")))))
            .WithBody(
                Block(
                    SingletonList<StatementSyntax>(
                        ExpressionStatement(
                            InvocationExpression(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("visitor"),
                                    IdentifierName(methodName)))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            ThisExpression())))))))),
            MethodDeclaration(
                IdentifierName("TNode"),
                Identifier("Accept"))
            .WithModifiers(
                TokenList(
                    [
                        Token(makeInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                        Token(SyntaxKind.OverrideKeyword)]))
            .WithTypeParameterList(
                TypeParameterList(
                    SingletonSeparatedList(
                        TypeParameter(
                            Identifier("TNode")))))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            Identifier("visitor"))
                        .WithType(
                            GenericName(
                                Identifier("SyntaxVisitor"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
                                        IdentifierName("TNode"))))))))
            .WithBody(
                Block(
                    SingletonList<StatementSyntax>(
                        ReturnStatement(
                            InvocationExpression(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("visitor"),
                                    IdentifierName(methodName)))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            ThisExpression()))))))))];
    }
}
