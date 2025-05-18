using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public static class AcceptMethodGenerator
{
    public static IEnumerable<MethodDeclarationSyntax> GenerateAcceptMethods(string nodeClassName, bool makeInternal = false, string suffix = "Syntax")
    {
        var str = suffix == "Syntax" ? "TNode" : "TResult";

        string name = nodeClassName.Replace(suffix, string.Empty);

        if (suffix == "Symbol")
        {
            name = name
                .Replace("Metadata", string.Empty)
                .Replace("Source", string.Empty);
        }

        var methodName = $"Visit{name}";

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
                            IdentifierName($"{(suffix == "Syntax" ? "" : "CodeAnalysis.")}{suffix}Visitor")))))
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
                IdentifierName(str),
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
                            Identifier(str)))))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            Identifier("visitor"))
                        .WithType(
                            GenericName(
                                Identifier($"{(suffix == "Syntax" ? "" : "CodeAnalysis.")}{suffix}Visitor"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
                                        IdentifierName(str))))))))
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
