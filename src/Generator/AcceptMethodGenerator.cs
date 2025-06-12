using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public class AcceptMethodGeneratorOptions(string nodeClassName, bool makeInternal = false, string suffix = "Syntax", string? visitorClassName = null, string? nodeBaseClass = "")
{
    public string NodeClassName { get; } = nodeClassName;
    public bool MakeInternal { get; } = makeInternal;
    public string Suffix { get; } = suffix;
    public string? VisitorClassName { get; } = visitorClassName;
    public string? NodeBaseClass { get; } = nodeBaseClass;
    public string VisitorName => VisitorClassName is not null ? VisitorClassName : suffix;
}

public static class AcceptMethodGenerator
{
    public static IEnumerable<MethodDeclarationSyntax> GenerateAcceptMethods(AcceptMethodGeneratorOptions options)
    {
        var str = options.Suffix == "Syntax" ? "TNode" : "TResult";

        string name = options.NodeClassName.Replace(options.Suffix, string.Empty);

        if (options.Suffix == "Symbol")
        {
            name = name
                .Replace("PE", string.Empty)
                .Replace("Source", string.Empty)
                .Replace("Merged", string.Empty);
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
                        Token(options.MakeInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                        Token(SyntaxKind.OverrideKeyword)]))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            Identifier("visitor"))
                        .WithType(
                            IdentifierName($"{(options.Suffix == "Syntax" ? "" : "CodeAnalysis.")}{options.VisitorName}Visitor")))))
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
                        Token(options.MakeInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
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
                                Identifier($"{(options.Suffix == "Syntax" ? "" : "CodeAnalysis.")}{options.VisitorName}Visitor"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
                                        IdentifierName(str))))))))
        /*.WithConstraintClauses(
            SingletonList(
                TypeParameterConstraintClause(
                    IdentifierName("TResult"))
                .WithConstraints(
                    SingletonSeparatedList<TypeParameterConstraintSyntax>(
                        TypeConstraint(
                            IdentifierName(nodeBaseClass)))))) */
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
