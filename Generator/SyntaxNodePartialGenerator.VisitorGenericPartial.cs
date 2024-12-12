namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GenerateVisitorGenericPartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        List<MethodDeclarationSyntax> methods = [];

        var methodName = $"Visit{classSymbol.Name.Replace("Syntax", string.Empty)}";

        methods.Add(SyntaxFactory.MethodDeclaration(
            SyntaxFactory.IdentifierName("TResult"),
            SyntaxFactory.Identifier(methodName))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    [
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                                SyntaxFactory.Token(SyntaxKind.VirtualKeyword)]))
                            .WithParameterList(
                                SyntaxFactory.ParameterList(
                                    SyntaxFactory.SingletonSeparatedList<ParameterSyntax>(
                                        SyntaxFactory.Parameter(
                                            SyntaxFactory.Identifier("node"))
                                        .WithType(
                                            SyntaxFactory.IdentifierName(className)))))
                            .WithBody(
                                SyntaxFactory.Block(
                                    SyntaxFactory.SingletonList<StatementSyntax>(
                                        SyntaxFactory.ReturnStatement(
                                            SyntaxFactory.InvocationExpression(
                                                SyntaxFactory.IdentifierName("DefaultVisit"))
                                            .WithArgumentList(
                                                SyntaxFactory.ArgumentList(
                                                    SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                                                        SyntaxFactory.Argument(
                                                            SyntaxFactory.IdentifierName("node"))))))))));


        // Generate the partial class
        var generatedClass = ClassDeclaration("SyntaxVisitor")
            .WithTypeParameterList(
                TypeParameterList(
                    SeparatedList<TypeParameterSyntax>([TypeParameter("TResult")])))
            .WithModifiers(TokenList(
                Token(SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }
}
