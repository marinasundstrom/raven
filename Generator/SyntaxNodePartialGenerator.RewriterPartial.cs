namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GenerateRewriterPartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        List<MethodDeclarationSyntax> methods = [];

        var methodName = $"Visit{classSymbol.Name.Replace("Syntax", string.Empty)}";

        var properties = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial());

        var args = properties.Select(property =>
        {
            var propertyType = ParseTypeName(property.Type.ToDisplayString());

            var childPropertyTypeNae = propertyType.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Last();

            string childPropertyName = property.Name;

            var accessNodeChild = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("node"),
                    Token(SyntaxKind.DotToken),
                    IdentifierName(childPropertyName));

            if (property.Name == "Kind")
            {
                return Argument(accessNodeChild);
            }

            var isList = propertyType.ToString().Contains("SyntaxList");

            var updateMethodNameStr = isList
                ? "VisitList" : $"Visit{childPropertyTypeNae.ToString().Replace("Syntax", string.Empty)}";

            NameSyntax updateMethodName = IdentifierName(updateMethodNameStr);

            if (isList)
            {
                var paramType2 = propertyType.DescendantNodes().OfType<GenericNameSyntax>().Last();

                updateMethodName = GenericName(updateMethodNameStr).WithTypeArgumentList(
                        TypeArgumentList(
                            SingletonSeparatedList(
                                paramType2.TypeArgumentList.Arguments.First())));
            }

            var updateInvocation = InvocationExpression(updateMethodName, ArgumentList([
                Argument(accessNodeChild)
            ]));

            var castingResult = CastExpression(propertyType, updateInvocation);

            return Argument(castingResult);
        }).ToList();

        var expr = InvocationExpression(
            ConditionalAccessExpression(IdentifierName("node"), MemberBindingExpression(IdentifierName("Update"))))
        .WithArgumentList(
            ArgumentList(
                SeparatedList(
                    args)));

        methods.Add(MethodDeclaration(
            NullableType(IdentifierName("SyntaxNode")),
                        Identifier(methodName))
                        .WithModifiers(
                            TokenList(
                                [
                                    Token(SyntaxKind.PublicKeyword),
                        Token(SyntaxKind.OverrideKeyword)]))
                                    .WithParameterList(
                                        ParameterList(
                                            SingletonSeparatedList(
                                                Parameter(
                                                    Identifier("node"))
                                                .WithType(
                                                    IdentifierName(className)))))
                                  .WithExpressionBody(ArrowExpressionClause(expr))
                                .WithSemicolonToken(
                                    Token(SyntaxKind.SemicolonToken)));


        // Generate the partial class
        var generatedClass = ClassDeclaration("SyntaxRewriter")
            .WithBaseList(BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        GenericName(
                            Identifier("SyntaxVisitor"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList<TypeSyntax>(
                                    NullableType(IdentifierName("SyntaxNode")))))))))
            .WithModifiers(TokenList(
                Token(SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }
}
