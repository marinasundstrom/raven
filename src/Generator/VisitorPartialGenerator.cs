using System.Reflection.Metadata;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public static class VisitorPartialGenerator
{
    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForVisitor(SourceProductionContext context, string namespaceNem, string nodeClassName, bool isInternal = false, string suffix = "Syntax", string resultType = "SyntaxNode")
    {
        List<MethodDeclarationSyntax> methods = [];

        string sv = nodeClassName;

        if (suffix == "Symbol")
        {
            sv = sv.Substring(1);
        }

        var methodName = $"Visit{sv.Replace(suffix, string.Empty)}";

        methods.Add(MethodDeclaration(
        PredefinedType(
            Token(SyntaxKind.VoidKeyword)),
            Identifier(methodName))
            .WithModifiers(
                TokenList(
                    [
                            Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.VirtualKeyword)]))
                            .WithParameterList(
                                ParameterList(
                                    SingletonSeparatedList<ParameterSyntax>(
                                        Parameter(
                                            Identifier("node"))
                                        .WithType(
                                            IdentifierName(nodeClassName)))))
                            .WithBody(
                                Block(
                                    SingletonList<StatementSyntax>(
                                        ExpressionStatement(
                                            InvocationExpression(
                                                IdentifierName("DefaultVisit"))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList(
                                                        Argument(
                                                            IdentifierName("node"))))))))));


        // Generate the partial class
        var generatedClass = ClassDeclaration($"{suffix}Visitor")
            .WithModifiers(TokenList(
                Token(isInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }

    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForGenericVisitor(SourceProductionContext context, string namespaceNem, string nodeClassName, bool isInternal = false, string suffix = "Syntax", string resultType = "SyntaxNode")
    {
        List<MethodDeclarationSyntax> methods = [];

        string sv = nodeClassName;

        if (suffix == "Symbol")
        {
            sv = sv.Substring(1);
        }

        var methodName = $"Visit{sv.Replace(suffix, string.Empty)}";

        methods.Add(MethodDeclaration(
            IdentifierName("TResult"),
            Identifier(methodName))
            .WithModifiers(
                TokenList(
                    [
                            Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.VirtualKeyword)]))
                            .WithParameterList(
                                ParameterList(
                                    SingletonSeparatedList(
                                        Parameter(
                                            Identifier("node"))
                                        .WithType(
                                            IdentifierName(nodeClassName)))))
                            .WithBody(
                                Block(
                                    SingletonList<StatementSyntax>(
                                        ReturnStatement(
                                            InvocationExpression(
                                                IdentifierName("DefaultVisit"))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            IdentifierName("node"))))))))));


        // Generate the partial class
        var generatedClass = ClassDeclaration($"{suffix}Visitor")
            .WithTypeParameterList(
                TypeParameterList(
                    SeparatedList<TypeParameterSyntax>([TypeParameter("TResult")])))
            .WithModifiers(TokenList(
                Token(isInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }

    public static ClassDeclarationSyntax GenerateVisitMethodForRewriter(SourceProductionContext context, INamedTypeSymbol? classSymbol, string suffix = "Syntax", string resultType = "SyntaxNode")
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        List<MethodDeclarationSyntax> methods = [];

        string sv = className;

        if (suffix == "Symbol")
        {
            sv = sv.Substring(1);
        }

        var methodName = $"Visit{sv.Replace(suffix, string.Empty)}";

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
            NullableType(IdentifierName(resultType)),
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
        var generatedClass = ClassDeclaration($"{suffix}Rewriter")
            .WithBaseList(BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        GenericName(
                            Identifier($"{suffix}Visitor"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList<TypeSyntax>(
                                    NullableType(IdentifierName(resultType)))))))))
            .WithModifiers(TokenList(
                Token(SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }
}
