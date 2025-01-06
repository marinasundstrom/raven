using System.Reflection.Metadata;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public static class VisitorPartialGenerator
{
    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForVisitor(SourceProductionContext context, string namespaceNem, string nodeClassName, bool isInternal = false)
    {
        List<MethodDeclarationSyntax> methods = [];

        var methodName = $"Visit{nodeClassName.Replace("Syntax", string.Empty)}";

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
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            IdentifierName("node"))))))))));


        // Generate the partial class
        var generatedClass = ClassDeclaration("SyntaxVisitor")
            .WithModifiers(TokenList(
                Token(isInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }

    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForGenericVisitor(SourceProductionContext context, string namespaceNem, string nodeClassName, bool isInternal = false)
    {
        List<MethodDeclarationSyntax> methods = [];

        var methodName = $"Visit{nodeClassName.Replace("Syntax", string.Empty)}";

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
                                    SingletonSeparatedList<ParameterSyntax>(
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
        var generatedClass = ClassDeclaration("SyntaxVisitor")
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

    public static ClassDeclarationSyntax GenerateVisitMethodForRewriter(SourceProductionContext context, INamedTypeSymbol? classSymbol)
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

public class PropOrParamType(string name, string type)
{
    public string Name { get; } = name;
    public string Type { get; } = type;
}


public static class UpdateMethodGenerator
{
    private static string FixName(PropOrParamType property)
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

            return Parameter(Identifier(FixName(property)))
                            .WithType(propertyType);
        }).ToList();

        var typeName = ParseTypeName(className);

        ExpressionSyntax condition = null!;

        foreach (var p in parameters)
        {
            var expr1 = BinaryExpression(SyntaxKind.NotEqualsExpression,
                IdentifierName(p.Name), IdentifierName(FixName(p)));

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