﻿namespace Generator;

using System;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GeneratePartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

#if !DEBUG
        if (namespaceName != "Raven.CodeAnalysis.Syntax")
            return;
#endif

        var accessModifier = Token(SyntaxKind.PublicKeyword);

        var members = GenerateFields(classSymbol).ToList();

        members.AddRange(
            ImplementPartialProperties(classSymbol));

        var f = GenerateGetNodeSlot(classSymbol);

        if (f != null)
        {
            members.Add(f);
        }

        members.AddRange(
            GenerateUpdateMethod(classSymbol));

        members.AddRange(
            GenerateWithMethods(classSymbol));

        members.AddRange(
            GenerateAcceptMethods(classSymbol));

        // Generate the partial class
        var generatedClass = ClassDeclaration(className)
            .WithModifiers(TokenList(accessModifier, Token(SyntaxKind.PartialKeyword)))
            .AddMembers(members.ToArray());

        return generatedClass;
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateAcceptMethods(INamedTypeSymbol classSymbol)
    {
        var typeName = ParseTypeName(classSymbol.Name);

        var methodName = $"Visit{classSymbol.Name.Replace("Syntax", string.Empty)}";

        IEnumerable<MethodDeclarationSyntax> methods = [
            MethodDeclaration(
                PredefinedType(
                    Token(SyntaxKind.VoidKeyword)),
                Identifier("Accept"))
            .WithModifiers(
                TokenList(
                    [
                        Token(SyntaxKind.PublicKeyword),
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
                        Token(SyntaxKind.PublicKeyword),
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

        return methods;
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateWithMethods(INamedTypeSymbol classSymbol)
    {
        var typeName = ParseTypeName(classSymbol.Name);

        var parameters = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial());

        var withMethodDeclarations = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial())
            .Select(property =>
            {
                var propertyType = ParseTypeName(property.Type.ToDisplayString());
                var propertyName = Identifier(property.Name);

                var paramDef = parameters.Select(param =>
                {
                    var paramType = ParseTypeName(param.Type.ToDisplayString());

                    string paramName = param.Name == property.Name ? FixName(param) : param.Name;

                    return Parameter(Identifier(paramName))
                                    .WithType(paramType);
                }).ToList();

                var expr = InvocationExpression(
                    MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, ThisExpression(), IdentifierName("Update")))
                .WithArgumentList(
                    ArgumentList(
                        SeparatedList<ArgumentSyntax>(
                            paramDef.Select(p =>
                                Argument(IdentifierName(p.Identifier))))));

                return MethodDeclaration(
                   ParseTypeName(classSymbol.Name), $"With{propertyName}")
                .WithModifiers([Token(SyntaxKind.PublicKeyword)])
                .WithParameterList(
                    ParameterList(
                        SeparatedList([
                            Parameter(Identifier(FixName(property)))
                            .WithType(propertyType)
                ])))
                .WithExpressionBody(ArrowExpressionClause(expr))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));
            });

        return withMethodDeclarations;
    }

    private static string FixName(IPropertySymbol property)
    {
        var name = property.Name.ToCamelCase();

        var x = SyntaxFacts.IsKeywordKind(SyntaxFacts.GetKeywordKind(name));
        if (x)
        {
            return $"@{name}";
        }
        return name;
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateUpdateMethod(INamedTypeSymbol classSymbol)
    {
        var parameters = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial());

        var paramDef = parameters.Select(property =>
        {
            var propertyType = ParseTypeName(property.Type.ToDisplayString());
            var propertyName = Identifier(property.Name);

            return Parameter(Identifier(FixName(property)))
                            .WithType(propertyType);
        }).ToList();

        var typeName = ParseTypeName(classSymbol.Name);

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
                )))
                .WithExpressionBody(ArrowExpressionClause(expr))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));

        return [updateMethodDeclaration];
    }

    private static MemberDeclarationSyntax? GenerateGetNodeSlot(INamedTypeSymbol classSymbol)
    {
        int index = 0;

        List<SwitchExpressionArmSyntax> arms = new List<SwitchExpressionArmSyntax>();

        var properties = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial());

        if (!properties.Any(property => IsSyntaxNodeOrDerivative(property.Type)))
            return null;

        foreach (var property in properties)
        {
            if (!IsSyntaxNodeOrDerivative(property.Type))
            {
                index++;
                continue;
            }

            arms.Add(SwitchExpressionArm(
                ConstantPattern(
                    LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        Literal(index))),
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        ThisExpression(),
                        IdentifierName("GetRed")))
                .WithArgumentList(
                    ArgumentList(
                        SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]{
                                Argument(
                                    IdentifierName("_" + property.Name.ToCamelCase()))
                                .WithRefOrOutKeyword(
                                    Token(SyntaxKind.RefKeyword)),
                                Token(SyntaxKind.CommaToken),
                                Argument(
                                    LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        Literal(index)))})))));
            index++;
        }

        arms.Add(SwitchExpressionArm(
            DiscardPattern(),
                ThrowExpression(
                        ObjectCreationExpression(
                            IdentifierName("Exception"))
                        .WithArgumentList(
                            ArgumentList()))).WithTrailingTrivia(CarriageReturnLineFeed));

        return MethodDeclaration(
                    NullableType(
                        IdentifierName("SyntaxNode")),
                    Identifier("GetNodeSlot"))
                .WithModifiers(
                    TokenList(
                        [
                            Token(SyntaxKind.InternalKeyword),
                            Token(SyntaxKind.OverrideKeyword)]))
                .WithParameterList(
                    ParameterList(
                        SingletonSeparatedList<ParameterSyntax>(
                            Parameter(
                                Identifier("index"))
                            .WithType(
                                PredefinedType(
                                    Token(SyntaxKind.IntKeyword))))))
                .WithBody(
                    Block(
                        SingletonList<StatementSyntax>(
                            ReturnStatement(
                                SwitchExpression(
                                    IdentifierName("index"))
                                .WithArms(
                                    SeparatedList<SwitchExpressionArmSyntax>(
                                        arms.JoinWithSeparator(Token(SyntaxKind.CommaToken))
                                        )
                                    )
                                )
                            )
                        )
                    );
    }

    private static MemberDeclarationSyntax[] GenerateFields(INamedTypeSymbol classSymbol)
    {
        // Identify all partial properties in the class
        var fieldDeclarations = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial() && !IsSyntaxToken(property.Type) && !IsSyntaxList(property.Type))
            .Select(property =>
            {
                var propertyType = ParseTypeName(property.Type.ToDisplayString());
                var propertyName = Identifier(property.Name);

                return FieldDeclaration(
                        VariableDeclaration(propertyType)
                        .WithVariables(
                            SingletonSeparatedList(
                                VariableDeclarator(
                                    Identifier("_" + property.Name.ToCamelCase())))))
                        .WithModifiers(TokenList(Token(SyntaxKind.InternalKeyword)));
            })
            .ToArray();

        if (fieldDeclarations.Any())
        {
            var last = fieldDeclarations[fieldDeclarations.Length - 1];

            var newTrailingTrivia = last.GetTrailingTrivia()
                .Add(CarriageReturnLineFeed) // Line break
                .Add(CarriageReturnLineFeed);

            fieldDeclarations[fieldDeclarations.Length - 1] = last.WithTrailingTrivia(newTrailingTrivia);
        }

        return fieldDeclarations;
    }

    private static MemberDeclarationSyntax[] ImplementPartialProperties(INamedTypeSymbol classSymbol)
    {
        int index = 0;

        // Identify all partial properties in the class
        var partialProperties = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial())
            .Select(property =>
            {
                var propertyType = ParseTypeName(property.Type.ToDisplayString());
                var propertyName = Identifier(property.Name);

                // Generate properties based on their type
                if (IsSyntaxNodeOrDerivative(property.Type))
                {
                    return GenerateSyntaxNodeProperty(index++, property, propertyType, propertyName);
                }
                else if (IsSyntaxToken(property.Type))
                {
                    return GenerateSyntaxTokenProperty(index++, property.ContainingType, property, propertyType, propertyName);
                }
                else if (IsSyntaxList(property.Type) || IsSeparatedSyntaxList(property.Type))
                {
                    return GenerateSyntaxListProperty(index++, property.ContainingType, propertyType, propertyName);
                }

                return GenerateDefaultProperty(propertyType, propertyName);
            })
            .ToArray();

        return partialProperties;
    }

    private static bool IsSyntaxNodeOrDerivative(ITypeSymbol typeSymbol)
    {
        if (typeSymbol.Name == "SyntaxNode")
        {
            return true;
        }
        return InheritsFromSyntaxNode(typeSymbol);
    }

    private static bool IsSyntaxToken(ITypeSymbol typeSymbol)
    {
        return typeSymbol.Name == "SyntaxToken"; // && typeSymbol.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis";
    }

    private static bool IsSyntaxList(ITypeSymbol typeSymbol)
    {
        return typeSymbol.Name == "SyntaxList"; // && typeSymbol.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis";
    }

    private static bool IsSeparatedSyntaxList(ITypeSymbol typeSymbol)
    {
        return typeSymbol.Name == "SeparatedSyntaxList"; // && typeSymbol.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis";
    }

    private static PropertyDeclarationSyntax GenerateSyntaxNodeProperty(int index, IPropertySymbol property, TypeSyntax propertyType, SyntaxToken propertyName)
    {
        List<SyntaxToken> modifiers = [Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword)];

        if (property.IsOverride)
        {
            modifiers.Insert(1, Token(SyntaxKind.OverrideKeyword));
        }

        var body = ArrowExpressionClause(
                        CastExpression(
                            propertyType,
                            InvocationExpression(
                                IdentifierName("GetNodeSlot"))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList<ArgumentSyntax>(
                                        Argument(
                                            LiteralExpression(
                                                SyntaxKind.NumericLiteralExpression,
                                                Literal(index))))))));

        return PropertyDeclaration(propertyType, propertyName)
                    .AddModifiers(modifiers.ToArray())
                    .WithExpressionBody(body).WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));
        ;
    }

    private static PropertyDeclarationSyntax GenerateSyntaxTokenProperty(int index, INamedTypeSymbol type, IPropertySymbol property, TypeSyntax propertyType, SyntaxToken propertyName)
    {
        List<SyntaxToken> modifiers = [Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword)];

        if (property.IsOverride)
        {
            modifiers.Insert(1, Token(SyntaxKind.OverrideKeyword));
        }

        var body = ArrowExpressionClause(
                        ObjectCreationExpression(
                            IdentifierName("SyntaxToken"))
                        .WithArgumentList(
                            ArgumentList(
                                SeparatedList<ArgumentSyntax>(
                                    new SyntaxNodeOrToken[]{
                                        Argument(
                                            BinaryExpression(
                                                SyntaxKind.AsExpression,
                                                InvocationExpression(
                                                    MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        IdentifierName("Green"),
                                                        IdentifierName("GetSlot")))
                                                .WithArgumentList(
                                                    ArgumentList(
                                                        SingletonSeparatedList<ArgumentSyntax>(
                                                            Argument(
                                                                LiteralExpression(
                                                                    SyntaxKind.NumericLiteralExpression,
                                                                    Literal(index)))))),
                                                QualifiedName(
                                                    QualifiedName(
                                                        QualifiedName(
                                                            QualifiedName(
                                                                IdentifierName("Raven"),
                                                                IdentifierName("CodeAnalysis")),
                                                            IdentifierName("Syntax")),
                                                        IdentifierName("InternalSyntax")),
                                                    IdentifierName("SyntaxToken")))),
                                        Token(SyntaxKind.CommaToken),
                                        Argument(
                                            ThisExpression()),
                                        Token(SyntaxKind.CommaToken),
                                        Argument(
                                            BinaryExpression(
                                                SyntaxKind.AddExpression,
                                                IdentifierName("Position"),
                                                InvocationExpression(
                                                    MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        IdentifierName("Green"),
                                                        IdentifierName("GetChildStartPosition")))
                                                .WithArgumentList(
                                                    ArgumentList(
                                                        SingletonSeparatedList<ArgumentSyntax>(
                                                            Argument(LiteralExpression(
                                                                SyntaxKind.NumericLiteralExpression,
                                                                Literal(index))))))))
                                        }))));

        return PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(modifiers.ToArray())
            .WithExpressionBody(body).WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));
    }

    private static PropertyDeclarationSyntax GenerateSyntaxListProperty(int index, INamedTypeSymbol type, TypeSyntax propertyType, SyntaxToken propertyName)
    {
        var targetGreenNodeType = propertyType.ToString().Contains("Separated") ?
            "SeparatedSyntaxList" : "SyntaxList";

        return PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword))
            .WithAccessorList(
                    AccessorList(
                        SingletonList<AccessorDeclarationSyntax>(
                    AccessorDeclaration(
                        SyntaxKind.GetAccessorDeclaration)
                    .WithBody(
                        Block(
                            LocalDeclarationStatement(
                                VariableDeclaration(
                                    IdentifierName("GreenNode"))
                                .WithVariables(
                                    SingletonSeparatedList<VariableDeclaratorSyntax>(
                                        VariableDeclarator(
                                            Identifier("green"))
                                        .WithInitializer(
                                            EqualsValueClause(
                                                InvocationExpression(
                                                    MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        IdentifierName("Green"),
                                                        IdentifierName("GetSlot")))
                                                .WithArgumentList(
                                                    ArgumentList(
                                                        SingletonSeparatedList<ArgumentSyntax>(
                                                            Argument(
                                                                LiteralExpression(
                                                                    SyntaxKind.NumericLiteralExpression,
                                                                    Literal(index))))))))))),
                            IfStatement(
                                IsPatternExpression(
                                    IdentifierName("green"),
                                    ConstantPattern(
                                        LiteralExpression(
                                            SyntaxKind.NullLiteralExpression))),
                                Block(
                                    SingletonList<StatementSyntax>(
                                        ReturnStatement(
                                            DefaultExpression(propertyType))))),
                            ReturnStatement(
                                ObjectCreationExpression(propertyType)
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    BinaryExpression(
                                                        SyntaxKind.AsExpression,
                                                        IdentifierName("green"),
                                                        QualifiedName(
                                                            QualifiedName(
                                                                QualifiedName(
                                                                    QualifiedName(
                                                                        IdentifierName("Raven"),
                                                                        IdentifierName("CodeAnalysis")),
                                                                    IdentifierName("Syntax")),
                                                                IdentifierName("InternalSyntax")),
                                                            IdentifierName(targetGreenNodeType)))),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    ThisExpression()),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    BinaryExpression(
                                                        SyntaxKind.AddExpression,
                                                        IdentifierName("Position"),
                                                        InvocationExpression(
                                                            MemberAccessExpression(
                                                                SyntaxKind.SimpleMemberAccessExpression,
                                                                IdentifierName("Green"),
                                                                IdentifierName("GetChildStartPosition")))
                                                        .WithArgumentList(
                                                            ArgumentList(
                                                                SingletonSeparatedList<ArgumentSyntax>(
                                                                    Argument(LiteralExpression(
                                                                        SyntaxKind.NumericLiteralExpression,
                                                                        Literal(index))))))))
                                                })))))))));
    }

    private static PropertyDeclarationSyntax GenerateDefaultProperty(TypeSyntax propertyType, SyntaxToken propertyName)
    {
        return PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword))
            .AddAccessorListAccessors(
                AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                    .WithBody(Block(
                                    SingletonList<StatementSyntax>(
                                        ReturnStatement(
                                            LiteralExpression(
                                                SyntaxKind.NullLiteralExpression))))));
    }
}