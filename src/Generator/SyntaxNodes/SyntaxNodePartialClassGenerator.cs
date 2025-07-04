namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public class SyntaxNodePartialClassGenerator
{
    public static ClassDeclarationSyntax GeneratePartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

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

    private static IEnumerable<MemberDeclarationSyntax> GenerateAcceptMethods(INamedTypeSymbol classSymbol)
    {
        return AcceptMethodGenerator.GenerateAcceptMethods(new AcceptMethodGeneratorOptions(classSymbol.Name, nodeBaseClass: "SyntaxNode"));
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

                    string paramName = param.Name == property.Name ? HelperExtensions.FixIdentifier(param.Name) : param.Name;

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
                            Parameter(Identifier(HelperExtensions.FixIdentifier(property.Name)))
                            .WithType(propertyType)
                ])))
                .WithExpressionBody(ArrowExpressionClause(expr))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));
            });

        return withMethodDeclarations;
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateUpdateMethod(INamedTypeSymbol classSymbol)
    {
        var parameters = classSymbol.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(property => property.IsPartial())
            .Select(x => new PropOrParamType(x.Name, x.Type.ToDisplayString()));

        return [UpdateMethodGenerator.GenerateUpdateMethod(classSymbol.Name, parameters)];
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
            if (property.Name == "Kind")
            {
                continue;
            }

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
                else if (IsSyntaxList(property.Type) || IsSeparatedSyntaxList(property.Type) || IsSyntaxTokenList(property.Type))
                {
                    return GenerateSyntaxListProperty(index++, property, property.ContainingType, propertyType, propertyName);
                }

                return GenerateDefaultProperty(property, propertyType, propertyName);
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
        return typeSymbol.InheritsFromSyntaxNode();
    }

    private static bool IsSyntaxToken(ITypeSymbol typeSymbol)
    {
        var unwrapped = UnwrapNullable(typeSymbol);
        return unwrapped.Name == "SyntaxToken";
    }

    private static ITypeSymbol UnwrapNullable(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is INamedTypeSymbol namedType &&
            namedType.IsGenericType &&
            namedType.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T)
        {
            return namedType.TypeArguments[0];
        }

        return typeSymbol;
    }

    private static bool IsSyntaxList(ITypeSymbol typeSymbol)
    {
        return typeSymbol.Name == "SyntaxList"; // && typeSymbol.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis";
    }

    private static bool IsSyntaxTokenList(ITypeSymbol typeSymbol)
    {
        return typeSymbol.Name == "SyntaxTokenList"; // && typeSymbol.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis";
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
        var modifiers = new List<SyntaxToken> { Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword) };

        if (property.IsOverride)
            modifiers.Insert(1, Token(SyntaxKind.OverrideKeyword));

        var greenSlotAccess = InvocationExpression(
            MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                IdentifierName("Green"),
                IdentifierName("GetSlot")))
            .WithArgumentList(
                ArgumentList(
                    SingletonSeparatedList(
                        Argument(
                            LiteralExpression(
                                SyntaxKind.NumericLiteralExpression,
                                Literal(index))))));

        var greenTypeName = ParseTypeName("Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken");

        bool isNullable = propertyType is NullableTypeSyntax;

        if (!isNullable)
        {
            // Expression-bodied non-nullable property
            return PropertyDeclaration(propertyType, propertyName)
                .AddModifiers(modifiers.ToArray())
                .WithExpressionBody(
                    ArrowExpressionClause(
                        ObjectCreationExpression(IdentifierName("SyntaxToken"))
                        .WithArgumentList(
                            ArgumentList(
                                SeparatedList(new[]
                                {
                                Argument(
                                    BinaryExpression(SyntaxKind.AsExpression, greenSlotAccess, greenTypeName)),
                                Argument(ThisExpression()),
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
                                                SingletonSeparatedList(
                                                    Argument(
                                                        LiteralExpression(
                                                            SyntaxKind.NumericLiteralExpression,
                                                            Literal(index))))))))

                                })))))
                .WithSemicolonToken(Token(SyntaxKind.SemicolonToken));
        }

        // Full-body nullable property
        return PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(modifiers.ToArray())
            .WithAccessorList(
                AccessorList(
                    SingletonList(
                        AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                        .WithBody(
                            Block(
                                SingletonList<StatementSyntax>(
                                    ReturnStatement(
                                        ConditionalExpression(
                                            BinaryExpression(
                                                SyntaxKind.EqualsExpression,
                                                greenSlotAccess,
                                                LiteralExpression(SyntaxKind.NullLiteralExpression)),
                                            LiteralExpression(SyntaxKind.NullLiteralExpression),
                                            ObjectCreationExpression(IdentifierName("SyntaxToken"))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SeparatedList(new[]
                                                    {
                                                    Argument(
                                                        BinaryExpression(
                                                            SyntaxKind.AsExpression,
                                                            greenSlotAccess,
                                                            greenTypeName)),
                                                    Argument(ThisExpression()),
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
                                                                    SingletonSeparatedList(
                                                                        Argument(
                                                                            LiteralExpression(
                                                                                SyntaxKind.NumericLiteralExpression,
                                                                                Literal(index))))))))

                                                    })))))))))));
    }

    private static PropertyDeclarationSyntax GenerateSyntaxListProperty(int index, IPropertySymbol property, INamedTypeSymbol type, TypeSyntax propertyType, SyntaxToken propertyName)
    {
        var targetGreenNodeType = "SyntaxList";

        List<SyntaxToken> modifiers = [Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword)];

        if (property.IsOverride)
        {
            modifiers.Insert(1, Token(SyntaxKind.OverrideKeyword));
        }

        return PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(modifiers.ToArray())
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

    private static PropertyDeclarationSyntax GenerateDefaultProperty(IPropertySymbol property, TypeSyntax propertyType, SyntaxToken propertyName)
    {
        List<SyntaxToken> modifiers = [Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword)];

        if (property.IsOverride)
        {
            modifiers.Insert(1, Token(SyntaxKind.OverrideKeyword));
        }

        var prop = PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(modifiers.ToArray());

        if (property.Name == "Kind")
        {
            return prop = prop.WithExpressionBody(
                    ArrowExpressionClause(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("Green"),
                            IdentifierName("Kind"))))
                .WithSemicolonToken(
                    Token(SyntaxKind.SemicolonToken));
        }

        /*
        prop = prop.WithAccessorList(
                    AccessorList(
                        List(
                            [
                                AccessorDeclaration(
                                    SyntaxKind.GetAccessorDeclaration)
                                .WithSemicolonToken(
                                    Token(SyntaxKind.SemicolonToken)),
                                AccessorDeclaration(
                                    SyntaxKind.SetAccessorDeclaration)
                                .WithModifiers(
                                    TokenList(
                                        Token(SyntaxKind.PrivateKeyword)))
                                .WithSemicolonToken(
                                    Token(SyntaxKind.SemicolonToken))])));
        */

        return prop.AddAccessorListAccessors(
                AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                    .WithBody(Block(
                                    SingletonList<StatementSyntax>(
                                        ReturnStatement(
                                            LiteralExpression(
                                                SyntaxKind.NullLiteralExpression))))));
    }
}