namespace Generator;

using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        //Debugger.Launch();

        // Identify all partial classes that inherit (directly or indirectly) from SyntaxNode
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialPartialClass,
                transform: GetClassSymbol)
            .Where(symbol => symbol is not null && InheritsFromSyntaxNode(symbol));

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, GeneratePartialClass);
    }

    private static bool IsPotentialPartialClass(SyntaxNode node, CancellationToken cancellationToken)
    {
        if (node is ClassDeclarationSyntax classDecl)
        {
            return !classDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword)) &&
             classDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.PartialKeyword));
        }
        return false;
    }

    private static INamedTypeSymbol? GetClassSymbol(GeneratorSyntaxContext context, CancellationToken cancellationToken)
    {
        if (context.Node is ClassDeclarationSyntax classDecl &&
            context.SemanticModel.GetDeclaredSymbol(classDecl, cancellationToken) is INamedTypeSymbol classSymbol)
        {
            return classSymbol;
        }

        return null;
    }

    private static bool InheritsFromSyntaxNode(ITypeSymbol classSymbol)
    {
        // Traverse the inheritance hierarchy to check if the type derives from SyntaxNode
        var baseType = classSymbol.BaseType;
        while (baseType != null)
        {
            if (baseType.Name == "SyntaxNode") // && baseType.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }

    private static void GeneratePartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return;

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

#if !DEBUG
        if (namespaceName != "Raven.CodeAnalysis.Syntax")
            return;
#endif

        var accessModifier = Token(SyntaxKind.PublicKeyword);

        var members = GenerateFields(classSymbol)
                .Concat(ImplementPartialProperties(classSymbol)).ToList();

        var f = GenerateGetNodeSlot(classSymbol);

        if (f != null)
        {
            members.Add(f);
        }

        // Generate the partial class
        var generatedClass = ClassDeclaration(className)
            .WithModifiers(TokenList(accessModifier, Token(SyntaxKind.PartialKeyword)))
            .AddMembers(members.ToArray());

        // Wrap it in a namespace
        var namespaceDeclaration = NamespaceDeclaration(ParseName(namespaceName))
            .AddMembers(generatedClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
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
                                                        IdentifierName("Green"),
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
                        new[]{
                            Token(SyntaxKind.InternalKeyword),
                            Token(SyntaxKind.OverrideKeyword)}))
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
            .Where(property => property.IsPartial() && !IsSyntaxToken(property.Type))
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
                    return GenerateSyntaxNodeProperty(index++, propertyType, propertyName);
                }
                else if (IsSyntaxToken(property.Type))
                {
                    return GenerateSyntaxTokenProperty(index++, property.ContainingType, propertyType, propertyName);
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
        return typeSymbol.Name == "SeparatedSyntaxListSyntaxList"; // && typeSymbol.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis";
    }

    private static PropertyDeclarationSyntax GenerateSyntaxNodeProperty(int index, TypeSyntax propertyType, SyntaxToken propertyName)
    {
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
                    .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword))
                    .WithExpressionBody(body).WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));
        ;
    }

    private static PropertyDeclarationSyntax GenerateSyntaxTokenProperty(int index, INamedTypeSymbol type, TypeSyntax propertyType, SyntaxToken propertyName)
    {
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
                                            ThisExpression())}))));

        return PropertyDeclaration(propertyType, propertyName)
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword))
            .WithExpressionBody(body).WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken));
    }


    private static PropertyDeclarationSyntax GenerateSyntaxListProperty(int index, INamedTypeSymbol type, TypeSyntax propertyType, SyntaxToken propertyName)
    {
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
                                                            IdentifierName("SyntaxList")))),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    ThisExpression())})))))))));
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
