using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Raven.Generators;

public static class GreenNodeGenerator
{
    public static string GenerateGreenNode(
        SyntaxNodeModel node,
        Dictionary<string, SyntaxNodeModel> nodesByName)
    {
        var name = node.Name + "Syntax";
        var baseName = MapType(node.Base, false);
        var isAbstract = node.Abstract;

        var allProperties = isAbstract
            ? node.Properties.ToList() // include abstract props in abstract classes
            : node.Properties.Where(p => !p.Abstract).ToList(); // exclude in concrete

        var slotIndex = 0;

        var ctorFields = allProperties.ToList();

        ConstructorDeclarationSyntax? ctor = null;

        var members = new List<MemberDeclarationSyntax>();

        if (!isAbstract)
        {
            var parameters = ctorFields.Select(p =>
                Parameter(Identifier(ToCamelCase(p.Name))).WithType(
                    IdentifierName(MapType(p.Type, p.Nullable)))).ToList();

            parameters.Add(Parameter(Identifier("diagnostics"))
                .WithType(NullableType(IdentifierName("IEnumerable<DiagnosticInfo>")))
                .WithDefault(EqualsValueClause(LiteralExpression(SyntaxKind.NullLiteralExpression))));

            if (node.ExplicitKind)
            {
                parameters.Insert(0, Parameter(Identifier("kind"))
                .WithType(IdentifierName("SyntaxKind")));
            }

            var slotArgs = ctorFields.Select(p =>
                (ExpressionSyntax)(p.Nullable
                    ? IdentifierName(ToCamelCase(p.Name))
                    : BinaryExpression(SyntaxKind.CoalesceExpression,
                        IdentifierName(ToCamelCase(p.Name)),
                        ThrowExpression(
                            ObjectCreationExpression(IdentifierName("ArgumentNullException"))
                            .WithArgumentList(ArgumentList(SingletonSeparatedList(
                                Argument(InvocationExpression(IdentifierName("nameof"))
                                    .WithArgumentList(ArgumentList(SingletonSeparatedList(
                                        Argument(IdentifierName(p.Name))))))))))))
                ).ToArray();

            ctor = ConstructorDeclaration(name)
                .AddModifiers(Token(SyntaxKind.PublicKeyword))
                .WithParameterList(ParameterList(SeparatedList(parameters)))
                .WithInitializer(ConstructorInitializer(SyntaxKind.BaseConstructorInitializer,
                    ArgumentList(SeparatedList(
                    [
                        Argument(node.ExplicitKind ? IdentifierName("kind")
                        : MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("SyntaxKind"), IdentifierName(node.Name))),
                        Argument(CollectionExpression(
                            SeparatedList<CollectionElementSyntax>(CreateExpressionElementList(slotArgs)))),
                        Argument(IdentifierName("diagnostics"))
                    ]))))
                    .WithBody(Block());

            members.Add(ctor);
        }

        // Add new constructor taking (SyntaxKind kind, GreenNode[] slots)
        var rawCtor = ConstructorDeclaration(name)
            .AddModifiers(Token(SyntaxKind.ProtectedKeyword))
            .WithParameterList(ParameterList(SeparatedList(
            [
            Parameter(Identifier("kind"))
                .WithType(IdentifierName("SyntaxKind")),
            Parameter(Identifier("slots"))
                .WithType(ArrayType(IdentifierName("GreenNode"))
                    .WithRankSpecifiers(SingletonList(ArrayRankSpecifier(
                        SingletonSeparatedList<ExpressionSyntax>(
                            OmittedArraySizeExpression()))))),
            Parameter(Identifier("diagnostics"))
                .WithType(NullableType(IdentifierName("IEnumerable<DiagnosticInfo>")))
                .WithDefault(EqualsValueClause(LiteralExpression(SyntaxKind.NullLiteralExpression)))
            ])))
            .WithInitializer(ConstructorInitializer(
                SyntaxKind.BaseConstructorInitializer,
                ArgumentList(SeparatedList(
                [
                Argument(IdentifierName("kind")),
                Argument(IdentifierName("slots")),
                Argument(IdentifierName("diagnostics"))
                ]))))
            .WithBody(Block());

        members.Add(rawCtor);

        foreach (var prop in allProperties)
        {
            var typeName = IdentifierName(MapType(prop.Type, prop.Nullable));

            if (isAbstract && !prop.Inherited && prop.Abstract)
            {
                members.Add(PropertyDeclaration(typeName, prop.Name)
                    .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.AbstractKeyword))
                    .WithAccessorList(
                        AccessorList([
                            AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))
                        ])
                    ));
                continue;
            }

            if (isAbstract && !prop.Inherited)
            {
                // Skip body in abstract class for non-abstract fields
                continue;
            }

            var modifiers = new List<SyntaxToken> { Token(SyntaxKind.PublicKeyword) };
            if (prop.Inherited)
            {
                modifiers.Add(Token(SyntaxKind.OverrideKeyword));
            }

            members.Add(PropertyDeclaration(typeName, prop.Name)
                .WithModifiers(TokenList(modifiers))
                .WithExpressionBody(ArrowExpressionClause(
                    InvocationExpression(
                        GenericName("GetSlot").WithTypeArgumentList(TypeArgumentList(
                            SingletonSeparatedList<TypeSyntax>(typeName))))
                    .WithArgumentList(ArgumentList(SingletonSeparatedList(
                        Argument(LiteralExpression(SyntaxKind.NumericLiteralExpression,
                            Literal(slotIndex++))))))))
                .WithSemicolonToken(Token(SyntaxKind.SemicolonToken)));
        }

        if (!isAbstract)
        {
            // Prepare update parameters and arguments
            var updateParams = node.Properties.Select(p =>
                Parameter(Identifier(ToCamelCase(p.Name)))
                    .WithType(ParseTypeName(MapType(p.Type, p.Nullable)))
            ).ToList();

            var updateArgs = node.Properties.Select(p =>
                Argument(IdentifierName(ToCamelCase(p.Name)))
            ).ToList();

            var updateMethod = MethodUpdate(
                name, // class name with "Syntax" suffix
                updateParams,
                updateArgs,
                node
            );

            members.AddRange(
            [
                GenerateAccept(name, node.Name),
                GenerateAcceptGeneric(name, node.Name),
                GenerateCreateRed(name, node.Name),
                GenerateWithUpdatedChildren(name),
                updateMethod
            ]);
        }

        var classDecl = ClassDeclaration(name)
            .WithModifiers(TokenList(Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.PartialKeyword))
                .InsertRange(1, isAbstract ? [Token(SyntaxKind.AbstractKeyword)] : Array.Empty<SyntaxToken>()))
            .WithBaseList(BaseList(SingletonSeparatedList<BaseTypeSyntax>(
                SimpleBaseType(IdentifierName(baseName)))))
            .AddMembers(members.ToArray());

        var ns = NamespaceDeclaration(ParseName("Raven.CodeAnalysis.Syntax.InternalSyntax"))
            .AddMembers(classDecl);

        var cu = CompilationUnit()
            .AddUsings(UsingDirective(ParseName("System")))
            .AddUsings(UsingDirective(ParseName("System.Collections.Generic")))
            .AddUsings(UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(ns);

        return cu.NormalizeWhitespace().ToFullString();
    }

    private static SyntaxNodeOrToken[] CreateExpressionElementList(ExpressionSyntax[] slotArgs)
    {
        var result = new List<SyntaxNodeOrToken>();
        for (int i = 0; i < slotArgs.Length; i++)
        {
            result.Add(ExpressionElement(slotArgs[i]));
            if (i < slotArgs.Length - 1)
            {
                result.Add(Token(SyntaxKind.CommaToken));
            }
        }

        return result.ToArray();
    }

    private static string MapType(string rawType, bool nullable)
    {
        var type = rawType switch
        {
            "Node" => "SyntaxNode",
            "Token" => "SyntaxToken",
            "TokenList" => "SyntaxList",
            var t when t.StartsWith("List<") => "SyntaxList",
            var t when t.StartsWith("SeparatedList<") => "SyntaxList",
            _ => rawType + "Syntax"
        };
        return nullable ? type + "?" : type;
    }

    private static MethodDeclarationSyntax GenerateAccept(string className, string nodeName)
    {
        return MethodDeclaration(
                PredefinedType(Token(SyntaxKind.VoidKeyword)),
                "Accept")
            .AddModifiers(Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddParameterListParameters(
                Parameter(Identifier("visitor"))
                    .WithType(IdentifierName("SyntaxVisitor")))
            .WithBody(Block(
                ExpressionStatement(
                    InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("visitor"),
                            IdentifierName($"Visit{nodeName}")))
                    .WithArgumentList(ArgumentList(
                        SingletonSeparatedList(
                            Argument(ThisExpression())))))));
    }

    private static MethodDeclarationSyntax GenerateAcceptGeneric(string className, string nodeName)
    {
        return MethodDeclaration(
                IdentifierName("TNode"),
                "Accept")
            .AddModifiers(Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddTypeParameterListParameters(TypeParameter("TNode"))
            .AddParameterListParameters(
                Parameter(Identifier("visitor"))
                    .WithType(GenericName("SyntaxVisitor")
                        .WithTypeArgumentList(TypeArgumentList(
                            SingletonSeparatedList<TypeSyntax>(IdentifierName("TNode"))))))
            .WithBody(Block(
                ReturnStatement(
                    InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("visitor"),
                            IdentifierName($"Visit{nodeName}")))
                    .WithArgumentList(ArgumentList(
                        SingletonSeparatedList(
                            Argument(ThisExpression())))))));
    }

    private static MethodDeclarationSyntax GenerateCreateRed(string className, string nodeName)
    {
        return MethodDeclaration(
                QualifiedName(
                    IdentifierName("Syntax"), IdentifierName("SyntaxNode")),
                "CreateRed")
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddParameterListParameters(
                Parameter(Identifier("parent"))
                    .WithType(NullableType(QualifiedName(
                                IdentifierName("Syntax"), IdentifierName("SyntaxNode")))),
                Parameter(Identifier("position"))
                    .WithType(PredefinedType(Token(SyntaxKind.IntKeyword))))
            .WithBody(Block(
                ReturnStatement(
                    ObjectCreationExpression(
                            QualifiedName(
                                IdentifierName("Syntax"),
                                IdentifierName(className)))
                        .WithArgumentList(ArgumentList(SeparatedList(
                        [
                        Argument (ThisExpression()),
                        Argument (IdentifierName("parent")),
                        Argument (IdentifierName("position"))
                        ]))))));
    }

    private static MethodDeclarationSyntax GenerateWithUpdatedChildren(string className)
    {
        return MethodDeclaration(
                IdentifierName(className),
                "WithUpdatedChildren")
            .AddModifiers(Token(SyntaxKind.ProtectedKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddParameterListParameters(
                Parameter(Identifier("newChildren"))
                    .WithType(ArrayType(IdentifierName("GreenNode"))
                        .WithRankSpecifiers(SingletonList(
                            ArrayRankSpecifier(
                                SingletonSeparatedList<ExpressionSyntax>(
                                    OmittedArraySizeExpression()))))))
            .WithBody(Block(
                ReturnStatement(
                    ObjectCreationExpression(IdentifierName(className))
                        .WithArgumentList(ArgumentList(SeparatedList(
                        [
                        Argument (IdentifierName("Kind")),
                        Argument (IdentifierName("newChildren"))
                        ]))))));
    }

    private static MethodDeclarationSyntax MethodUpdate(
        string className,
        List<ParameterSyntax> updateParams,
        List<ArgumentSyntax> updateArgs,
        SyntaxNodeModel node)
    {
        var conditions = new List<ExpressionSyntax>();
        var ctorArgs = new List<ArgumentSyntax>();
        var finalParams = new List<ParameterSyntax>();

        int offset = 0;

        if (node.ExplicitKind)
        {
            // Inject kind parameter first
            var kindParam = Parameter(Identifier("kind")).WithType(IdentifierName("SyntaxKind"));
            finalParams.Add(kindParam);
            ctorArgs.Add(Argument(IdentifierName("kind")));

            // Add kind != this.Kind check
            conditions.Add(BinaryExpression(
                SyntaxKind.NotEqualsExpression,
                IdentifierName("Kind"),
                IdentifierName("kind")));

            offset = 1;
        }

        // Add property parameters and constructor args
        for (int i = 0; i < node.Properties.Count; i++)
        {
            var prop = node.Properties[i];
            var param = updateParams[i];

            finalParams.Add(param);
        }

        ctorArgs.AddRange(updateArgs.Select(arg => arg));

        // Property inequality checks
        if (updateArgs.Count > 0)
        {
            conditions.AddRange(updateArgs
                .Zip(node.Properties, (arg, prop) =>
                    BinaryExpression(SyntaxKind.NotEqualsExpression, IdentifierName(prop.Name), arg.Expression)));
        }

        var condition = conditions.Count == 0
            ? LiteralExpression(SyntaxKind.FalseLiteralExpression)
            : conditions.Aggregate((a, b) => BinaryExpression(SyntaxKind.LogicalOrExpression, a, b));

        return MethodDeclaration(IdentifierName(className), "Update")
            .AddModifiers(Token(SyntaxKind.PublicKeyword))
            .WithParameterList(ParameterList(SeparatedList(finalParams)))
            .WithBody(Block(
                IfStatement(
                    condition,
                    Block(ReturnStatement(
                        ObjectCreationExpression(IdentifierName(className))
                            .WithArgumentList(ArgumentList(SeparatedList(ctorArgs)))))),
                ReturnStatement(ThisExpression())));
    }

    public static CompilationUnitSyntax GenerateFactoryMethod(SyntaxNodeModel node)
    {
        if (node.Abstract)
            return null!; // skip abstract nodes

        var methodName = node.Name;
        var typeName = node.Name + "Syntax";
        var parameters = new List<ParameterSyntax>();
        var arguments = new List<ArgumentSyntax>();

        if (node.ExplicitKind)
        {
            parameters.Add(
                Parameter(Identifier("kind"))
                    .WithType(IdentifierName("SyntaxKind")));

            arguments.Add(Argument(IdentifierName("kind")));
        }

        foreach (var prop in node.Properties)
        {
            parameters.Add(
                Parameter(Identifier(ToCamelCase(prop.Name)))
                    .WithType(IdentifierName(MapType(prop.Type, prop.Nullable))));

            arguments.Add(Argument(IdentifierName(ToCamelCase(prop.Name))));
        }

        // diagnostics (optional)
        parameters.Add(
            Parameter(Identifier("diagnostics"))
                .WithType(NullableType(IdentifierName("IEnumerable<DiagnosticInfo>")))
                .WithDefault(EqualsValueClause(
                    LiteralExpression(SyntaxKind.NullLiteralExpression))));

        arguments.Add(Argument(IdentifierName("diagnostics")));

        var method = MethodDeclaration(IdentifierName(typeName), methodName)
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.StaticKeyword))
            .WithParameterList(ParameterList(SeparatedList(parameters)))
            .WithExpressionBody(ArrowExpressionClause(
                ObjectCreationExpression(IdentifierName(typeName))
                    .WithArgumentList(ArgumentList(SeparatedList(arguments)))))
            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken));

        var factoryClass = ClassDeclaration("SyntaxFactory")
            .AddModifiers(Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.StaticKeyword), Token(SyntaxKind.PartialKeyword))
            .AddMembers(method);

        return CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("System.Collections.Generic")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(NamespaceDeclaration(ParseName("Raven.CodeAnalysis.Syntax.InternalSyntax")).AddMembers(factoryClass))
            .NormalizeWhitespace();
    }

    private static string ToCamelCase(string name)
    {
        return string.IsNullOrEmpty(name) || char.IsLower(name[0])
            ? name
            : char.ToLowerInvariant(name[0]) + name.Substring(1);
    }
}