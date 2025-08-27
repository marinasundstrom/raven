using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NodesShared;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Raven.Generators;

public static class RedNodeGenerator
{
    public static CompilationUnitSyntax GenerateRedNode(SyntaxNodeModel node)
    {
        var className = node.Name + "Syntax";
        var baseType = IdentifierName(MapRedType(node.Inherits, false));
        var isSealed = !node.IsAbstract;
        var greenType = ParseTypeName($"InternalSyntax.{className}");
        var slotIndex = 0;

        var ctorParams = new List<ParameterSyntax>();
        var greenArgs = new List<ArgumentSyntax>();
        var updateParams = new List<ParameterSyntax>();
        var updateArgs = new List<ArgumentSyntax>();
        var withMethods = new List<MethodDeclarationSyntax>();
        var properties = new List<MemberDeclarationSyntax>();
        var backingFields = new List<FieldDeclarationSyntax>();
        var getNodeSwitchArms = new List<SwitchExpressionArmSyntax>();

        if (node.HasExplicitKind)
        {
            ctorParams.Add(Parameter(Identifier("kind")).WithType(IdentifierName("SyntaxKind")));
            greenArgs.Add(Argument(IdentifierName("kind")));
        }

        foreach (var prop in node.Slots)
        {
            if (node.IsAbstract && prop.IsAbstract && !prop.IsInherited)
            {
                // Declare abstract property in abstract base class
                properties.Add(PropertyDeclaration(ParseTypeName(MapRedType(prop.FullTypeName, prop.IsNullable)), prop.Name)
                    .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.AbstractKeyword))
                    .WithAccessorList(AccessorList(SingletonList(
                        AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))))));
                continue;
            }

            if (node.IsAbstract && !prop.IsInherited)
                continue;

            var camel = ToCamelCase(prop.Name);
            var typeName = MapRedType(prop.FullTypeName, false);
            var greenSlot = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(slotIndex));
            var isList = IsListType(prop.FullTypeName);
            var isTokenList = prop.FullTypeName == "TokenList";
            var isToken = prop.FullTypeName == "Token" || isTokenList;
            var isNullable = prop.IsNullable;

            var ctorType = ParseTypeName(typeName + (isNullable ? "?" : ""));
            ctorParams.Add(Parameter(Identifier(camel)).WithType(ctorType));
            updateParams.Add(Parameter(Identifier(camel)).WithType(ctorType));

            ExpressionSyntax greenExpr = GetGreenCast(camel, prop.FullTypeName, isToken, isNullable);

            greenArgs.Add(Argument(greenExpr));
            updateArgs.Add(Argument(IdentifierName(camel)));

            var modifiers = new List<SyntaxToken> { Token(SyntaxKind.PublicKeyword) };
            if (prop.IsInherited) modifiers.Add(Token(SyntaxKind.OverrideKeyword));

            ExpressionSyntax? getExpr = null;
            BlockSyntax? getBlock = null;

            if (isTokenList)
            {
                getExpr = ObjectCreationExpression(IdentifierName("SyntaxTokenList"))
                    .WithArgumentList(ArgumentList(SeparatedList(new[]
                    {
            Argument(CastExpression(IdentifierName("InternalSyntax.SyntaxList"),
                InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("Green"), IdentifierName("GetSlot")))
                .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot)))))),
            Argument(ThisExpression()),
            Argument(BinaryExpression(SyntaxKind.AddExpression, IdentifierName("Position"),
                InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("Green"), IdentifierName("GetChildStartPosition")))
                .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot))))))
                    })));
            }
            else if (isToken)
            {
                getExpr = ObjectCreationExpression(IdentifierName("SyntaxToken"))
                    .WithArgumentList(ArgumentList(SeparatedList(new[]
                    {
                        Argument(CastExpression(IdentifierName("InternalSyntax.SyntaxToken"),
                            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName("Green"), IdentifierName("GetSlot"))).WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot)))))),
                        Argument(ThisExpression()),
                        Argument(BinaryExpression(SyntaxKind.AddExpression, IdentifierName("Position"),
                            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName("Green"), IdentifierName("GetChildStartPosition"))).WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot))))))
                    })));
                if (isNullable)
                {
                    getExpr = ConditionalExpression(
                        BinaryExpression(SyntaxKind.EqualsExpression,
                            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName("Green"), IdentifierName("GetSlot"))).WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot)))),
                            LiteralExpression(SyntaxKind.NullLiteralExpression)),
                        LiteralExpression(SyntaxKind.NullLiteralExpression),
                        getExpr);
                }
            }
            else if (isList)
            {
                var backing = "_" + camel;
                var redFieldName = backing;
                var slotIndexLiteral = greenSlot;

                // 1. Backing field
                backingFields.Add(
                    FieldDeclaration(
                        VariableDeclaration(ParseTypeName(typeName))
                            .WithVariables(SingletonSeparatedList(VariableDeclarator(Identifier(redFieldName)))))
                    .AddModifiers(Token(SyntaxKind.InternalKeyword)));

                // 2. Property getter expression (lazy init)
                getBlock = Block(
                    IfStatement(
                        BinaryExpression(SyntaxKind.EqualsExpression,
                            IdentifierName(redFieldName),
                            DefaultExpression(ParseTypeName(typeName))),
                        ExpressionStatement(
                            AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                IdentifierName(redFieldName),
                                ObjectCreationExpression(ParseTypeName(typeName))
                                    .WithArgumentList(ArgumentList(SeparatedList(new[]
                                    {
                        Argument(CastExpression(ParseTypeName("InternalSyntax.SyntaxList"),
                            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName("Green"),
                                IdentifierName("GetSlot")))
                            .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(slotIndexLiteral)))))),
                        Argument(ThisExpression()),
                        Argument(BinaryExpression(SyntaxKind.AddExpression,
                            IdentifierName("Position"),
                            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName("Green"),
                                IdentifierName("GetChildStartPosition")))
                            .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(slotIndexLiteral))))))
                                    })))
                            )
                        )),
                    ReturnStatement(IdentifierName(redFieldName))
                );
            }
            else
            {
                if (isNullable)
                {
                    var backing = "_" + camel;
                    backingFields.Add(FieldDeclaration(
                        VariableDeclaration(IdentifierName(typeName + "?"))
                            .WithVariables(SingletonSeparatedList(VariableDeclarator(Identifier(backing)))))
                        .AddModifiers(Token(SyntaxKind.InternalKeyword)));

                    getExpr = CastExpression(IdentifierName(typeName + "?"),
                        InvocationExpression(IdentifierName("GetNodeSlot"))
                            .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot)))));

                    getNodeSwitchArms.Add(
                        SwitchExpressionArm(
                            ConstantPattern(greenSlot),
                            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                                ThisExpression(), IdentifierName("GetRed")))
                                .WithArgumentList(ArgumentList(SeparatedList(
                                [
                                    Argument(IdentifierName("ref " + backing)),
                                    Argument(greenSlot)
                                ])))
                        ));
                }
                else
                {
                    var backing = "_" + camel;
                    backingFields.Add(FieldDeclaration(
                        VariableDeclaration(IdentifierName(typeName + "?"))
                            .WithVariables(SingletonSeparatedList(VariableDeclarator(Identifier(backing)))))
                        .AddModifiers(Token(SyntaxKind.InternalKeyword)));

                    getExpr = CastExpression(IdentifierName(typeName),
                        InvocationExpression(IdentifierName("GetNodeSlot"))
                            .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(greenSlot)))));

                    getNodeSwitchArms.Add(
                        SwitchExpressionArm(
                        ConstantPattern(greenSlot),
                        InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                            ThisExpression(), IdentifierName("GetRed"))).WithArgumentList(
                            ArgumentList(SeparatedList(new[]
                            {
                                    Argument(IdentifierName("ref " + backing)),
                                        Argument(greenSlot)
                            })))));
                }
            }


            var decl = PropertyDeclaration(ParseTypeName(MapRedType(prop.FullTypeName, prop.IsNullable)), prop.Name)
                .WithModifiers(TokenList(modifiers));

            if (getBlock is null && getExpr is not null)
            {
                decl = decl
                    .WithExpressionBody(ArrowExpressionClause(getExpr))
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken));
            }
            else if (getBlock is not null)
            {
                decl = decl
                    .WithAccessorList(AccessorList(List([AccessorDeclaration(SyntaxKind.GetAccessorDeclaration, getBlock)])));
            }


            properties.Add(decl);

            getExpr = null;
            getBlock = null;

            if (!node.IsAbstract)
            {
                var updateArgsForWith = node.Slots.Select(p =>
                {
                    var propName = ToCamelCase(p.Name);
                    return Argument(
                        p.Name == prop.Name
                            ? IdentifierName(propName)
                            : MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                ThisExpression(),
                                IdentifierName(p.Name)
                            )
                    );
                }).ToList();

                if (node.HasExplicitKind)
                {
                    updateArgsForWith.Insert(0,
                        Argument(MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            ThisExpression(),
                            IdentifierName("Kind"))));
                }

                withMethods.Add(
                       MethodDeclaration(IdentifierName(className), "With" + prop.Name)
                           .AddModifiers(Token(SyntaxKind.PublicKeyword))
                           .WithParameterList(ParameterList(SingletonSeparatedList(Parameter(Identifier(camel)).WithType(ctorType))))
                           .WithExpressionBody(ArrowExpressionClause(
                               InvocationExpression(IdentifierName("Update"))
                                   .WithArgumentList(ArgumentList(SeparatedList(updateArgsForWith)))))
                           .WithSemicolonToken(Token(SyntaxKind.SemicolonToken)));
            }

            slotIndex++;
        }

        if (!node.IsAbstract && node.HasExplicitKind)
        {
            var updateArgsForWithKind = new List<ArgumentSyntax>
                {
                    Argument(IdentifierName("kind"))
                };
            updateArgsForWithKind.AddRange(node.Slots.Select(p =>
                Argument(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, ThisExpression(), IdentifierName(p.Name)))
            ));

            withMethods.Insert(0, // add first for consistency
                MethodDeclaration(IdentifierName(className), "WithKind")
                    .AddModifiers(Token(SyntaxKind.PublicKeyword))
                    .WithParameterList(ParameterList(SingletonSeparatedList(
                        Parameter(Identifier("kind")).WithType(IdentifierName("SyntaxKind")))))
                    .WithExpressionBody(ArrowExpressionClause(
                        InvocationExpression(IdentifierName("Update"))
                            .WithArgumentList(ArgumentList(SeparatedList(updateArgsForWithKind)))))
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken)));
        }

        var members = new List<MemberDeclarationSyntax>();
        members.AddRange(backingFields);
        members.Add(ConstructorFromGreen(className, greenType));
        if (!node.IsAbstract)
        {
            members.Add(ConstructorPublic(className, ctorParams, greenType, greenArgs));
            members.Add(MethodUpdate(className, updateParams, updateArgs, node));
            members.AddRange(withMethods);
            members.Add(MethodAccept(className, node.Name));
            members.Add(MethodAcceptT(className, node.Name));
        }
        members.AddRange(properties);

        if (!node.IsAbstract && node.Slots.Any(s => s.Name == "TerminatorToken"))
        {
            var effectiveSpanProp = ParseMemberDeclaration(@"public override TextSpan EffectiveSpan
{
    get
    {
        var span = Span;
        return TerminatorToken.Kind == SyntaxKind.NewLineToken
            ? new TextSpan(span.Start, TerminatorToken.Span.Start - span.Start)
            : span;
    }
}");
            members.Add(effectiveSpanProp);
        }

        if (getNodeSwitchArms.Count > 0)
        {
            members.Add(MethodGetNodeSlot(getNodeSwitchArms));
        }
        var classModifiers = new List<SyntaxToken> { Token(SyntaxKind.PublicKeyword) };

        if (node.IsAbstract)
            classModifiers.Add(Token(SyntaxKind.AbstractKeyword));
        else
            classModifiers.Add(Token(SyntaxKind.SealedKeyword));

        classModifiers.Add(Token(SyntaxKind.PartialKeyword));

        var classDecl = ClassDeclaration(className)
            .AddModifiers(classModifiers.ToArray())
            .WithBaseList(BaseList(SingletonSeparatedList<BaseTypeSyntax>(SimpleBaseType(baseType))))
            .AddMembers(members.ToArray());

        return CompilationUnit()
            .WithUsings(List(new[]
            {
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax.InternalSyntax"))
            }))
            .WithMembers(SingletonList<MemberDeclarationSyntax>(
                NamespaceDeclaration(ParseName("Raven.CodeAnalysis.Syntax"))
                    .AddMembers(classDecl)))
            .NormalizeWhitespace();
    }

    private static ConstructorDeclarationSyntax ConstructorFromGreen(string className, TypeSyntax greenType) =>
        ConstructorDeclaration(className)
            .AddModifiers(Token(SyntaxKind.InternalKeyword))
            .WithParameterList(ParameterList(SeparatedList(new[]
            {
                Parameter(Identifier("green")).WithType(greenType),
                Parameter(Identifier("parent")).WithType(NullableType(IdentifierName("SyntaxNode"))),
                Parameter(Identifier("position")).WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
            })))
            .WithInitializer(ConstructorInitializer(SyntaxKind.BaseConstructorInitializer,
                ArgumentList(SeparatedList(new[]
                {
                    Argument(IdentifierName("green")),
                    Argument(IdentifierName("parent")),
                    Argument(IdentifierName("position"))
                }))))
            .WithBody(Block());

    private static ConstructorDeclarationSyntax ConstructorPublic(string className, List<ParameterSyntax> parameters, TypeSyntax greenType, List<ArgumentSyntax> greenArgs) =>
        ConstructorDeclaration(className)
            .AddModifiers(Token(SyntaxKind.PublicKeyword))
            .WithParameterList(ParameterList(SeparatedList(parameters)))
            .WithInitializer(ConstructorInitializer(SyntaxKind.ThisConstructorInitializer,
                ArgumentList(SeparatedList([
                    Argument(ObjectCreationExpression(greenType)
                        .WithArgumentList(ArgumentList(SeparatedList(greenArgs.Concat(
                        [
                            Argument(LiteralExpression(SyntaxKind.NullLiteralExpression))
                                .WithNameColon(NameColon("diagnostics")),
                            Argument(LiteralExpression(SyntaxKind.NullLiteralExpression))
                                .WithNameColon(NameColon("annotations"))
                        ]))))),
                        Argument(LiteralExpression(SyntaxKind.NullLiteralExpression)), Argument(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(0)))]))))
            .WithBody(Block());

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

        if (node.HasExplicitKind)
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
        for (int i = 0; i < node.Slots.Count; i++)
        {
            var prop = node.Slots[i];
            var param = updateParams[i];

            finalParams.Add(param);
        }

        ctorArgs.AddRange(updateArgs.Select(arg => arg));

        // Property inequality checks
        if (updateArgs.Count > 0)
        {
            conditions.AddRange(updateArgs
                .Zip(node.Slots, (arg, prop) =>
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

    private static MethodDeclarationSyntax MethodAccept(string className, string nodeName) =>
        MethodDeclaration(PredefinedType(Token(SyntaxKind.VoidKeyword)), "Accept")
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddParameterListParameters(Parameter(Identifier("visitor")).WithType(IdentifierName("SyntaxVisitor")))
            .WithBody(Block(ExpressionStatement(InvocationExpression(
                MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName("visitor"), IdentifierName("Visit" + nodeName)))
                .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(ThisExpression())))))));

    private static MethodDeclarationSyntax MethodAcceptT(string className, string nodeName) =>
        MethodDeclaration(IdentifierName("TNode"), "Accept")
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddTypeParameterListParameters(TypeParameter("TNode"))
            .AddParameterListParameters(Parameter(Identifier("visitor"))
                .WithType(GenericName("SyntaxVisitor").WithTypeArgumentList(TypeArgumentList(SingletonSeparatedList<TypeSyntax>(IdentifierName("TNode"))))))
            .WithBody(Block(ReturnStatement(InvocationExpression(
                MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName("visitor"), IdentifierName("Visit" + nodeName)))
                .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(ThisExpression())))))));

    private static MethodDeclarationSyntax MethodGetNodeSlot(List<SwitchExpressionArmSyntax> arms) =>
        MethodDeclaration(IdentifierName("SyntaxNode"), "GetNodeSlot")
            .AddModifiers(Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.OverrideKeyword))
            .AddParameterListParameters(Parameter(Identifier("index")).WithType(PredefinedType(Token(SyntaxKind.IntKeyword))))
            .WithBody(Block(ReturnStatement(SwitchExpression(IdentifierName("index")).WithArms(SeparatedList(arms.Append(
                SwitchExpressionArm(DiscardPattern(), ThrowExpression(ObjectCreationExpression(IdentifierName("Exception"))
                    .WithArgumentList(ArgumentList())))))))));

    public static CompilationUnitSyntax GenerateRedFactoryMethod(SyntaxNodeModel node)
    {
        if (node.IsAbstract)
            return null!; // skip abstract nodes

        var methodName = node.Name;
        var typeName = node.Name + "Syntax";
        var parameters = new List<ParameterSyntax>();
        var arguments = new List<ArgumentSyntax>();

        if (node.HasExplicitKind)
        {
            parameters.Add(
                Parameter(Identifier("kind"))
                    .WithType(IdentifierName("SyntaxKind")));
            arguments.Add(Argument(IdentifierName("kind")));
        }

        foreach (var prop in node.Slots)
        {
            parameters.Add(
                Parameter(Identifier(ToCamelCase(prop.Name)))
                    .WithType(IdentifierName(MapRedType(prop.FullTypeName, prop.IsNullable))));
            arguments.Add(Argument(IdentifierName(ToCamelCase(prop.Name))));
        }

        var method = MethodDeclaration(IdentifierName(typeName), methodName)
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.StaticKeyword))
            .WithParameterList(ParameterList(SeparatedList(parameters)))
            .WithExpressionBody(ArrowExpressionClause(
                ObjectCreationExpression(IdentifierName(typeName))
                    .WithArgumentList(ArgumentList(SeparatedList(arguments)))))
            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken));

        var factoryClass = ClassDeclaration("SyntaxFactory")
            .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.StaticKeyword), Token(SyntaxKind.PartialKeyword))
            .AddMembers(method);

        return CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(NamespaceDeclaration(ParseName("Raven.CodeAnalysis.Syntax")).AddMembers(factoryClass))
            .NormalizeWhitespace();
    }

    private static string MapRedType(string rawType, bool nullable) => rawType switch
    {
        "Node" => "SyntaxNode",
        "Token" => nullable ? "SyntaxToken?" : "SyntaxToken",
        "TokenList" => "SyntaxTokenList",
        var t when t.StartsWith("List<") => "SyntaxList<" + t[(t.IndexOf('<') + 1)..^1] + "Syntax>",
        var t when t.StartsWith("SeparatedList<") => "SeparatedSyntaxList<" + t[(t.IndexOf('<') + 1)..^1] + "Syntax>",
        _ => rawType + "Syntax" + (nullable ? "?" : "")
    };

    private static string MapGreenType(string rawType) => rawType switch
    {
        "Token" => "SyntaxToken",
        "TokenList" => "SyntaxList",
        var t when t.StartsWith("List<") || t.StartsWith("SeparatedList<") => "SyntaxList",
        _ => rawType + "Syntax"
    };

    private static bool IsListType(string rawType) => rawType.StartsWith("List<") || rawType.StartsWith("SeparatedList<");

    private static string ToCamelCase(string name) => char.ToLowerInvariant(name[0]) + name[1..];

    private static ExpressionSyntax GetGreenCast(string camel, string type, bool isToken, bool isNullable)
    {
        var greenType = ParseTypeName("InternalSyntax." + MapGreenType(type));
        ExpressionSyntax access = isNullable
            ? ConditionalAccessExpression(IdentifierName(camel), MemberBindingExpression(IdentifierName("Green")))
            : MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName(camel), IdentifierName("Green"));

        return isToken
            ? access
            : isNullable
                ? CastExpression(NullableType(greenType), access)
                : CastExpression(greenType, access);
    }
}
