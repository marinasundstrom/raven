using System.Reflection.Metadata;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public class VisitorPartialGeneratorOptions(string nodeClassName, bool isInternal = false, string suffix = "Syntax", string? visitorClassName = null, string resultType = "SyntaxNode")
{
    public string NodeClassName { get; } = nodeClassName;
    public bool IsInternal { get; } = isInternal;
    public string Suffix { get; } = suffix;
    public string? VisitorClassName { get; } = visitorClassName;
    public string ResultType { get; } = resultType;

    public string VisitorName => $"{(VisitorClassName is not null ? VisitorClassName : Suffix)}Visitor";

    public string NodeParamName
    {
        get
        {
            string nodeParamName = "node";

            if (Suffix == "Symbol")
            {
                nodeParamName = "symbol";
            }

            return nodeParamName;
        }
    }

    public string MethodName
    {
        get
        {
            string sv = NodeClassName;

            if (Suffix == "Symbol")
            {
                sv = sv.Substring(1);
            }

            return $"Visit{sv.Replace(Suffix, string.Empty)}";
        }
    }
}

public class RewriterPartialGeneratorOptions(INamedTypeSymbol typeSymbol, bool isInternal = false, string suffix = "Syntax", string? rewriterClassName = null, string resultType = "SyntaxNode", bool implement = true)
{
    public string NodeClassName => typeSymbol.Name;
    public INamedTypeSymbol TypeSymbol { get; } = typeSymbol;
    public bool IsInternal { get; } = isInternal;
    public string Suffix { get; } = suffix;
    public string? RewriterClassName { get; } = rewriterClassName;
    public string ResultType { get; } = resultType;
    public bool Implement { get; } = implement;

    public string RewriterName => $"{(RewriterClassName is not null ? RewriterClassName : Suffix)}Rewriter";

    public string VisitorName => $"{(RewriterClassName is not null ? RewriterClassName : Suffix)}Visitor";


    public string NodeParamName
    {
        get
        {
            string nodeParamName = "node";

            if (Suffix == "Symbol")
            {
                nodeParamName = "symbol";
            }

            return nodeParamName;
        }
    }

    public string MethodName
    {
        get
        {
            string sv = NodeClassName;

            if (Suffix == "Symbol")
            {
                sv = sv.Substring(1);
            }

            return $"Visit{sv.Replace(Suffix, string.Empty)}";
        }
    }
}

public static class VisitorPartialGenerator
{
    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForVisitor(VisitorPartialGeneratorOptions options)
    {
        List<MethodDeclarationSyntax> methods = [];

        var methodName = options.MethodName;

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
                                    SingletonSeparatedList(
                                        Parameter(
                                            Identifier(options.NodeParamName))
                                        .WithType(
                                            IdentifierName(options.NodeClassName)))))
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
                                                            IdentifierName(options.NodeParamName))))))))));


        // Generate the partial class
        var generatedClass = ClassDeclaration(options.VisitorName)
            .WithModifiers(TokenList(
                Token(options.IsInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }

    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForGenericVisitor(VisitorPartialGeneratorOptions options)
    {
        List<MethodDeclarationSyntax> methods = [];

        var methodName = options.MethodName;

        methods.Add(CreateVisitMethod(options));

        // Generate the partial class
        var generatedClass = ClassDeclaration(options.VisitorName)
            .WithTypeParameterList(
                TypeParameterList(
                    SeparatedList([TypeParameter("TResult")])))
            .WithModifiers(TokenList(
                Token(options.IsInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }

    private static MethodDeclarationSyntax CreateVisitMethod(VisitorPartialGeneratorOptions options)
    {
        return MethodDeclaration(
                    IdentifierName("TResult"),
                    Identifier(options.MethodName))
                    .WithModifiers(
                        TokenList(
                            [
                                    Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.VirtualKeyword)]))
                                    .WithParameterList(
                                        ParameterList(
                                            SingletonSeparatedList(
                                                Parameter(
                                                    Identifier(options.NodeParamName))
                                                .WithType(
                                                    IdentifierName(options.NodeClassName)))))
                                    .WithBody(
                                        CreateDefaultBody(options.NodeParamName));
    }

    private static BlockSyntax? CreateDefaultBody(string nodeName)
    {
        return
                                Block(
                                    SingletonList<StatementSyntax>(
                                        ReturnStatement(
                                            InvocationExpression(
                                                IdentifierName("DefaultVisit"))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList(
                                                        Argument(
                                                            IdentifierName(nodeName))))))));
    }

    public static ClassDeclarationSyntax GenerateVisitMethodForRewriter(INamedTypeSymbol? classSymbol, RewriterPartialGeneratorOptions options)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        List<MethodDeclarationSyntax> methods = [];

        var methodName = options.MethodName;

        ExpressionSyntax? expr;

        if (options.Implement)
        {
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
                        IdentifierName(options.NodeParamName),
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

            expr = InvocationExpression(
                ConditionalAccessExpression(IdentifierName(options.NodeParamName), MemberBindingExpression(IdentifierName("Update"))))
            .WithArgumentList(
                ArgumentList(
                    SeparatedList(
                        args)));
        }
        else
        {
            expr = IdentifierName(Identifier(options.NodeParamName));
        }

        methods.Add(MethodDeclaration(
                NullableType(IdentifierName(options.ResultType)),
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
                                                        Identifier(options.NodeParamName))
                                                    .WithType(
                                                        IdentifierName(className)))))
                                      .WithExpressionBody(ArrowExpressionClause(expr))
                                    .WithSemicolonToken(
                                        Token(SyntaxKind.SemicolonToken)));

        // Generate the partial class
        var generatedClass = ClassDeclaration(options.RewriterName)
            .WithBaseList(BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        GenericName(
                            Identifier(options.VisitorName))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList<TypeSyntax>(
                                    NullableType(IdentifierName(options.ResultType)))))))))
            .WithModifiers(TokenList(
                Token(options.IsInternal ? SyntaxKind.InternalKeyword : SyntaxKind.PublicKeyword),
                Token(SyntaxKind.AbstractKeyword),
                Token(SyntaxKind.PartialKeyword)))
            .AddMembers(methods.ToArray());

        return generatedClass;
    }
}
