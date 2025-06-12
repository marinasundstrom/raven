using System.Reflection.Metadata;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Generator;

public class VisitorPartialGeneratorOptions(INamedTypeSymbol typeSymbol, bool isInternal = false, string nodeTypeNamePrefix = "Syntax", string? visitorClassNamePrefix = null, string resultType = "SyntaxNode")
{
    public INamedTypeSymbol TypeSymbol { get; } = typeSymbol;
    public string NodeClassName => TypeSymbol.Name;
    public string Namespace => TypeSymbol.ContainingNamespace.ToDisplayString();
    public bool IsInternal { get; } = isInternal;

    /// <summary>
    /// Prefix to trim from the node type name
    /// </summary>
    public string NodeTypeNamePrefix { get; } = nodeTypeNamePrefix;

    /// <summary>
    /// The prefix component of the visitor name
    /// </summary>
    public string? VisitorClassNamePrefix { get; } = visitorClassNamePrefix;
    public string ResultType { get; } = resultType;

    public string VisitorClassName => $"{(VisitorClassNamePrefix is not null ? VisitorClassNamePrefix : NodeTypeNamePrefix)}Visitor";

    public string NodeParamName
    {
        get
        {
            string nodeParamName = "node";

            if (NodeTypeNamePrefix == "Symbol")
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

            if (NodeTypeNamePrefix == "Symbol")
            {
                // Trim the I from the start of the name
                sv = sv.Substring(1);
            }

            return $"Visit{sv.Replace(NodeTypeNamePrefix, string.Empty)}";
        }
    }
}

public class RewriterPartialGeneratorOptions(INamedTypeSymbol typeSymbol, Func<RewriterPartialGeneratorOptions, ExpressionSyntax?> updateMethodGenerator, bool isInternal = false, string nodeTypeNamePrefix = "Syntax", string? rewriterClassNamePrefix = null, string resultType = "SyntaxNode", bool implement = true)
{
    public string NodeClassName => typeSymbol.Name;
    public string Namespace => typeSymbol.ContainingNamespace.ToDisplayString();
    public INamedTypeSymbol TypeSymbol { get; } = typeSymbol;
    public bool IsInternal { get; } = isInternal;

    /// <summary>
    /// Prefix to trim from the type name
    /// </summary>
    public string NodeTypeNamePrefix { get; } = nodeTypeNamePrefix;

    /// <summary>
    /// The prefix component of the rewriter name (visitor)
    /// </summary>
    public string? RewriterClassNamePrefix { get; } = rewriterClassNamePrefix;
    public string ResultType { get; } = resultType;
    public bool Implement { get; } = implement;

    public string RewriterClassName => $"{(RewriterClassNamePrefix is not null ? RewriterClassNamePrefix : NodeTypeNamePrefix)}Rewriter";
    public string BaseVisitorClassName => $"{(RewriterClassNamePrefix is not null ? RewriterClassNamePrefix : NodeTypeNamePrefix)}Visitor";

    public string NodeParamName
    {
        get
        {
            string nodeParamName = "node";

            if (NodeTypeNamePrefix == "Symbol")
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

            if (NodeTypeNamePrefix == "Symbol")
            {
                // Trim the I from the start of the name
                sv = sv.Substring(1);
            }

            return $"Visit{sv.Replace(NodeTypeNamePrefix, string.Empty)}";
        }
    }

    public Func<RewriterPartialGeneratorOptions, ExpressionSyntax?> UpdateMethodGenerator { get; } = updateMethodGenerator;
}

public static class VisitorPartialGenerator
{
    public static ClassDeclarationSyntax GeneratePartialClassWithVisitMethodForVisitor(VisitorPartialGeneratorOptions options)
    {
        List<MethodDeclarationSyntax> methods = [];

        methods.Add(MethodDeclaration(
        PredefinedType(
            Token(SyntaxKind.VoidKeyword)),
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
        var generatedClass = ClassDeclaration(options.VisitorClassName)
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

        methods.Add(CreateVisitMethod(options));

        // Generate the partial class
        var generatedClass = ClassDeclaration(options.VisitorClassName)
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

    public static ClassDeclarationSyntax GenerateVisitMethodForRewriter(RewriterPartialGeneratorOptions options)
    {
        List<MethodDeclarationSyntax> methods = [];

        ExpressionSyntax? expr;

        if (options.Implement)
        {
            expr = options.UpdateMethodGenerator(options);
        }
        else
        {
            // Just return the node. Alternatively. Just throw emit exception.

            expr = IdentifierName(Identifier(options.NodeParamName));
        }

        methods.Add(MethodDeclaration(
                NullableType(IdentifierName(options.ResultType)),
                            Identifier(options.MethodName))
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
                                                        IdentifierName(options.NodeClassName)))))
                                      .WithExpressionBody(ArrowExpressionClause(expr))
                                    .WithSemicolonToken(
                                        Token(SyntaxKind.SemicolonToken)));

        // Generate the partial class
        var generatedClass = ClassDeclaration(options.RewriterClassName)
            .WithBaseList(BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        GenericName(
                            Identifier(options.BaseVisitorClassName))
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
