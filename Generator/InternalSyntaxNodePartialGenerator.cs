namespace Generator;

using System;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public partial class InternalSyntaxNodePartialGenerator : IIncrementalGenerator
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
        context.RegisterSourceOutput(partialClasses, ProcessSyntaxNodeClass);
    }

    private void ProcessSyntaxNodeClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
        {
            return;
        }

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var constructor = ConstructorDeclaration(
                    Identifier(className))
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.PrivateKeyword)))
                .WithParameterList(
                    ParameterList(
                        SeparatedList<ParameterSyntax>(
                            new SyntaxNodeOrToken[]{
                                Parameter(
                                    Identifier("kind"))
                                .WithType(
                                    IdentifierName("SyntaxKind")),
                                Token(SyntaxKind.CommaToken),
                                Parameter(
                                    Identifier("slots"))
                                .WithType(
                                    ArrayType(
                                        IdentifierName("GreenNode"))
                                    .WithRankSpecifiers(
                                        SingletonList<ArrayRankSpecifierSyntax>(
                                            ArrayRankSpecifier(
                                                SingletonSeparatedList<ExpressionSyntax>(
                                                    OmittedArraySizeExpression())))))})))
                .WithInitializer(
                    ConstructorInitializer(
                        SyntaxKind.BaseConstructorInitializer,
                        ArgumentList(
                            SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Argument(
                                        IdentifierName("kind")),
                                    Token(SyntaxKind.CommaToken),
                                    Argument(
                                        IdentifierName("slots"))}))))
                .WithBody(
                    Block());

        var withUpdatedChildren = MethodDeclaration(
                    IdentifierName("GreenNode"),
                    Identifier("WithUpdatedChildren"))
                .WithModifiers(
                    TokenList(
                        new[]{
                            Token(SyntaxKind.ProtectedKeyword),
                            Token(SyntaxKind.OverrideKeyword)}))
                .WithParameterList(
                    ParameterList(
                        SingletonSeparatedList<ParameterSyntax>(
                            Parameter(
                                Identifier("newChildren"))
                            .WithType(
                                ArrayType(
                                    IdentifierName("GreenNode"))
                                .WithRankSpecifiers(
                                    SingletonList<ArrayRankSpecifierSyntax>(
                                        ArrayRankSpecifier(
                                            SingletonSeparatedList<ExpressionSyntax>(
                                                OmittedArraySizeExpression()))))))))
                .WithBody(
                    Block(
                        SingletonList<StatementSyntax>(
                            ReturnStatement(
                                ObjectCreationExpression(
                                    IdentifierName(className))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    IdentifierName("Kind")),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    IdentifierName("newChildren"))})))))));

        var createRed = MethodDeclaration(
                                    QualifiedName(
                                        IdentifierName("Syntax"),
                                        IdentifierName("SyntaxNode")),
                                    Identifier("CreateRed"))
                                .WithModifiers(
                                    TokenList(
                                        new[]{
                                    Token(SyntaxKind.PublicKeyword),
                                    Token(SyntaxKind.OverrideKeyword)}))
                                .WithParameterList(
                                    ParameterList(
                                        SeparatedList<ParameterSyntax>(
                                            new SyntaxNodeOrToken[]{
                                        Parameter(
                                            Identifier("parent"))
                                        .WithType(
                                            NullableType(
                                                QualifiedName(
                                                    IdentifierName("Syntax"),
                                                    IdentifierName("SyntaxNode")))),
                                        Token(SyntaxKind.CommaToken),
                                        Parameter(
                                            Identifier("position"))
                                        .WithType(
                                            PredefinedType(
                                                Token(SyntaxKind.IntKeyword)))})))
                                .WithBody(
                                    Block(
                                        SingletonList<StatementSyntax>(
                                            ReturnStatement(
                                                ObjectCreationExpression(
                                                    QualifiedName(
                                                        IdentifierName("Syntax"),
                                                        IdentifierName(className)))
                                                .WithArgumentList(
                                                    ArgumentList(
                                                        SeparatedList<ArgumentSyntax>(
                                                            new SyntaxNodeOrToken[]{
                                                        Argument(
                                                            ThisExpression()),
                                                        Token(SyntaxKind.CommaToken),
                                                        Argument(
                                                            IdentifierName("parent")),
                                                        Token(SyntaxKind.CommaToken),
                                                        Argument(
                                                            IdentifierName("position"))})))))));

        List<MemberDeclarationSyntax> members = [constructor];

        if (!classSymbol.IsAbstract)
        {
            members.Add(createRed);
            members.Add(withUpdatedChildren);
        }

        var ns =
                FileScopedNamespaceDeclaration(
                    QualifiedName(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("Raven"),
                                IdentifierName("CodeAnalysis")),
                            IdentifierName("Syntax")),
                        IdentifierName("InternalSyntax")))
                .WithMembers(
                    SingletonList<MemberDeclarationSyntax>(
                        ClassDeclaration(className)
                        .WithModifiers(
                            TokenList(
                                [
                            Token(SyntaxKind.PublicKeyword),
                            Token(SyntaxKind.PartialKeyword)]))
                        /*.WithBaseList(
                            BaseList(
                                SingletonSeparatedList<BaseTypeSyntax>(
                                    SimpleBaseType(
                                        IdentifierName("StatementSyntax"))))) */
                        .WithMembers(
                            List<MemberDeclarationSyntax>(members))));


        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(ns)
            .NormalizeWhitespace();

        context.AddSource($"InternalSyntax/{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
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
            if (baseType.Name == "SyntaxNode" && baseType.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis.Syntax.InternalSyntax")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }
}