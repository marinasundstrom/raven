namespace Generator;

using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public partial class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        //Debugger.Launch();

        // Identify all partial classes that inherit (directly or indirectly) from SyntaxNode
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialPartialClass,
                transform: GetClassSymbol)
            .Where(symbol => symbol is not null && symbol.InheritsFromSyntaxNode());

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, ProcessSyntaxNodeClass);
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

    private static void ProcessSyntaxNodeClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return;

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var syntaxNodePartialClass = SyntaxNodePartialClassGenerator.GeneratePartialClass(context, classSymbol);

        var visitorPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForVisitor(new VisitorPartialGeneratorOptions(classSymbol));

        var visitorGenericPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForGenericVisitor(new VisitorPartialGeneratorOptions(classSymbol));

        var rewriterGenericPartialClass = VisitorPartialGenerator.GenerateVisitMethodForRewriter(new RewriterPartialGeneratorOptions(classSymbol, GenerateSyntaxNodeUpdateMethodImpl));

        // Wrap it in a namespace
        var namespaceDeclaration = FileScopedNamespaceDeclaration(ParseName(namespaceName))
            .AddMembers(
                syntaxNodePartialClass, visitorPartialClass,
                visitorGenericPartialClass, rewriterGenericPartialClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
    }

    private static ExpressionSyntax GenerateSyntaxNodeUpdateMethodImpl(RewriterPartialGeneratorOptions options)
    {
        ExpressionSyntax expr;
        var properties = options.TypeSymbol.GetMembers()
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
        return expr;
    }
}