namespace Generator;

using System.Diagnostics;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public partial class BoundNodeVisitorPartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // Identify all partial classes that inherit (directly or indirectly) from Symbol
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialPartialClass,
                transform: GetClassSymbol)
            .Where(symbol => symbol is not null && symbol.InheritsFromBoundNode() && IsNotReserved(symbol));

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, ProcessBoundNodeClass);
    }

    string[] reservedNames = [];

    private bool IsNotReserved(INamedTypeSymbol symbol)
    {
        return !reservedNames.Contains(symbol.Name);
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

    private static void ProcessBoundNodeClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return;

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var visitorPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForVisitor(new VisitorPartialGeneratorOptions(classSymbol, nodeTypeNamePrefix: "Bound", visitorClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true));

        var visitorGenericPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForGenericVisitor(new VisitorPartialGeneratorOptions(classSymbol, nodeTypeNamePrefix: "Bound", visitorClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true));

        var rewriterGenericPartialClass = VisitorPartialGenerator.GenerateVisitMethodForRewriter(new RewriterPartialGeneratorOptions(classSymbol, GenerateBoundTreeUpdateMethodImpl, nodeTypeNamePrefix: "Bound", rewriterClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true, implement: false));

        // Wrap it in a namespace
        var namespaceDeclaration = FileScopedNamespaceDeclaration(ParseName(namespaceName))
            .AddMembers(
                visitorPartialClass,
                visitorGenericPartialClass,
                rewriterGenericPartialClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
    }

    private static ExpressionSyntax GenerateBoundTreeUpdateMethodImpl(RewriterPartialGeneratorOptions options)
    {
        /*
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

        */

        return IdentifierName(Identifier(options.NodeParamName));
    }
}