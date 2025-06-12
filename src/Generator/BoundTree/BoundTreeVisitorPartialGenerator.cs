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

    readonly string[] reservedNames = [];

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

        var rewriterGenericPartialClass = VisitorPartialGenerator.GenerateVisitMethodForRewriter(new RewriterPartialGeneratorOptions(classSymbol, GenerateBoundTreeUpdateMethodImpl, nodeTypeNamePrefix: "Bound", rewriterClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true));

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
        ExpressionSyntax expr;

        var parameters = options.TypeSymbol.Constructors.First().Parameters
            .Select(x => new PropOrParamType(x.Name, x.Type.ToDisplayString(), FormatName(x), x.Type));

        var args = parameters.Select(property =>
 {
     var accessExpr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
         IdentifierName(options.NodeParamName),
         IdentifierName(property.TargetName));

     var typeSymbol = property.TypeSymbol;
     var elementType = typeSymbol.GetElementType();
     var isArray = typeSymbol is IArrayTypeSymbol;

     // Determine collection type
     var isCollection = elementType is not null;

     // If elementType is BoundNode-derived
     if (elementType is { } et && et.InheritsFromBoundNode())
     {
         var visitListInvocation = InvocationExpression(
             IdentifierName("VisitList"),
             ArgumentList(SingletonSeparatedList(Argument(accessExpr)))
         );

         return Argument(visitListInvocation);
     }

     // If elementType is ISymbol or implements it
     if (elementType is { } symType && (symType.IsISymbol() || symType.IsImplementingISymbol()))
     {
         var visitSymbolListInvocation = InvocationExpression(
             GenericName("VisitSymbolList")
                 .WithTypeArgumentList(TypeArgumentList(
                     SingletonSeparatedList(ParseTypeName(symType.Name)))),
             ArgumentList(SingletonSeparatedList(Argument(accessExpr)))
         );

         return Argument(visitSymbolListInvocation);
     }

     // Handle single bound node
     if (typeSymbol.InheritsFromBoundNode())
     {
         var methodName = $"Visit{typeSymbol.Name.Replace("Bound", "")}";
         var invocation = InvocationExpression(
             IdentifierName(methodName),
             ArgumentList(SingletonSeparatedList(Argument(accessExpr)))
         );

         var cast = CastExpression(ParseTypeName(property.Type), invocation);
         return Argument(cast);
     }

     // Handle ISymbol scalar
     if (typeSymbol.IsISymbol() || typeSymbol.IsImplementingISymbol())
     {
         var invocation = InvocationExpression(
             IdentifierName("VisitSymbol"),
             ArgumentList(SingletonSeparatedList(Argument(accessExpr)))
         );

         var cast = CastExpression(ParseTypeName(property.Type), invocation);
         return Argument(cast);
     }

     // Fall back to raw property
     return Argument(accessExpr);
 }).ToList();

        expr = InvocationExpression(
            ConditionalAccessExpression(IdentifierName(options.NodeParamName), MemberBindingExpression(IdentifierName("Update"))))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList(
                                args)));

        return expr;

        //return IdentifierName(options.NodeParamName);
    }

    private static string FormatName(IParameterSymbol x)
    {
        return char.ToUpper(x.Name[0]) + x.Name.Substring(1);
    }
}