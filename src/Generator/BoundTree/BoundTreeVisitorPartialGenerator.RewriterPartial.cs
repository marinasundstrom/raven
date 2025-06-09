namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class BoundNodeVisitorPartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GenerateRewriterPartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        return VisitorPartialGenerator.GenerateVisitMethodForRewriter(context, classSymbol, suffix: "BoundTree", visitorClassName: "BoundTree", resultType: "BoundNode", isInternal: true);
    }
}