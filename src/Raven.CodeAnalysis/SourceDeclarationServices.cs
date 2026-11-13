using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal interface SourceDeclarationServices : AttributeValidationServices
{
    SyntaxTree[] SyntaxTrees { get; }
    bool UsesDeferredSourceAttributes { get; }
    bool SourceDeclarationsComplete { get; }
    SourceAttributeDeclarationContext GetAttributeDeclarationContext(Symbol owner);
    bool TryGetCurrentDeclaringSyntax(SyntaxReference reference, out SyntaxNode syntax);
    SourceAttributeBinding? BindAttribute(Symbol owner, AttributeSyntax syntax);
    void PublishAttributeDiagnostics(SourceAttributeBinding attribute);
}

internal interface SourceDeclarationServiceProvider
{
    SourceDeclarationServices DeclarationServices { get; }
}

internal readonly record struct SourceAttributeBinding(
    AttributeData Data,
    AttributeSyntax Syntax,
    DiagnosticBag Diagnostics);
