using System;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public partial class AttachedMacroContext
{
    public AttachedMacroContext(Compilation compilation, SemanticModel semanticModel,
        AttributeSyntax syntax, SyntaxNode targetDeclaration, SyntaxNode currentDeclaration,
        CancellationToken cancellationToken = default)
        : this(CreateServices(compilation, semanticModel, syntax), syntax, targetDeclaration, currentDeclaration, cancellationToken)
    {
    }

    public Compilation Compilation => HostServices.Compilation;

    public SemanticModel SemanticModel => HostServices.SemanticModel;

    private SemanticModelMacroArgumentServices HostServices => _services as SemanticModelMacroArgumentServices
        ?? throw new InvalidOperationException("This internal context has no semantic-model host adapter.");

    private static MacroArgumentServices CreateServices(Compilation compilation, SemanticModel semanticModel, AttributeSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(semanticModel);
        return new SemanticModelMacroArgumentServices(compilation, semanticModel, MacroArgument.CreateServices(syntax, semanticModel));
    }
}
