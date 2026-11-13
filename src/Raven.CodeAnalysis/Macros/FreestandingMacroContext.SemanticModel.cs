using System;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public partial class FreestandingMacroContext
{
    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(compilation, semanticModel, FreestandingMacroInvocation.Create(syntax), cancellationToken)
    {
    }

    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroMemberDeclarationSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(compilation, semanticModel, FreestandingMacroInvocation.Create(syntax), cancellationToken)
    {
    }

    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroDeclarationSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(compilation, semanticModel, FreestandingMacroInvocation.Create(syntax), cancellationToken)
    {
    }

    internal FreestandingMacroContext(Compilation compilation, SemanticModel semanticModel,
        FreestandingMacroInvocation invocation, CancellationToken cancellationToken = default)
        : this(CreateServices(compilation, semanticModel, invocation), invocation, cancellationToken)
    {
    }

    public Compilation Compilation => HostServices.Compilation;

    public SemanticModel SemanticModel => HostServices.SemanticModel;

    private SemanticModelMacroArgumentServices HostServices => _services as SemanticModelMacroArgumentServices
        ?? throw new InvalidOperationException("This internal context has no semantic-model host adapter.");

    private static MacroArgumentServices CreateServices(Compilation compilation, SemanticModel semanticModel, FreestandingMacroInvocation invocation)
    {
        ArgumentNullException.ThrowIfNull(invocation.Syntax, "invocationSyntax");
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(semanticModel);
        return new SemanticModelMacroArgumentServices(compilation, semanticModel, MacroArgument.CreateServices(invocation.Syntax, semanticModel));
    }
}
