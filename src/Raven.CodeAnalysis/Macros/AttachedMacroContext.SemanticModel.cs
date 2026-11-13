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

    private SemanticModelAttachedServices HostServices => _services as SemanticModelAttachedServices
        ?? throw new InvalidOperationException("This internal context has no semantic-model host adapter.");

    private static MacroArgumentServices CreateServices(Compilation compilation, SemanticModel semanticModel, AttributeSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(semanticModel);
        return new SemanticModelAttachedServices(compilation, semanticModel, MacroArgument.CreateServices(syntax, semanticModel));
    }

    private sealed class SemanticModelAttachedServices(Compilation compilation, SemanticModel semanticModel,
        MacroArgumentServices arguments) : MacroArgumentServices
    {
        public Compilation Compilation => compilation;
        public SemanticModel SemanticModel => semanticModel;
        public TypeSymbol GetSpecialType(SpecialType specialType) => arguments.GetSpecialType(specialType);
        public TypeSymbol? GetArgumentType(ExpressionSyntax expression) => arguments.GetArgumentType(expression);
        public TypeSymbol? ResolveConstraint(Type runtimeType) => arguments.ResolveConstraint(runtimeType);
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => arguments.ClassifyConversion(source, destination);
    }
}
