using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed partial class MacroArgument
{
    public MacroArgument(ArgumentSyntax syntax, SemanticModel semanticModel)
        : this(syntax, CreateServices(syntax, semanticModel))
    {
    }

    public MacroArgument(ExpressionSyntax syntax, SemanticModel semanticModel)
        : this(syntax, CreateServices(syntax, semanticModel))
    {
    }

    private static MacroArgumentServices CreateServices(SyntaxNode? syntax, SemanticModel semanticModel)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        ArgumentNullException.ThrowIfNull(semanticModel);
        return new SemanticModelArgumentServices(semanticModel);
    }

    private sealed class SemanticModelArgumentServices(SemanticModel semanticModel) : MacroArgumentServices
    {
        public TypeSymbol GetSpecialType(SpecialType specialType)
            => semanticModel.Compilation.GetSpecialType(specialType);

        public TypeSymbol? GetArgumentType(ExpressionSyntax expression)
            => semanticModel.GetMacroArgumentType(expression);

        public TypeSymbol? ResolveConstraint(Type runtimeType)
            => MacroExpressionTypeFacts.ResolveConstraint(semanticModel.Compilation, runtimeType);

        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination)
            => semanticModel.Compilation.ClassifyConversion(source, destination);
    }
}
