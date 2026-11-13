using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal sealed class SemanticModelMacroArgumentServices(Compilation compilation, SemanticModel semanticModel,
    MacroArgumentServices arguments) : MacroArgumentServices
{
    public Compilation Compilation => compilation;
    public SemanticModel SemanticModel => semanticModel;
    public TypeSymbol GetSpecialType(SpecialType specialType) => arguments.GetSpecialType(specialType);
    public TypeSymbol? GetArgumentType(ExpressionSyntax expression) => arguments.GetArgumentType(expression);
    public TypeSymbol? ResolveConstraint(Type runtimeType) => arguments.ResolveConstraint(runtimeType);
    public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => arguments.ClassifyConversion(source, destination);
}
