using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

// Compiler-owned semantic operations for an authored macro argument.
internal interface MacroArgumentServices
{
    TypeSymbol GetSpecialType(SpecialType specialType);
    TypeSymbol? GetArgumentType(ExpressionSyntax expression);
    TypeSymbol? ResolveConstraint(Type runtimeType);
    Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination);
}
