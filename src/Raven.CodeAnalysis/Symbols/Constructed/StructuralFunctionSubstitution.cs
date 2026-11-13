namespace Raven.CodeAnalysis.Symbols;

// Callable shapes own reconstruction; generic substitution only supplies the mapping.
internal interface StructuralFunctionSubstitution : TypeSymbol, IStructuralFunctionIdentity
{
    TypeSymbol Substitute(Func<TypeSymbol, TypeSymbol> substitute);
}
