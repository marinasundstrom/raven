using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class AsyncReturnTypeUtilities
{
    public static ITypeSymbol InferAsyncReturnType(Compilation compilation, BoundNode body)
    {
        var inferred = ReturnTypeCollector.Infer(body);
        return InferAsyncReturnType(compilation, inferred);
    }

    public static ITypeSymbol InferAsyncReturnType(Compilation compilation, ITypeSymbol? bodyType)
    {
        var taskType = compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);

        if (bodyType is null || bodyType.TypeKind == TypeKind.Error)
            return taskType;

        var normalized = TypeSymbolNormalization.NormalizeForInference(bodyType);

        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

        if (normalized.SpecialType == SpecialType.System_Threading_Tasks_Task)
        {
            normalized = unitType;
        }
        else if (normalized is INamedTypeSymbol named &&
                 named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
                 named.TypeArguments.Length == 1)
        {
            normalized = named.TypeArguments[0];
        }

        if (SymbolEqualityComparer.Default.Equals(normalized, unitType) ||
            normalized.SpecialType == SpecialType.System_Void)
        {
            return taskType;
        }

        if (normalized is LiteralTypeSymbol literal)
            normalized = literal.UnderlyingType;

        if (compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task_T)
            is INamedTypeSymbol taskGeneric &&
            taskGeneric.TypeKind != TypeKind.Error)
        {
            return taskGeneric.Construct(normalized);
        }

        return taskType;
    }

    public static ITypeSymbol? ExtractAsyncResultType(Compilation compilation, ITypeSymbol asyncReturnType)
    {
        if (asyncReturnType is NullableTypeSymbol nullable)
            asyncReturnType = nullable.UnderlyingType;

        if (asyncReturnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return compilation.GetSpecialType(SpecialType.System_Unit);

        if (asyncReturnType is INamedTypeSymbol named &&
            named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            named.TypeArguments.Length == 1)
        {
            return named.TypeArguments[0];
        }

        return null;
    }
}
