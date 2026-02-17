using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class AsyncReturnTypeUtilities
{
    public static ITypeSymbol InferAsyncReturnType(Compilation compilation, BoundNode body)
    {
        return ReturnTypeCollector.InferAsync(compilation, body)
            ?? compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
    }

    public static ITypeSymbol InferAsyncReturnType(Compilation compilation, ITypeSymbol? bodyType)
    {
        var taskType = compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);

        if (bodyType is null || bodyType.TypeKind == TypeKind.Error)
            return taskType;

        var normalized = TypeSymbolNormalization.NormalizeForInference(bodyType);

        // If the body already produces a task-shaped value, keep it as-is instead of wrapping it
        // again. This prevents double-tasking async lambdas such as `async () => 42` when generic
        // delegate inference substitutes `Task<T>` into the body type.
        normalized = normalized.GetPlainType();

        if (normalized.SpecialType == SpecialType.System_Threading_Tasks_Task ||
            normalized is INamedTypeSymbol { OriginalDefinition.SpecialType: SpecialType.System_Threading_Tasks_Task_T } ||
            IsNonGenericValueTask(normalized) ||
            IsGenericValueTask(normalized))
        {
            return normalized;
        }

        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

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
        asyncReturnType = asyncReturnType.GetPlainType();

        if (asyncReturnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return compilation.GetSpecialType(SpecialType.System_Unit);

        if (asyncReturnType is INamedTypeSymbol named &&
            named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            named.TypeArguments.Length == 1)
        {
            return named.TypeArguments[0];
        }

        if (IsNonGenericValueTask(asyncReturnType))
            return compilation.GetSpecialType(SpecialType.System_Unit);

        if (asyncReturnType is INamedTypeSymbol valueTaskNamed &&
            IsGenericValueTask(valueTaskNamed) &&
            valueTaskNamed.TypeArguments.Length == 1)
        {
            return valueTaskNamed.TypeArguments[0];
        }

        return null;
    }

    internal static bool IsNonGenericValueTask(ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol named)
            return false;

        return named.MetadataName == "ValueTask" &&
            named.TypeArguments.IsDefaultOrEmpty &&
            IsSystemThreadingTasksNamespace(named.ContainingNamespace);
    }

    internal static bool IsGenericValueTask(ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol named)
            return false;

        var definition = named.OriginalDefinition as INamedTypeSymbol ?? named.ConstructedFrom as INamedTypeSymbol ?? named;
        return definition.MetadataName == "ValueTask`1" &&
            definition.Arity == 1 &&
            IsSystemThreadingTasksNamespace(definition.ContainingNamespace);
    }

    private static bool IsSystemThreadingTasksNamespace(INamespaceSymbol? ns)
    {
        return ns is
        {
            Name: "Tasks",
            ContainingNamespace:
            {
                Name: "Threading",
                ContainingNamespace:
                {
                    Name: "System",
                    ContainingNamespace.IsGlobalNamespace: true
                }
            }
        };
    }
}
