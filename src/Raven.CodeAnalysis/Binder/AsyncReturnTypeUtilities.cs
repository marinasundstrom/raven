using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class AsyncReturnTypeUtilities
{
    // This is a backend capability, separate from selecting consumption interfaces.
    internal static bool TryGetRuntimeIterator(Compilation compilation, TypeSymbol? type,
        out IteratorMethodKind kind, out TypeSymbol element)
    {
        kind = IteratorMethodKind.None;
        element = compilation.ErrorTypeSymbol;
        if (compilation.Options.RuntimeIterationContract is not { Async: { AllowGeneratorBodies: true } selected } contract ||
            type is not NamedTypeSymbol { TypeArguments.Length: 1 } named)
            return false;
        var definition = named.OriginalDefinition;
        if (definition.ContainingAssembly?.Name != contract.AssemblyName || definition.TypeKind != TypeKind.Interface)
            return false;
        if (SymbolEqualityComparer.Default.Equals(definition, compilation.GetTypeByMetadataName(selected.IterableTypeName)))
            kind = IteratorMethodKind.AsyncEnumerable;
        else if (SymbolEqualityComparer.Default.Equals(definition, compilation.GetTypeByMetadataName(selected.IteratorTypeName)))
            kind = IteratorMethodKind.AsyncEnumerator;
        else
            return false;
        element = named.TypeArguments[0];
        return true;
    }

    internal static bool IsNonGenericTaskLike(TypeSymbol type)
    {
        type = type.GetNonNullableType();
        return type.SpecialType == SpecialType.System_Threading_Tasks_Task || IsNonGenericValueTask(type);
    }

    public static bool IsValidAsyncReturnType(TypeSymbol? type, bool allowErrorType = true)
    {
        if (type is null)
            return false;

        if (type.TypeKind == TypeKind.Error)
            return allowErrorType;

        if (type is NullableTypeSymbol nullable)
            type = nullable.UnderlyingType;

        if (type.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return true;

        if (type is NamedTypeSymbol named &&
            (named.OriginalDefinition as NamedTypeSymbol ?? named.ConstructedFrom as NamedTypeSymbol ?? named)
                .SpecialType == SpecialType.System_Threading_Tasks_Task_T)
        {
            return true;
        }

        if (IsNonGenericValueTask(type) || IsGenericValueTask(type))
            return true;

        if (type is NamedTypeSymbol namedType)
        {
            var definition = namedType.OriginalDefinition as NamedTypeSymbol
                ?? namedType.ConstructedFrom as NamedTypeSymbol
                ?? namedType;

            if ((definition.MetadataName == "IAsyncEnumerable`1" ||
                 definition.MetadataName == "IAsyncEnumerator`1") &&
                definition.ContainingNamespace is
                {
                    Name: "Generic",
                    ContainingNamespace:
                    {
                        Name: "Collections",
                        ContainingNamespace:
                        {
                            Name: "System",
                            ContainingNamespace.IsGlobalNamespace: true
                        }
                    }
                })
            {
                return true;
            }
        }

        return false;
    }

    public static string GetSuggestedAsyncReturnTypeDisplay(Compilation compilation, TypeSymbol? returnType)
    {
        var taskType = compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
        var normalized = returnType?.GetNonNullableType();

        if (normalized is null ||
            normalized.TypeKind == TypeKind.Error ||
            SymbolEqualityComparer.Default.Equals(normalized, compilation.GetSpecialType(SpecialType.System_Unit)) ||
            normalized.SpecialType == SpecialType.System_Void)
        {
            return taskType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        }

        if (normalized is LiteralTypeSymbol literal)
            normalized = literal.UnderlyingType;

        if (compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task_T) is NamedTypeSymbol taskOfT &&
            taskOfT.TypeKind != TypeKind.Error)
        {
            return taskOfT.Construct(normalized)
                .ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        }

        return taskType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }

    public static TypeSymbol InferAsyncReturnType(Compilation compilation, BoundNode body)
    {
        return ReturnTypeCollector.InferAsync(compilation, body)
            ?? compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
    }

    public static TypeSymbol InferAsyncReturnType(Compilation compilation, TypeSymbol? bodyType)
    {
        var taskType = compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);

        if (bodyType is null || bodyType.TypeKind == TypeKind.Error)
            return taskType;

        var normalized = TypeSymbolNormalization.NormalizeForInference(bodyType);

        // If the body already produces a task-shaped value, keep it as-is instead of wrapping it
        // again. This prevents double-tasking async lambdas such as `async () => 42` when generic
        // delegate inference substitutes `Task<T>` into the body type.
        normalized = normalized.GetNonNullableType();

        if (normalized.SpecialType == SpecialType.System_Threading_Tasks_Task ||
            normalized is NamedTypeSymbol { OriginalDefinition.SpecialType: SpecialType.System_Threading_Tasks_Task_T } ||
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
            is NamedTypeSymbol taskGeneric &&
            taskGeneric.TypeKind != TypeKind.Error)
        {
            return taskGeneric.Construct(normalized);
        }

        return taskType;
    }

    public static TypeSymbol? ExtractAsyncResultType(Compilation compilation, TypeSymbol asyncReturnType)
    {
        asyncReturnType = asyncReturnType.GetNonNullableType();

        if (asyncReturnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return compilation.GetSpecialType(SpecialType.System_Unit);

        if (asyncReturnType is NamedTypeSymbol named &&
            (named.OriginalDefinition as NamedTypeSymbol ?? named.ConstructedFrom as NamedTypeSymbol ?? named)
                .SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            named.TypeArguments.Length == 1)
        {
            return named.TypeArguments[0];
        }

        if (IsNonGenericValueTask(asyncReturnType))
            return compilation.GetSpecialType(SpecialType.System_Unit);

        if (asyncReturnType is NamedTypeSymbol valueTaskNamed &&
            IsGenericValueTask(valueTaskNamed) &&
            valueTaskNamed.TypeArguments.Length == 1)
        {
            return valueTaskNamed.TypeArguments[0];
        }

        return null;
    }

    internal static bool IsNonGenericValueTask(TypeSymbol type)
    {
        if (type is not NamedTypeSymbol named)
            return false;

        return named.MetadataName == "ValueTask" &&
            named.TypeArguments.IsDefaultOrEmpty &&
            IsSystemThreadingTasksNamespace(named.ContainingNamespace);
    }

    internal static bool IsGenericValueTask(TypeSymbol type)
    {
        if (type is not NamedTypeSymbol named)
            return false;

        var definition = named.OriginalDefinition as NamedTypeSymbol ?? named.ConstructedFrom as NamedTypeSymbol ?? named;
        return definition.MetadataName == "ValueTask`1" &&
            definition.Arity == 1 &&
            IsSystemThreadingTasksNamespace(definition.ContainingNamespace);
    }

    private static bool IsSystemThreadingTasksNamespace(NamespaceSymbol? ns)
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
