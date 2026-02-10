using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal static class EntryPointSignature
{
    internal const string EntryPointName = "Main";

    public static bool Matches(IMethodSymbol method, Compilation compilation)
    {
        if (method.Name != EntryPointName || !method.IsStatic || method.IsGenericMethod)
            return false;

        if (!method.TypeParameters.IsDefaultOrEmpty)
            return false;

        if (!HasValidReturnType(method.ReturnType, compilation))
            return false;

        return HasValidParameters(method.Parameters, compilation);
    }

    public static bool HasValidReturnType(ITypeSymbol returnType, Compilation compilation)
    {
        switch (returnType.SpecialType)
        {
            case SpecialType.System_Int32:
            case SpecialType.System_Void:
            case SpecialType.System_Unit:
                return true;
        }

        var taskType = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task");

        if (returnType is INamedTypeSymbol namedReturn && IsTaskType(namedReturn, taskType))
            return true;

        if (returnType is INamedTypeSymbol named && !named.IsUnboundGenericType && named.Arity == 1)
        {
            var taskOfT = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task`1");

            if (IsTaskOfT(named, taskOfT))
            {
                var intType = compilation.GetSpecialType(SpecialType.System_Int32);
                if (!named.TypeArguments.IsDefaultOrEmpty && SymbolEqualityComparer.Default.Equals(named.TypeArguments[0], intType))
                    return true;

                if (!named.TypeArguments.IsDefaultOrEmpty &&
                    TryGetResultType(named.TypeArguments[0], out _, out var okType, out _) &&
                    IsSupportedResultOkType(okType))
                {
                    return true;
                }
            }
        }

        if (TryGetResultType(returnType, out _, out var directOkType, out _) &&
            IsSupportedResultOkType(directOkType))
            return true;

        return false;
    }

    public static bool IsAsyncReturnType(ITypeSymbol returnType, Compilation compilation, out bool returnsInt)
    {
        returnsInt = false;

        var taskType = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task");

        if (returnType is INamedTypeSymbol namedReturn && IsTaskType(namedReturn, taskType))
            return true;

        if (returnType is INamedTypeSymbol named && !named.IsUnboundGenericType && named.Arity == 1)
        {
            var taskOfT = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task`1");

            if (IsTaskOfT(named, taskOfT))
            {
                var intType = compilation.GetSpecialType(SpecialType.System_Int32);
                if (!named.TypeArguments.IsDefaultOrEmpty && SymbolEqualityComparer.Default.Equals(named.TypeArguments[0], intType))
                {
                    returnsInt = true;
                    return true;
                }

                if (!named.TypeArguments.IsDefaultOrEmpty &&
                    TryGetResultType(named.TypeArguments[0], out _, out var okType, out _))
                {
                    if (IsSupportedResultOkType(okType))
                    {
                        returnsInt = okType.SpecialType == SpecialType.System_Int32;
                        return true;
                    }
                }
            }
        }

        return false;
    }

    public static bool HasValidParameters(ImmutableArray<IParameterSymbol> parameters, Compilation compilation)
    {
        if (parameters.Length == 0)
            return true;

        if (parameters.Length > 1)
            return false;

        var parameter = parameters[0];

        if (parameter.RefKind != RefKind.None)
            return false;

        if (parameter.Type is not IArrayTypeSymbol arrayType)
            return false;

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        return SymbolEqualityComparer.Default.Equals(arrayType.ElementType, stringType);
    }

    public static ITypeSymbol ResolveReturnType(Compilation compilation, bool returnsInt)
    {
        return returnsInt
            ? compilation.GetSpecialType(SpecialType.System_Int32)
            : compilation.GetSpecialType(SpecialType.System_Unit);
    }

    public static ITypeSymbol ResolveBridgeReturnType(Compilation compilation, ITypeSymbol returnType)
    {
        if (TryGetResultPayloadTypes(returnType, out var okType, out _, out _))
        {
            return okType.SpecialType == SpecialType.System_Int32
                ? compilation.GetSpecialType(SpecialType.System_Int32)
                : compilation.GetSpecialType(SpecialType.System_Unit);
        }

        if (returnType.SpecialType == SpecialType.System_Int32)
            return compilation.GetSpecialType(SpecialType.System_Int32);

        if (IsAsyncReturnType(returnType, compilation, out var returnsInt))
            return ResolveReturnType(compilation, returnsInt);

        return compilation.GetSpecialType(SpecialType.System_Unit);
    }

    public static ITypeSymbol ResolveAsyncReturnType(Compilation compilation, IAssemblySymbol assembly, bool returnsInt)
    {
        if (returnsInt && assembly.GetTypeByMetadataName("System.Threading.Tasks.Task`1") is INamedTypeSymbol taskOfT)
        {
            var intType = compilation.GetSpecialType(SpecialType.System_Int32);
            if (intType.TypeKind != TypeKind.Error)
                return taskOfT.Construct(intType);
        }

        if (assembly.GetTypeByMetadataName("System.Threading.Tasks.Task") is { } taskType)
            return taskType;

        return compilation.GetSpecialType(SpecialType.System_Unit);
    }

    public static ITypeSymbol CreateStringArrayType(IAssemblySymbol assembly)
    {
        var arrayType = assembly.GetTypeByMetadataName("System.Array");
        var stringType = assembly.GetTypeByMetadataName("System.String");

        return new ArrayTypeSymbol(arrayType, stringType, arrayType.ContainingSymbol, null, arrayType.ContainingNamespace, Array.Empty<Location>());
    }

    public static bool RequiresEntryPointBridge(ITypeSymbol returnType, Compilation compilation)
    {
        return IsAsyncReturnType(returnType, compilation, out _) ||
            TryGetResultPayloadTypes(returnType, out _, out _, out _);
    }

    public static bool TryGetResultPayloadTypes(
        ITypeSymbol returnType,
        out ITypeSymbol okType,
        out ITypeSymbol errorType,
        out bool asyncWrapped)
    {
        okType = null!;
        errorType = null!;
        asyncWrapped = false;

        if (TryGetResultType(returnType, out _, out okType, out errorType))
            return true;

        if (returnType is INamedTypeSymbol named &&
            !named.IsUnboundGenericType &&
            named.Arity == 1 &&
            IsTaskOfT(named, taskOfT: null) &&
            !named.TypeArguments.IsDefaultOrEmpty &&
            TryGetResultType(named.TypeArguments[0], out _, out okType, out errorType))
        {
            asyncWrapped = true;
            return true;
        }

        return false;
    }

    private static bool IsTaskType(INamedTypeSymbol namedReturn, INamedTypeSymbol? taskType)
    {
        if (taskType is not null && SymbolEqualityComparer.Default.Equals(namedReturn, taskType))
            return true;

        return namedReturn.MetadataName == "Task"
            && IsInNamespace(namedReturn.ContainingNamespace, "System.Threading.Tasks");
    }

    private static bool IsTaskOfT(INamedTypeSymbol named, INamedTypeSymbol? taskOfT)
    {
        if (taskOfT is not null && SymbolEqualityComparer.Default.Equals(named.OriginalDefinition, taskOfT))
            return true;

        return named.OriginalDefinition.MetadataName == "Task`1"
            && IsInNamespace(named.OriginalDefinition.ContainingNamespace, "System.Threading.Tasks");
    }

    private static bool TryGetResultType(
        ITypeSymbol type,
        out INamedTypeSymbol resultType,
        out ITypeSymbol okType,
        out ITypeSymbol errorType)
    {
        resultType = null!;
        okType = null!;
        errorType = null!;

        type = type.UnwrapLiteralType() ?? type;

        if (type is not INamedTypeSymbol named ||
            named.IsUnboundGenericType ||
            named.Arity != 2 ||
            !string.Equals(named.Name, "Result", StringComparison.Ordinal) ||
            named.TypeArguments.Length != 2)
        {
            return false;
        }

        resultType = named;
        okType = named.TypeArguments[0];
        errorType = named.TypeArguments[1];
        return true;
    }

    private static bool IsSupportedResultOkType(ITypeSymbol okType)
    {
        return okType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Unit;
    }

    private static bool IsInNamespace(INamespaceSymbol? namespaceSymbol, string qualifiedNamespace)
    {
        if (namespaceSymbol is null)
            return false;

        var remaining = qualifiedNamespace;

        while (!namespaceSymbol.IsGlobalNamespace)
        {
            var lastDot = remaining.LastIndexOf('.');
            var segment = lastDot >= 0 ? remaining[(lastDot + 1)..] : remaining;

            if (!string.Equals(namespaceSymbol.Name, segment, StringComparison.Ordinal))
                return false;

            if (lastDot < 0)
                return namespaceSymbol.ContainingNamespace.IsGlobalNamespace;

            remaining = remaining[..lastDot];
            namespaceSymbol = namespaceSymbol.ContainingNamespace;

            if (namespaceSymbol is null)
                return false;
        }

        return false;
    }
}
