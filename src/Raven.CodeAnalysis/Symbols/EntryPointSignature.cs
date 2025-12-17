using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal static class EntryPointSignature
{
    private const string EntryPointName = "Main";

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
        if (taskType is not null && SymbolEqualityComparer.Default.Equals(returnType, taskType))
            return true;

        if (returnType is INamedTypeSymbol named && !named.IsUnboundGenericType && named.Arity == 1)
        {
            var taskOfT = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task`1");
            if (taskOfT is INamedTypeSymbol definition && SymbolEqualityComparer.Default.Equals(named.ConstructedFrom, definition))
            {
                var intType = compilation.GetSpecialType(SpecialType.System_Int32);
                if (!named.TypeArguments.IsDefaultOrEmpty && SymbolEqualityComparer.Default.Equals(named.TypeArguments[0], intType))
                    return true;
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
}
