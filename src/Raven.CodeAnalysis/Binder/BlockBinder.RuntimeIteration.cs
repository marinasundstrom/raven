using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private static readonly DiagnosticDescriptor s_invalidRuntimeIterationContract = DiagnosticDescriptor.Create(
        "RAVT001", "Invalid runtime iteration contract", "", "",
        "Runtime iteration contract '{0}' cannot be used: {1}.", "compiler", DiagnosticSeverity.Error, true);

    private bool TryClassifyTargetIteration(ITypeSymbol collection, RuntimeIterationContract contract,
        out ForIterationInfo iteration, out string reason)
    {
        iteration = ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        reason = "missing or incompatible iterable/iterator declarations";
        if (string.IsNullOrWhiteSpace(contract.AssemblyName)
            || string.IsNullOrWhiteSpace(contract.IterableTypeName)
            || string.IsNullOrWhiteSpace(contract.IteratorTypeName)
            || string.IsNullOrWhiteSpace(contract.AcquisitionMethod)
            || string.IsNullOrWhiteSpace(contract.AdvanceMethod)
            || string.IsNullOrWhiteSpace(contract.CurrentProperty))
            return false;
        var iterable = Compilation.GetTypeByMetadataName(contract.IterableTypeName);
        var iterator = Compilation.GetTypeByMetadataName(contract.IteratorTypeName);
        bool Valid(INamedTypeSymbol? type) => type is { TypeKind: TypeKind.Interface, Arity: 1 }
            && type.ContainingAssembly?.Name == contract.AssemblyName && IsSymbolAccessible(type);
        if (!Valid(iterable) || !Valid(iterator)) return false;
        if (collection is not INamedTypeSymbol named) return false;
        var candidates = named.AllInterfaces.Prepend(named).OfType<INamedTypeSymbol>()
            .Where(t => SymbolEqualityComparer.Default.Equals(t.OriginalDefinition, iterable)).ToArray();
        if (candidates.Length != 1)
        {
            reason = "collection must implement exactly one selected iterable instantiation";
            return false;
        }
        var source = candidates[0];
        var expectedCursor = iterator!.Construct(source.TypeArguments.ToArray()) as INamedTypeSymbol;
        if (expectedCursor is null) return false;
        bool Callable(IMethodSymbol method) => !method.IsStatic && method.Parameters.Length == 0
            && method.TypeParameters.Length == 0 && IsSymbolAccessible(method);
        var acquire = source.GetMembers(contract.AcquisitionMethod).OfType<IMethodSymbol>()
            .Where(m => Callable(m) && SymbolEqualityComparer.Default.Equals(m.ReturnType, expectedCursor)).ToArray();
        var advance = expectedCursor.GetMembers(contract.AdvanceMethod).OfType<IMethodSymbol>()
            .Where(m => Callable(m) && m.ReturnType.SpecialType == SpecialType.System_Boolean).ToArray();
        var current = expectedCursor.GetMembers(contract.CurrentProperty).OfType<IPropertySymbol>()
            .Where(p => !p.IsStatic && p.Parameters.Length == 0 && p.GetMethod is { } getter && Callable(getter)
                && SymbolEqualityComparer.Default.Equals(p.Type, source.TypeArguments[0]))
            .Select(p => p.GetMethod!).ToArray();
        if (acquire.Length != 1 || advance.Length != 1 || current.Length != 1)
        {
            reason = "required acquisition, Boolean advance or element getter has an incompatible or ambiguous signature";
            return false;
        }
        iteration = ForIterationInfo.ForGeneric(source, expectedCursor, acquire[0], advance[0], current[0]);
        reason = "";
        return true;
    }
}
