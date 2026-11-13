namespace Raven.CodeAnalysis;

public partial class Compilation : SequenceTypeServices
{
    TypeSymbol SequenceTypeServices.CharacterType => GetSpecialType(SpecialType.System_Char);

    NamedTypeSymbol? SequenceTypeServices.GetSequenceDefinition(bool asynchronous)
    {
        if (Options.RuntimeIterationContract is { } contract)
        {
            var name = asynchronous ? contract.Async?.IterableTypeName : contract.IterableTypeName;
            if (string.IsNullOrWhiteSpace(name))
                return null;
            var selected = GetTypeByMetadataName(name);
            return selected is { TypeKind: TypeKind.Interface, Arity: 1 } &&
                selected.ContainingAssembly?.Name == contract.AssemblyName ? selected : null;
        }

        var definition = asynchronous
            ? GetTypeByMetadataName("System.Collections.Generic.IAsyncEnumerable`1")
            : GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T);
        return definition is { TypeKind: TypeKind.Interface, Arity: 1 } ? definition : null;
    }
}
