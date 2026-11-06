using System.Collections.Immutable;
namespace Raven.CodeAnalysis.Symbols;

internal static class TupleStorageLayout
{
    internal readonly record struct Slot(FieldSymbol Field, TypeSymbol Type, ImmutableArray<FieldSymbol> Path);

    internal static bool HasRest(NamedTypeSymbol type)
        => (type.ConstructedFrom ?? type).SpecialType == SpecialType.System_ValueTuple_TRest;

    internal static NamedTypeSymbol? Construct(TypeSymbol[] elements, Func<int, NamedTypeSymbol?> resolve)
    {
        if (elements.Length <= 7 || resolve(8) is not { } wide || !HasRest(wide))
            return resolve(elements.Length)?.Construct(elements) as NamedTypeSymbol;

        NamedTypeSymbol? rest = null;
        for (int start = ((elements.Length - 1) / 7) * 7; start >= 0; start -= 7)
        {
            int count = Math.Min(7, elements.Length - start);
            var arguments = new TypeSymbol[count + (rest is null ? 0 : 1)];
            Array.Copy(elements, start, arguments, 0, count);
            if (rest is not null)
                arguments[^1] = rest;
            var definition = arguments.Length == 8 ? wide : resolve(arguments.Length);
            if (definition?.Construct(arguments) is not NamedTypeSymbol constructed)
                return null;
            rest = constructed;
        }
        return rest;
    }

    internal static ImmutableArray<Slot> Describe(NamedTypeSymbol storage)
    {
        var slots = ImmutableArray.CreateBuilder<Slot>();
        ImmutableArray<FieldSymbol> path = [];
        while (true)
        {
            var arguments = storage.TypeArguments;
            bool hasRest = HasRest(storage);
            int count = hasRest ? 7 : arguments.Length;
            if (count == 0 || hasRest && arguments.Length != 8)
                return default;
            for (int i = 0; i < count; i++)
            {
                var field = storage.GetMembers($"Item{i + 1}").OfType<FieldSymbol>().FirstOrDefault(f => !f.IsStatic);
                if (field is null) return default;
                slots.Add(new Slot(field, arguments[i], path));
            }
            if (!hasRest) return slots.ToImmutable();
            var rest = storage.GetMembers("Rest").OfType<FieldSymbol>().FirstOrDefault(f => !f.IsStatic);
            if (rest is null || arguments[7] is not NamedTypeSymbol tail ||
                (tail.ConstructedFrom ?? tail).SpecialType is < SpecialType.System_ValueTuple_T1 or > SpecialType.System_ValueTuple_TRest)
                return default;
            path = path.Add(rest);
            storage = tail;
        }
    }

    internal static TupleTypeSymbolImplementation? Project(NamedTypeSymbol storage, ImmutableArray<Slot> slots,
        (string? name, TypeSymbol type)[] elements, TupleTypeSymbol? original = null)
    {
        if (slots.IsDefault || slots.Length != elements.Length)
            return null;
        var tuple = new TupleTypeSymbolImplementation(storage, original?.ContainingSymbol!, original?.ContainingType, original?.ContainingNamespace, []);
        var fields = ImmutableArray.CreateBuilder<FieldSymbol>(elements.Length);
        for (int i = 0; i < elements.Length; i++)
            fields.Add(new TupleFieldSymbol(elements[i].name ?? $"Item{i + 1}", slots[i].Field, storage, [], elements[i].type, slots[i].Path));
        tuple.SetTupleElements(fields.MoveToImmutable());
        return tuple;
    }
}
