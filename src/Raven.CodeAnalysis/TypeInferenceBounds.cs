using System.Collections;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

// Provisional substitutions and their evidence belong to one inference candidate.
// Retain bounds so a later argument can refine an earlier upper-bound hint.
internal sealed class TypeInferenceBounds : IReadOnlyDictionary<ITypeParameterSymbol, ITypeSymbol>
{
    internal enum BoundKind { Lower, Upper, Exact }
    private readonly record struct Bound(ITypeSymbol Type, BoundKind Kind);
    private readonly Dictionary<ITypeParameterSymbol, List<Bound>> _bounds = new(SymbolEqualityComparer.Default);

    private readonly Dictionary<ITypeParameterSymbol, ITypeSymbol> _substitutions = new(SymbolEqualityComparer.Default);

    internal TypeInferenceBounds() { }

    public int Count => _substitutions.Count;
    public IEnumerable<ITypeParameterSymbol> Keys => _substitutions.Keys;
    public IEnumerable<ITypeSymbol> Values => _substitutions.Values;
    public ITypeSymbol this[ITypeParameterSymbol parameter] => _substitutions[parameter];
    public bool ContainsKey(ITypeParameterSymbol parameter) => _substitutions.ContainsKey(parameter);
    public bool TryGetValue(ITypeParameterSymbol parameter, out ITypeSymbol value) => _substitutions.TryGetValue(parameter, out value!);
    public IEnumerator<KeyValuePair<ITypeParameterSymbol, ITypeSymbol>> GetEnumerator() => _substitutions.GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    internal TypeInferenceBounds(TypeInferenceBounds source) : this() => ReplaceWith(source);

    internal void AddBound(Compilation compilation, ITypeParameterSymbol parameter, ITypeSymbol type, BoundKind kind)
    {
        if (!_bounds.TryGetValue(parameter, out var bounds))
            _bounds[parameter] = bounds = [];
        bounds.Add(new(type, kind));

        var candidates = bounds.Select(bound => WithoutReferenceAnnotation(bound.Type))
            .Distinct<ITypeSymbol>(SymbolEqualityComparer.Default)
            .Where(candidate => bounds.All(bound => Satisfies(candidate, bound)))
            .ToArray();
        var best = candidates.Where(candidate => candidates.All(other =>
            SymbolEqualityComparer.Default.Equals(other, candidate) ||
            compilation.ClassifyConversion(other, candidate).IsImplicit)).ToArray();
        if (best.Length != 1)
        {
            // A later bound may supply a unique candidate; final fixing requires
            // an entry for every method type parameter.
            _substitutions.Remove(parameter);
            return;
        }

        var result = best[0];
        if (!result.IsValueType)
        {
            var matching = bounds.Where(bound => SymbolEqualityComparer.Default.Equals(
                WithoutReferenceAnnotation(bound.Type), result)).ToArray();
            var annotationBounds = matching.Where(bound => bound.Kind == BoundKind.Exact).ToArray();
            if (annotationBounds.Length == 0)
                annotationBounds = matching.Where(bound => bound.Kind == BoundKind.Lower).ToArray();
            var nullable = annotationBounds.Length > 0
                ? annotationBounds.Any(bound => bound.Type.IsNullable)
                : matching.All(bound => bound.Type.IsNullable);
            if (nullable)
                result = result.GetNullableType();
        }
        _substitutions[parameter] = result;

        bool Satisfies(ITypeSymbol candidate, Bound bound)
        {
            var boundType = WithoutReferenceAnnotation(bound.Type);
            if (SymbolEqualityComparer.Default.Equals(candidate, boundType))
                return true;
            return bound.Kind switch
            {
                BoundKind.Lower => compilation.ClassifyConversion(boundType, candidate).IsImplicit,
                BoundKind.Upper => compilation.ClassifyConversion(candidate, boundType).IsImplicit,
                _ => false
            };
        }
    }

    internal void PreserveExistingFrom(TypeInferenceBounds source)
    {
        foreach (var entry in source)
        {
            _substitutions[entry.Key] = entry.Value;
            if (source._bounds.TryGetValue(entry.Key, out var bounds))
                _bounds[entry.Key] = [.. bounds];
        }
    }

    internal void ReplaceWith(TypeInferenceBounds source)
    {
        _substitutions.Clear();
        _bounds.Clear();
        foreach (var entry in source)
            _substitutions[entry.Key] = entry.Value;
        foreach (var entry in source._bounds)
            _bounds[entry.Key] = [.. entry.Value];
    }

    internal static BoundKind Reverse(BoundKind kind) => kind switch
    {
        BoundKind.Lower => BoundKind.Upper,
        BoundKind.Upper => BoundKind.Lower,
        _ => BoundKind.Exact
    };

    private static ITypeSymbol WithoutReferenceAnnotation(ITypeSymbol type)
        => type.IsValueType ? type : type.GetNonNullableType();
}
