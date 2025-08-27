using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class ReturnTypeCollector
{
    public static ITypeSymbol? Infer(BoundExpression body)
    {
        var collector = new Collector();
        collector.Visit(body);
        return collector.GetResult();
    }

    private sealed class Collector
    {
        private readonly HashSet<ITypeSymbol> _types = new(SymbolEqualityComparer.Default);

        public void Visit(BoundNode node)
        {
            switch (node)
            {
                case BoundReturnStatement ret:
                    if (ret.Expression?.Type is ITypeSymbol type)
                        AddType(type);
                    break;
                case BoundLambdaExpression:
                    // Don't traverse into nested lambdas
                    break;
                default:
                    foreach (var child in GetChildren(node))
                        Visit(child);
                    break;
            }
        }

        private static IEnumerable<BoundNode> GetChildren(BoundNode node)
        {
            foreach (var prop in node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance))
            {
                var value = prop.GetValue(node);
                switch (value)
                {
                    case BoundNode child:
                        yield return child;
                        break;
                    case IEnumerable enumerable when value is not string:
                        foreach (var item in enumerable)
                            if (item is BoundNode b)
                                yield return b;
                        break;
                }
            }
        }

        private void AddType(ITypeSymbol type)
        {
            if (type is IUnionTypeSymbol union)
            {
                foreach (var t in union.Types)
                    _types.Add(t);
            }
            else
            {
                _types.Add(type);
            }
        }

        public ITypeSymbol? GetResult()
        {
            if (_types.Count == 0)
                return null;
            if (_types.Count == 1)
                return _types.First();
            return new UnionTypeSymbol(_types, null, null, null, []);
        }
    }
}
