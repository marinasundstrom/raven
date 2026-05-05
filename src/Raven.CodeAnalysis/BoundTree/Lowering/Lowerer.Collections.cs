using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitDictionaryExpression(BoundDictionaryExpression node)
    {
        var type = (ITypeSymbol)VisitSymbol(node.Type)!;
        var elements = node.Elements.Select(VisitDictionaryElement).ToArray();
        var collectionSymbol = (ISymbol?)VisitSymbol(node.CollectionSymbol);

        return node.Update(type, elements, collectionSymbol, node.Reason);
    }

    private DictionaryElementBinding VisitDictionaryElement(DictionaryElementBinding element)
    {
        return element switch
        {
            DictionaryEntryBinding entry => new DictionaryEntryBinding(
                (BoundExpression)Visit(entry.Key)!,
                (BoundExpression)Visit(entry.Value)!),
            DictionarySpreadBinding spread => new DictionarySpreadBinding(
                (BoundExpression)Visit(spread.Expression)!),
            DictionaryComprehensionBinding comprehension => new DictionaryComprehensionBinding(
                (BoundExpression)Visit(comprehension.Source)!,
                comprehension.IterationLocal,
                (BoundExpression?)Visit(comprehension.Condition),
                (BoundExpression)Visit(comprehension.KeySelector)!,
                (BoundExpression)Visit(comprehension.ValueSelector)!,
                comprehension.KeyType,
                comprehension.ValueType),
            _ => element
        };
    }
}
