using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    // Reuse ordinary bound calls/locals/branches for adapters which opt into the
    // portable enumerator plan. The default .NET generator retains its existing path.
    internal static bool TryLowerPortableEnumeration(IMethodSymbol owner, BoundForStatement node, out BoundStatement? result)
    {
        result = null;
        if (owner.ContainingAssembly is not SourceAssemblySymbol || node.Iteration is not
            { Kind: ForIterationKind.Generic, GetEnumeratorMethod: { } get, MoveNextMethod: { } next, CurrentGetter: { } current } ||
            node.Local is not { } local || !get.ReturnType.IsReferenceType ||
            !SymbolEqualityComparer.Default.Equals(local.Type, current.ReturnType) || next.ReturnType.SpecialType != SpecialType.System_Boolean ||
            next.Parameters.Length != 0 || current.Parameters.Length != 0 ||
            (get.IsStatic ? !get.IsExtensionMethod || get.Parameters.Length != 1 : get.Parameters.Length != 0)) return false;
        var lowerer = CreateLowerer(owner);
        result = lowerer.LowerPortableEnumeration(node, get, next, current);
        return true;
    }

    private BoundStatement LowerPortableEnumeration(BoundForStatement node, IMethodSymbol get, IMethodSymbol next, IMethodSymbol current)
    {
        var compilation = GetCompilation();
        var iterator = CreateTempLocal("iterator", get.ReturnType, isMutable: false);
        var begin = CreateLabel("iterator_begin");
        var continueLabel = CreateLabel("iterator_continue");
        var end = CreateLabel("iterator_end");
        var collection = (BoundExpression)VisitExpression(node.Collection)!;
        BoundExpression Call(IMethodSymbol method, BoundExpression receiver) => method.IsStatic
            ? new BoundInvocationExpression(method, [ApplyConversionIfNeeded(receiver, method.Parameters[0].Type, compilation)])
            : new BoundInvocationExpression(method, [], ApplyConversionIfNeeded(receiver, method.ContainingType!, compilation));
        BoundStatement body;
        _loopStack.Push((end, continueLabel));
        try { body = (BoundStatement)VisitStatement(node.Body)!; }
        finally { _loopStack.Pop(); }
        return new BoundBlockStatement([
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(iterator, Call(get, collection))]),
            CreateLabelStatement(begin),
            new BoundConditionalGotoStatement(end, Call(next, new BoundLocalAccess(iterator)), jumpIfTrue: false),
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(node.Local!, Call(current, new BoundLocalAccess(iterator)))]),
            body,
            CreateLabelStatement(continueLabel),
            new BoundGotoStatement(begin, isBackward: true),
            CreateLabelStatement(end)
        ]);
    }
}
