using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    // Reuse ordinary bound calls/locals/branches for adapters which opt into the
    // portable enumerator plan. The default .NET generator retains its existing path.
    internal static bool TryLowerPortableEnumeration(IMethodSymbol owner, BoundForStatement node, out BoundStatement? result)
    {
        result = null;
        if (!CanLowerReferenceEnumeration(owner, node)) return false;
        var lowerer = CreateLowerer(owner);
        result = lowerer.LowerPortableEnumeration(node, lowerer.CreateLabel("iterator_end"), lowerer.CreateLabel("iterator_continue"));
        return true;
    }

    private static bool CanLowerReferenceEnumeration(ISymbol owner, BoundForStatement node)
        => owner.ContainingAssembly is SourceAssemblySymbol && node.Iteration is
        { Kind: ForIterationKind.Generic, GetEnumeratorMethod: { } get, MoveNextMethod: { } next, CurrentGetter: { } current } &&
            node.Local is { } local && get.ReturnType.IsReferenceType &&
            SymbolEqualityComparer.Default.Equals(local.Type, current.ReturnType) && next.ReturnType.SpecialType == SpecialType.System_Boolean &&
            next.Parameters.Length == 0 && current.Parameters.Length == 0 &&
            (get.IsStatic ? get.IsExtensionMethod && get.Parameters.Length == 1 : get.Parameters.Length == 0);

    // Materialize the resource before the method-wide scope-exit pass. Doing this
    // inside the late portable body planner loses ordering with enclosing use scopes.
    private bool CanLowerScopedEnumeration(BoundForStatement node)
        => GetCompilation().Options.RuntimeDisposalContract is { UseExceptionHandling: false } &&
            GetCompilation().Options.RuntimeIterationContract is not null &&
            CanLowerReferenceEnumeration(_containingSymbol, node) &&
            UseDisposalUtilities.SupportsUseDisposal(GetCompilation(), node.Iteration!.GetEnumeratorMethod!.ReturnType, preferAsync: false);

    private BoundStatement LowerPortableEnumeration(BoundForStatement node, ILabelSymbol end, ILabelSymbol continueLabel, bool dispose = false)
    {
        var compilation = GetCompilation();
        var get = node.Iteration!.GetEnumeratorMethod!;
        var next = node.Iteration.MoveNextMethod!;
        var current = node.Iteration.CurrentGetter!;
        var iterator = CreateTempLocal("iterator", get.ReturnType, isMutable: false);
        var begin = CreateLabel("iterator_begin");
        var collection = (BoundExpression)VisitExpression(node.Collection)!;
        BoundExpression Call(IMethodSymbol method, BoundExpression receiver) => method.IsStatic
            ? new BoundInvocationExpression(method, [ApplyConversionIfNeeded(receiver, method.Parameters[0].Type, compilation)])
            : new BoundInvocationExpression(method, [], ApplyConversionIfNeeded(receiver, method.ContainingType!, compilation));
        BoundStatement body;
        _loopStack.Push((end, continueLabel));
        try { body = (BoundStatement)VisitStatement(node.Body)!; }
        finally { _loopStack.Pop(); }
        return new BoundBlockStatement([
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(iterator, Call(get, collection))], isUsing: dispose),
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
