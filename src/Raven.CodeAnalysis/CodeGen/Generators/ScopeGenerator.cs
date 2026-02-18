using System.Collections.Generic;
using System.Collections.Immutable;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

class Scope : Generator
{
    private readonly IDictionary<ILocalSymbol, IILocal> _localBuilders = new Dictionary<ILocalSymbol, IILocal>(ReferenceEqualityComparer.Instance);
    private readonly ImmutableArray<ILocalSymbol> _localsToDispose;
    private bool _hasBreakLabel;
    private ILLabel _breakLabel;
    private bool _hasContinueLabel;
    private ILLabel _continueLabel;
    private bool _hasExceptionExitLabel;
    private ILLabel _exceptionExitLabel;

    public Scope(Generator parent) : this(parent, ImmutableArray<ILocalSymbol>.Empty)
    {
    }

    public Scope(Generator parent, ImmutableArray<ILocalSymbol> localsToDispose) : base(parent)
    {
        _localsToDispose = localsToDispose.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : localsToDispose;
    }

    public override void AddLocal(ILocalSymbol localSymbol, IILocal builder)
    {
        if (!_localBuilders.ContainsKey(localSymbol))
            _localBuilders.Add(localSymbol, builder);
    }

    public override IILocal? GetLocal(ILocalSymbol localSymbol)
    {
        if (_localBuilders.TryGetValue(localSymbol, out var localBuilder))
            return localBuilder;

        return Parent?.GetLocal(localSymbol);
    }

    public override IEnumerable<ILocalSymbol> EnumerateLocalsToDispose()
    {
        for (int i = _localsToDispose.Length - 1; i >= 0; i--)
            yield return _localsToDispose[i];

        if (Parent is not null)
        {
            foreach (var local in Parent.EnumerateLocalsToDispose())
                yield return local;
        }
    }

    public ImmutableArray<ILocalSymbol> LocalsToDispose => _localsToDispose;

    public bool IsLoopScope => _hasBreakLabel || _hasContinueLabel;

    public void SetLoopTargets(ILLabel breakLabel, ILLabel continueLabel)
    {
        _breakLabel = breakLabel;
        _continueLabel = continueLabel;
        _hasBreakLabel = true;
        _hasContinueLabel = true;
    }

    public bool TryGetBreakLabel(out ILLabel label)
    {
        if (_hasBreakLabel)
        {
            label = _breakLabel;
            return true;
        }

        label = default;
        return false;
    }

    public bool TryGetContinueLabel(out ILLabel label)
    {
        if (_hasContinueLabel)
        {
            label = _continueLabel;
            return true;
        }

        label = default;
        return false;
    }

    public void SetExceptionExitLabel(ILLabel label)
    {
        _exceptionExitLabel = label;
        _hasExceptionExitLabel = true;
    }

    public override bool TryGetExceptionExitLabel(out ILLabel label)
    {
        if (_hasExceptionExitLabel)
        {
            label = _exceptionExitLabel;
            return true;
        }

        return base.TryGetExceptionExitLabel(out label);
    }
}
