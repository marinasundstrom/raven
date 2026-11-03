using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

class Scope : Generator
{
    private readonly IDictionary<LocalSymbol, IILocal> _localBuilders = new Dictionary<LocalSymbol, IILocal>(SymbolEqualityComparer.Default);
    private readonly ImmutableArray<LocalSymbol> _localsToDispose;
    private bool _hasBreakLabel;
    private ILLabel _breakLabel;
    private bool _hasContinueLabel;
    private ILLabel _continueLabel;
    private readonly Dictionary<LabelSymbol, (ILLabel BreakLabel, ILLabel ContinueLabel)> _labeledLoopTargets = new(SymbolEqualityComparer.Default);
    private bool _hasExceptionExitLabel;
    private ILLabel _exceptionExitLabel;
    private bool _isInsideExceptionHandler;

    public Scope(Generator parent) : this(parent, ImmutableArray<LocalSymbol>.Empty)
    {
    }

    public Scope(Generator parent, ImmutableArray<LocalSymbol> localsToDispose) : base(parent)
    {
        _localsToDispose = localsToDispose.IsDefault ? ImmutableArray<LocalSymbol>.Empty : localsToDispose;
    }

    public override void AddLocal(LocalSymbol localSymbol, IILocal builder)
    {
        if (!_localBuilders.ContainsKey(localSymbol))
            _localBuilders.Add(localSymbol, builder);
    }

    public override IILocal? GetLocal(LocalSymbol localSymbol)
    {
        if (_localBuilders.TryGetValue(localSymbol, out var localBuilder))
            return localBuilder;

        return Parent?.GetLocal(localSymbol);
    }

    public override IEnumerable<LocalSymbol> EnumerateLocalsToDispose()
    {
        for (int i = _localsToDispose.Length - 1; i >= 0; i--)
            yield return _localsToDispose[i];

        if (Parent is not null)
        {
            foreach (var local in Parent.EnumerateLocalsToDispose())
                yield return local;
        }
    }

    public ImmutableArray<LocalSymbol> LocalsToDispose => _localsToDispose;

    public bool IsLoopScope => _hasBreakLabel || _hasContinueLabel;

    public void SetLoopTargets(ILLabel breakLabel, ILLabel continueLabel)
    {
        _breakLabel = breakLabel;
        _continueLabel = continueLabel;
        _hasBreakLabel = true;
        _hasContinueLabel = true;
    }

    public void SetLoopTargets(LabelSymbol label, ILLabel breakLabel, ILLabel continueLabel)
    {
        _labeledLoopTargets[label] = (breakLabel, continueLabel);
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

    public bool TryGetBreakLabel(LabelSymbol targetLabel, out ILLabel label)
    {
        if (_labeledLoopTargets.TryGetValue(targetLabel, out var target))
        {
            label = target.BreakLabel;
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

    public bool TryGetContinueLabel(LabelSymbol targetLabel, out ILLabel label)
    {
        if (_labeledLoopTargets.TryGetValue(targetLabel, out var target))
        {
            label = target.ContinueLabel;
            return true;
        }

        label = default;
        return false;
    }

    public void MarkAsInsideExceptionHandler()
    {
        _isInsideExceptionHandler = true;
    }

    internal Scope? ExceptionRegion
    {
        get
        {
            for (Generator? current = this; current is not null; current = current.Parent)
            {
                if (current is Scope { _isInsideExceptionHandler: true } region)
                    return region;
            }

            return null;
        }
    }

    public override bool IsInsideExceptionHandler =>
        _isInsideExceptionHandler || base.IsInsideExceptionHandler;

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
