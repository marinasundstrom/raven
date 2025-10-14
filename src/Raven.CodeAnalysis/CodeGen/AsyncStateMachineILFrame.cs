using System;
using System.Reflection.Emit;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal sealed class AsyncStateMachineILFrame : IDisposable
{
    private readonly MethodBodyGenerator _methodBodyGenerator;
    private readonly SynthesizedAsyncStateMachineTypeSymbol _stateMachine;

    private int _receiverDepth;
    private bool _suppressNextRelease;

    public AsyncStateMachineILFrame(MethodBodyGenerator methodBodyGenerator, SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        _methodBodyGenerator = methodBodyGenerator ?? throw new ArgumentNullException(nameof(methodBodyGenerator));
        _stateMachine = stateMachine ?? throw new ArgumentNullException(nameof(stateMachine));
    }

    public SynthesizedAsyncStateMachineTypeSymbol StateMachine => _stateMachine;

    private IILBuilder IL => _methodBodyGenerator.ILGenerator;

    public void BeginStatement(BoundStatement statement)
    {
        if (statement is null)
            return;

        if (statement is BoundLabeledStatement or BoundGotoStatement or BoundReturnStatement or BoundThrowStatement)
        {
            ReleaseReceiver();
            _suppressNextRelease = false;
            return;
        }

        if (_receiverDepth > 0 && !_suppressNextRelease)
            ReleaseReceiver();

        _suppressNextRelease = false;
    }

    public void EndStatement(BoundStatement statement)
    {
        if (statement is BoundReturnStatement or BoundThrowStatement)
            ReleaseReceiver();
    }

    public void SuppressNextRelease()
    {
        if (_receiverDepth == 0)
            return;

        _suppressNextRelease = true;
    }

    public void EnsureReceiverLoaded(bool keepAlive)
    {
        if (_receiverDepth == 0)
        {
            IL.Emit(OpCodes.Ldarg_0);
            _receiverDepth = 1;
        }

        if (keepAlive)
        {
            IL.Emit(OpCodes.Dup);
            _receiverDepth++;
        }
    }

    public void AfterFieldStore()
    {
        if (_receiverDepth == 0)
            return;

        _receiverDepth--;
    }

    public bool TryConsumeReceiver()
    {
        if (_receiverDepth == 0)
            return false;

        _receiverDepth--;
        return true;
    }

    public void ReleaseReceiver()
    {
        while (_receiverDepth > 0)
        {
            IL.Emit(OpCodes.Pop);
            _receiverDepth--;
        }
    }

    public bool HasReceiverOnStack => _receiverDepth > 0;

    public void Dispose()
    {
        ReleaseReceiver();
        _suppressNextRelease = false;
    }
}
