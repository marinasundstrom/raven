namespace Raven.CodeAnalysis.CodeGen;

enum EmitValueKind
{
    None,
    Value,
    Address
}

readonly struct EmitInfo
{
    public EmitValueKind Kind { get; }
    public ISymbol? Symbol { get; }
    public IILocal? Local { get; }
    public bool WasCaptured { get; }
    public bool WasSpilledToLocal { get; }

    public bool HasValueOnStack => Kind != EmitValueKind.None;
    public bool IsAddress => Kind == EmitValueKind.Address;

    private EmitInfo(EmitValueKind kind, ISymbol? symbol, IILocal? local, bool wasCaptured, bool wasSpilledToLocal)
    {
        Kind = kind;
        Symbol = symbol;
        Local = local;
        WasCaptured = wasCaptured;
        WasSpilledToLocal = wasSpilledToLocal;
    }

    public static EmitInfo None => new(EmitValueKind.None, symbol: null, local: null, wasCaptured: false, wasSpilledToLocal: false);

    public static EmitInfo ForValue(ISymbol? symbol = null, IILocal? local = null, bool wasCaptured = false)
        => new(EmitValueKind.Value, symbol, local, wasCaptured, wasSpilledToLocal: false);

    public static EmitInfo ForAddress(
        ISymbol? symbol = null,
        IILocal? local = null,
        bool wasCaptured = false,
        bool wasSpilledToLocal = false)
        => new(EmitValueKind.Address, symbol, local, wasCaptured, wasSpilledToLocal);
}
