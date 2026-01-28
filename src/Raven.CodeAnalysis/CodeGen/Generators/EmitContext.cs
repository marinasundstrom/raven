namespace Raven.CodeAnalysis.CodeGen;

enum EmitResultKind { None, Value, Address }

readonly record struct EmitContext(EmitResultKind ResultKind, bool ForceValue)
{
    public readonly static EmitContext None = new(EmitResultKind.None, false);
    public readonly static EmitContext Value = new(EmitResultKind.Value, false);
    public readonly static EmitContext Address = new(EmitResultKind.Address, false);

    public EmitContext WithResultKind(EmitResultKind kind) => this with { ResultKind = kind };
    public static EmitContext RequiredValue = new(EmitResultKind.Value, true);

}
