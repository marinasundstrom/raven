namespace Raven.CodeAnalysis;

// Carries an expected target-contract failure through synthesized body construction.
// Emission reports it as a compiler diagnostic; unrelated implementation failures are not caught.
internal sealed class MissingSynthesizedRuntimeMemberException(string member, int argumentCount)
    : InvalidOperationException("Missing runtime member for synthesized body: " + member)
{
    internal Diagnostic Diagnostic { get; } = Diagnostic.Create(
        CompilerDiagnostics.NoOverloadForMethod, Location.None, "method", member, argumentCount);
}
