namespace Raven.CodeAnalysis.Symbols;

/// <summary>
/// Owns constructor-time member registration. Implementations retain their own
/// synchronization and duplicate-member policies.
/// </summary>
internal interface SymbolMemberContainer
{
    void AddMember(Symbol member);
}
