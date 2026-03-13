namespace Raven.CodeAnalysis.Macros;

public interface IMacroDefinition
{
    string Name { get; }

    MacroKind Kind { get; }

    MacroTarget Targets { get; }
}
