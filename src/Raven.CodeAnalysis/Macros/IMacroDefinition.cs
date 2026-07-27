using System;

namespace Raven.CodeAnalysis.Macros;

public interface IMacroDefinition
{
    string Name { get; }

    MacroTarget Targets { get; }

    bool AcceptsArguments => false;
}

public interface IMacroDefinition<TParameters> : IMacroDefinition
    where TParameters : class
{
    Type ParametersType => typeof(TParameters);

    bool IMacroDefinition.AcceptsArguments => true;
}
