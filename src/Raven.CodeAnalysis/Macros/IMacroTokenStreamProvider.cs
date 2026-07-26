namespace Raven.CodeAnalysis.Macros;

public interface IMacroTokenStreamProvider
{
    IMacroTokenStream CreateTokenStream(MacroTokenStreamContext context);
}
