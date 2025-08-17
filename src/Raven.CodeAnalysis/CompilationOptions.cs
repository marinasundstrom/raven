namespace Raven.CodeAnalysis;

public class CompilationOptions
{
    public CompilationOptions()
    {
        OutputKind = OutputKind.ConsoleApplication;
    }

    public CompilationOptions(OutputKind outputKind)
    {
        OutputKind = outputKind;
    }

    public OutputKind OutputKind { get; }
}