namespace Raven.CodeAnalysis;

public class CompilationOptions
{
    public CompilationOptions()
        : this(OutputKind.ConsoleApplication)
    {
    }

    public CompilationOptions(OutputKind outputKind)
    {
        OutputKind = outputKind;
    }

    public OutputKind OutputKind { get; }
}