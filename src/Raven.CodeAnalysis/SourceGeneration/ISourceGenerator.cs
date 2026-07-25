namespace Raven.CodeAnalysis;

/// <summary>Produces additional Raven source files from a compilation.</summary>
public interface ISourceGenerator
{
    void Initialize(GeneratorInitializationContext context);

    void Execute(GeneratorExecutionContext context);
}
