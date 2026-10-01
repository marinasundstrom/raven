using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedStaticPropertyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ComputedStaticAccessorsShareCallsAndPreserveMetadata(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Box<T> {
                private var stored: T
                private init(value: T) { stored = value }
                static val Empty: Box<T> => Box<T>(default(T))
                val Value: T => stored
            }
            static class Settings {
                static var Values: int[] {
                    get => [0]
                    set { value[0] = 42 }
                }
                static func Get() -> int[] => Values
            }
            func Main() -> int {
                let values = Settings.Get()
                Settings.Values = values
                if Box<int>.Empty.Value != 0 { return 1 }
                return values[0]
            }
            """);
        var compilation = Compilation.Create("StaticProperties", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().Where(n => n is FunctionStatementSyntax or MethodDeclarationSyntax or AccessorDeclarationSyntax))
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        using var image = new MemoryStream(); var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var loaded = Assembly.Load(image.ToArray());
        Assert.Equal(42, loaded.EntryPoint!.Invoke(null, null));
        var property = loaded.GetType("Settings")!.GetProperty("Values")!;
        Assert.True(property.GetMethod!.IsStatic);
        Assert.True(property.SetMethod!.IsStatic);
        Assert.Empty(loaded.GetType("Settings")!.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static));
    }
}
