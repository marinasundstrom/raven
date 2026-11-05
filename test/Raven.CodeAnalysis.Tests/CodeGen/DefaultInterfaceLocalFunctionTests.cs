using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class DefaultInterfaceLocalFunctionTests
{
    [Theory]
    [InlineData("static func GetValue() -> int { return 42 }")]
    [InlineData("static func GetValue() -> int => 42")]
    [InlineData("func GetValue() -> int { return 42 }")]
    [InlineData("func GetValue() -> int => 42")]
    public void LocalHelperHasBodyAndDoesNotBecomeAnAbstractContract(string helper)
    {
        var source = $$"""
            interface Contract {
                func Read() -> int {
                    {{helper}}
                    return GetValue()
                }
            }
            class Implementation : Contract {}
            """;
        var compilation = Compilation.Create("InterfaceLocalFunction", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var contract = loaded.Assembly.GetType("Contract")!;
        var helperMethod = Assert.Single(contract.GetMethods(BindingFlags.Public | BindingFlags.NonPublic |
            BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly), method => method.Name.Contains("GetValue"));
        Assert.False(helperMethod.IsAbstract);
        Assert.False(helperMethod.IsVirtual);
        Assert.False(helperMethod.IsPublic);
        Assert.NotNull(helperMethod.GetMethodBody());
        var instance = Activator.CreateInstance(loaded.Assembly.GetType("Implementation")!);
        Assert.Equal(42, contract.GetMethod("Read")!.Invoke(instance, null));
    }
}
