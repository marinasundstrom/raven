using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RuntimeFailureContractTests
{
    [Theory]
    [InlineData("public func Fail(message: string) { }", true)]
    [InlineData("private func Fail(message: string) { }", false)]
    [InlineData("public func Fail(message: int) { }", false)]
    [InlineData("public func Fail(message: string) -> int => 1", false)]
    [InlineData("public func Fail<T>(message: string) { }", false)]
    [InlineData("public func Fail() { }", false)]
    [InlineData("public func Stop(message: string) { }", false)]
    [InlineData("public class Container { public static func Fail(message: string) { } }", false)]
    public void ExactNamespaceFunctionShapeAndOwnerAreRequired(string declaration, bool expected)
    {
        var tree = SyntaxTree.ParseText("namespace Services\n" + declaration);
        var compilation = Compilation.Create("FailureLibrary", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().FirstOrDefault();
        IMethodSymbol method;
        if (syntax is not null)
            method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        else
            method = compilation.GetTypeByMetadataName("Services.Container")!.GetMembers("Fail").OfType<IMethodSymbol>().Single();
        var contract = new RuntimeFailureContract("FailureLibrary", "Services");
        Assert.Equal(expected, contract.Matches(method));
        Assert.False((contract with { AssemblyName = "Other" }).Matches(method));
        Assert.False((contract with { NamespaceName = "Other" }).Matches(method));
        Assert.False(RuntimeFailureContract.IsTerminal(method, compilation.Options.WithRuntimeFailureContract(contract)));
        Assert.Equal(expected, RuntimeFailureContract.IsTerminal(method,
            compilation.Options.WithTargetPlatform(TargetPlatform.NeoCLR).WithRuntimeFailureContract(contract)));
        Assert.False(BoundNodeFacts.IsTerminalRuntimeFault(method)); // Ordinary .NET compilation is unchanged.
    }

    [Fact]
    public void OptionClonesPreserveAndCanRemoveTheExplicitContract()
    {
        var contract = new RuntimeFailureContract("FailureLibrary");
        var options = CompilationOptions.NeoCLR.WithRuntimeFailureContract(contract)
            .WithRuntimeDisposalContract(null).WithRuntimeSelfTypeContract(null)
            .WithOptimizationLevel(OptimizationLevel.Release).WithAllowUnsafe(true)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        Assert.Equal(contract, options.RuntimeFailureContract);
        Assert.Null(options.WithRuntimeFailureContract(null).RuntimeFailureContract);
        Assert.Null(CompilationOptions.DotNet.RuntimeFailureContract);
    }
    [Fact]
    public void EmptyOwnerAndDotNetConfigurationRejectExplicitly()
    {
        var native = new Targets.NeoClrCliRuntimeContract(CompilationOptions.NeoCLR
            .WithRuntimeFailureContract(new("")));
        Assert.Contains("failure contract", native.GetConfigurationError());
        var dotnet = new Targets.DotNetRuntimeContract(CompilationOptions.DotNet
            .WithRuntimeFailureContract(new("FailureLibrary")));
        Assert.Contains("failure contract", dotnet.GetConfigurationError());
    }

}
