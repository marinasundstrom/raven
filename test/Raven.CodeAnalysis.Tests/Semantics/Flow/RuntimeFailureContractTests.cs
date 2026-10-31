using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RuntimeFailureContractTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [Trait("Target", "NeoCLR")]
    [InlineData("public func Panic(message: string) { }", true)]
    [InlineData("private func Panic(message: string) { }", false)]
    [InlineData("public func Panic(message: int) { }", false)]
    [InlineData("public func Panic(message: string) -> int => 1", false)]
    [InlineData("public func Panic<T>(message: string) { }", false)]
    [InlineData("public func Panic() { }", false)]
    [InlineData("public func Fail(message: string) { }", false)]
    [InlineData("public func Stop(message: string) { }", false)]
    [InlineData("public class Container { public static func Panic(message: string) { } }", false)]
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
            method = compilation.GetTypeByMetadataName("Services.Container")!.GetMembers("Panic").OfType<IMethodSymbol>().Single();
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
    [Trait("Target", "DotNet")]
    [Trait("Target", "NeoCLR")]
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
    [Trait("Target", "DotNet")]
    [Trait("Target", "NeoCLR")]
    public void EmptyOwnerAndDotNetConfigurationRejectExplicitly()
    {
        var native = new Targets.NeoClrCliRuntimeContract(CompilationOptions.NeoCLR
            .WithRuntimeFailureContract(new("")));
        Assert.Contains("failure contract", native.GetConfigurationError());
        var dotnet = new Targets.DotNetRuntimeContract(CompilationOptions.DotNet
            .WithRuntimeFailureContract(new("FailureLibrary")));
        Assert.Contains("failure contract", dotnet.GetConfigurationError());
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    [Trait("Layer", "semantic-api")]
    public void NeoClrWorkspaceFailureContractChangeRebindsControlFlow()
    {
        var corePath = TestMetadataReferences.Default.OfType<PortableExecutableReference>()
            .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
        using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(corePath);
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        using var image = new MemoryStream();
        core.Write(image);
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray();
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithRuntimeDisposalContract(null)
            .WithRuntimeIterationContract(null).WithRuntimePropagationContract(null);
        const string source = """
            namespace Services
            public func Panic(message: string) { }
            public func Stop() -> int { Panic("stopped") }
            """;
        var workspace = new AdhocWorkspace();
        var project = workspace.CurrentSolution.AddProject("FailureLibrary", compilationOptions: options).Projects.Single();
        foreach (var reference in references)
            project = project.AddMetadataReference(reference);
        var document = project.AddDocument("main.rvn", Text.SourceText.From(source));
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));
        var original = workspace.GetCompilation(project.Id);
        Assert.True(EndPointIsReachable(original));
        Assert.DoesNotContain(original.GetDiagnostics(), diagnostic => diagnostic.Id.StartsWith("RAVT", StringComparison.Ordinal));

        foreach (var contract in new RuntimeFailureContract?[] { new("FailureLibrary", "Services"), null })
        {
            var changedOptions = options.WithRuntimeFailureContract(contract);
            Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(project.Id, changedOptions)));
            var current = workspace.GetCompilation(project.Id);
            var cold = Compilation.Create("FailureLibrary", [SyntaxTree.ParseText(source)], references, changedOptions);
            Assert.Equal(cold.GetDiagnostics().Select(diagnostic => diagnostic.ToString()),
                current.GetDiagnostics().Select(diagnostic => diagnostic.ToString()));
            Assert.Equal(contract is null, EndPointIsReachable(cold));
            Assert.Equal(EndPointIsReachable(cold), EndPointIsReachable(current));
        }
        Assert.True(EndPointIsReachable(original));

        static bool EndPointIsReachable(Compilation compilation)
        {
            var tree = compilation.SyntaxTrees.Single();
            var body = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>()
                .Single(function => function.Identifier.ValueText == "Stop")
                .DescendantNodes().OfType<BlockStatementSyntax>().First();
            return compilation.GetSemanticModel(tree).AnalyzeControlFlow(body).EndPointIsReachable;
        }
    }

}
