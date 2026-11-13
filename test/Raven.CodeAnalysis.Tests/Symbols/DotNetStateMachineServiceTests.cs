using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class DotNetStateMachineServiceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void AsyncSynthesisUsesProviderPolicyWithoutConcreteCompilation(bool heap, bool capture)
    {
        var compilation = CreateCompilation();
        var services = Provider(compilation, heap, capture);
        var method = Method(compilation, isAsync: true);
        var stateMachine = new SynthesizedAsyncStateMachineTypeSymbol(services, method, "State");
        Assert.Same(services, stateMachine.Services);
        Assert.Equal(heap, stateMachine.TypeKind == TypeKind.Class);
        Assert.Equal(!heap, stateMachine.IsValueType);
        Assert.Same(stateMachine, stateMachine.GetConstructedStateMachine(method));
        Assert.Same(stateMachine, stateMachine.Constructor.ContainingType);
        Assert.Equal(capture, stateMachine.GetBuilderMembers(method).SetException is not null);
    }

    [Fact]
    public void IteratorSynthesisUsesProviderTypesWithoutConcreteCompilation()
    {
        var compilation = CreateCompilation();
        var services = Provider(compilation, false, true);
        var method = Method(compilation, isAsync: false);
        var element = compilation.GetSpecialType(SpecialType.System_Int32);
        var stateMachine = new SynthesizedIteratorTypeSymbol(services, method, "Iterator", IteratorMethodKind.Enumerable, element);
        Assert.Same(services, stateMachine.Services);
        Assert.Same(element, stateMachine.ElementType);
        Assert.Same(element, stateMachine.CurrentField.Type);
        Assert.Same(stateMachine, stateMachine.GetConstructedStateMachine(method));
        Assert.NotEmpty(stateMachine.Interfaces);
    }

    [Fact]
    public void CompilationAdapterRetainsFactoriesAndSelectedPolicy()
    {
        var compilation = Compilation.Create("Services", [], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithHeapAsyncStateMachines(true).WithAsyncExceptionCapture(false));
        var services = (StateMachineServices)compilation;
        Assert.True(services.UseHeapAsyncStateMachines);
        Assert.False(services.CaptureAsyncExceptions);
        Assert.Same(compilation.ErrorTypeSymbol, services.ErrorTypeSymbol);
        var element = compilation.GetSpecialType(SpecialType.System_Int32);
        var ranked = Assert.IsAssignableFrom<ArrayTypeSymbol>(services.CreateArrayTypeSymbol(element, rank: 2));
        Assert.Equal(2, ranked.Rank);
        Assert.Same(element, ranked.ElementType);
        var fixedArray = Assert.IsAssignableFrom<ArrayTypeSymbol>(services.CreateArrayTypeSymbol(element, fixedLength: 7));
        Assert.Equal(7, fixedArray.FixedLength);
        Assert.Same(element, Assert.IsAssignableFrom<PointerTypeSymbol>(services.CreatePointerTypeSymbol(element)).PointedAtType);
    }

    private static Compilation CreateCompilation()
        => Compilation.Create("Services", [], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    private static SourceMethodSymbol Method(Compilation compilation, bool isAsync)
        => new("Run", compilation.GetSpecialType(SpecialType.System_Void), [], compilation.Assembly,
            null, compilation.SourceGlobalNamespace, [], [], isAsync: isAsync);

    private static StateMachineServices Provider(Compilation compilation, bool heap, bool capture)
    {
        var services = Make<StateMachineServices>(SymbolKind.Namespace, "Services");
        Set(services, "get_UseHeapAsyncStateMachines", heap);
        Set(services, "get_CaptureAsyncExceptions", capture);
        Set(services, "get_ErrorTypeSymbol", compilation.ErrorTypeSymbol);
        Set(services, "GetSpecialType", (Func<object?[]?, object?>)(args => compilation.GetSpecialType((SpecialType)args![0]!)));
        Set(services, "GetTypeByMetadataName", (Func<object?[]?, object?>)(args => compilation.GetTypeByMetadataName((string)args![0]!)));
        Set(services, "CreateArrayTypeSymbol", (Func<object?[]?, object?>)(args => compilation.CreateArrayTypeSymbol((TypeSymbol)args![0]!, (int)args[1]!, (int?)args[2])));
        Set(services, "CreatePointerTypeSymbol", (Func<object?[]?, object?>)(args => compilation.CreatePointerTypeSymbol((TypeSymbol)args![0]!)));
        return services;
    }
}
