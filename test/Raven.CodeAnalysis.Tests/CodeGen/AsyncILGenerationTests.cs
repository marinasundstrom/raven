using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class AsyncILGenerationTests
{
    private const string AsyncCode = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        await Task.CompletedTask
    }
}
""";

    [Fact]
    public void AsyncMethod_AppliesStateMachineMetadata()
    {
        var syntaxTree = SyntaxTree.ParseText(AsyncCode);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("async", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = ReflectionEmitILBuilderFactory.Instance
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var classDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var methodDeclaration = classDeclaration.Members.OfType<MethodDeclarationSyntax>().Single();

        var typeSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        var generatedType = Assert.IsAssignableFrom<Type>(codeGenerator.GetTypeBuilder(typeSymbol)!);
        var methodInfo = generatedType.GetMethod(methodSymbol.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(methodInfo);

        var attributes = methodInfo!.GetCustomAttributesData();

        var stateMachineAttribute = Assert.Single(attributes, attr => attr.AttributeType == typeof(AsyncStateMachineAttribute));
        var stateMachineType = Assert.IsAssignableFrom<Type>(stateMachineAttribute.ConstructorArguments[0].Value);
        Assert.Equal(methodSymbol.AsyncStateMachine!.Name, stateMachineType.Name);

        var builderAttribute = Assert.Single(attributes, attr => attr.AttributeType == typeof(AsyncMethodBuilderAttribute));
        var builderType = Assert.IsAssignableFrom<Type>(builderAttribute.ConstructorArguments[0].Value);
        Assert.Equal(typeof(AsyncTaskMethodBuilder), builderType);
    }

    [Fact]
    public void AsyncMethod_InvokesBuilderStartWithDynamicStateMachine()
    {
        var (method, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "Work" &&
            generator.MethodSymbol.ContainingType?.Name == "C");

        Assert.Equal("Work", method.Name);

        var startInvocation = instructions
            .Where(instruction => instruction.Operand.Kind == RecordedOperandKind.MethodInfo)
            .Select(instruction => instruction.Operand.Value)
            .OfType<MethodInfo>()
            .Single(info => string.Equals(info.Name, "Start", StringComparison.Ordinal));

        Assert.True(startInvocation.IsGenericMethod);
        var stateMachineArgument = Assert.Single(startInvocation.GetGenericArguments());
        Assert.True(stateMachineArgument.Assembly.IsDynamic);
    }

    [Fact]
    public void MoveNext_EmitsAwaitSchedulingPattern()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        Assert.Contains(instructions, instruction =>
            instruction.Operand.Kind == RecordedOperandKind.MethodInfo &&
            instruction.Operand.Value is MethodInfo method && method.Name.Contains("Await", StringComparison.Ordinal));

        Assert.Contains(instructions, instruction =>
            instruction.Operand.Kind == RecordedOperandKind.FieldBuilder &&
            instruction.Operand.Value is FieldBuilder field && field.Name == "_state");
    }

    private static (IMethodSymbol Method, RecordedInstruction[] Instructions) CaptureAsyncInstructions(Func<MethodGenerator, bool> predicate)
    {
        var syntaxTree = SyntaxTree.ParseText(AsyncCode);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("async", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var recordingFactory = new RecordingILBuilderFactory(ReflectionEmitILBuilderFactory.Instance, predicate);

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = recordingFactory
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        var method = recordingFactory.CapturedMethod ?? throw new InvalidOperationException("Failed to capture async method.");
        var instructions = recordingFactory.CapturedInstructions ?? throw new InvalidOperationException("Failed to capture IL.");

        return (method, instructions.ToArray());
    }
}
