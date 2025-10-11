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

        peStream.Position = 0;
        var assembly = Assembly.Load(peStream.ToArray());
        var generatedType = Assert.Single(assembly.GetTypes(), type => type.Name == typeSymbol.Name);
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
    public void MoveNext_EmitsAwaitSchedulingPattern()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        Assert.True(
            instructions.Any(instruction =>
                instruction.Operand.Value is MethodInfo method &&
                method.Name.Contains("Await", StringComparison.Ordinal)),
            "Await scheduling invocation not emitted.");

        Assert.Contains(instructions, instruction =>
            instruction.Opcode == OpCodes.Ldflda && FormatOperand(instruction.Operand) == "_state" ||
            instruction.Opcode == OpCodes.Ldfld && FormatOperand(instruction.Operand) == "_state");
    }

    [Fact]
    public void AsyncMethod_StoresBuilderThroughLocalAddress()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "Work" &&
            generator.MethodSymbol.ContainingType is SourceNamedTypeSymbol);

        var builderStoreIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Stfld &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderStoreIndex >= 0, "Builder field store not found in Work method body.");
        Assert.Contains(instructions.Take(builderStoreIndex), instruction => instruction.Opcode == OpCodes.Ldloca);
    }

    [Fact]
    public void AsyncMethod_InvokesBuilderStartByReference()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "Work" &&
            generator.MethodSymbol.ContainingType is SourceNamedTypeSymbol);

        var startCallIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("Start", StringComparison.Ordinal));

        Assert.True(startCallIndex >= 0, "Builder.Start call not found in Work method body.");

        Assert.Contains(instructions.Take(startCallIndex), instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");
    }

    [Fact]
    public void AsyncMethod_ReturnsTaskByCallingBuilderThroughFieldAddress()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "Work" &&
            generator.MethodSymbol.ContainingType is SourceNamedTypeSymbol);

        var taskCallIndex = Array.FindIndex(instructions, instruction =>
            instruction.Operand.Value is MethodInfo method && method.Name == "get_Task");

        Assert.True(taskCallIndex >= 0, "Builder.get_Task call not found in async method body.");
        Assert.Equal(OpCodes.Call, instructions[taskCallIndex].Opcode);

        var builderAddressIndex = Array.FindLastIndex(instructions, taskCallIndex, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderAddressIndex >= 0, "Builder field address not loaded before get_Task call.");

        for (var i = builderAddressIndex + 1; i < taskCallIndex; i++)
            Assert.NotEqual(OpCodes.Stloc, instructions[i].Opcode);
    }

    [Fact]
    public void MoveNext_CallsBuilderSetResultByReference()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setResultCallIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetResult", StringComparison.Ordinal));

        Assert.True(setResultCallIndex >= 0, "Builder.SetResult call not found in MoveNext body.");

        Assert.Contains(instructions.Take(setResultCallIndex), instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");
    }

    [Fact]
    public void MoveNext_WhenMethodFallsThroughWithCompletedTask_CallsParameterlessSetResult()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setResultCalls = instructions
            .Where(instruction =>
                instruction.Opcode == OpCodes.Call &&
                instruction.Operand.Value is MethodInfo method &&
                method.Name.Contains("SetResult", StringComparison.Ordinal))
            .Select(instruction => Assert.IsAssignableFrom<MethodInfo>(instruction.Operand.Value))
            .ToArray();

        Assert.NotEmpty(setResultCalls);
        Assert.All(setResultCalls, method => Assert.Empty(method.GetParameters()));
    }

    [Fact]
    public void AsyncLambda_EmitsStateMachineMetadata()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    public async Run() -> Task {
        let handler = async () -> Task {
            await Task.CompletedTask
        }

        await handler()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
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

        var lambdaStateMachine = compilation.GetSynthesizedAsyncStateMachineTypes()
            .Single(stateMachine => stateMachine.AsyncMethod.MethodKind == MethodKind.LambdaMethod);

        Assert.NotNull(lambdaStateMachine.MoveNextBody);
    }

    private static (IMethodSymbol Method, RecordedInstruction[] Instructions) CaptureAsyncInstructions(Func<MethodGenerator, bool> predicate)
    {
        return CaptureAsyncInstructions(AsyncCode, predicate);
    }

    private static (IMethodSymbol Method, RecordedInstruction[] Instructions) CaptureAsyncInstructions(string source, Func<MethodGenerator, bool> predicate)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
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

    private static string FormatOperand(RecordedOperand operand)
    {
        return operand.Kind switch
        {
            RecordedOperandKind.None => string.Empty,
            RecordedOperandKind.Local => $"local:{operand.Value}",
            RecordedOperandKind.Label => $"label:{operand.Value}",
            RecordedOperandKind.FieldInfo when operand.Value is FieldInfo field => field.Name,
            RecordedOperandKind.FieldBuilder when operand.Value is FieldBuilder fieldBuilder => fieldBuilder.Name,
            RecordedOperandKind.MethodInfo when operand.Value is MethodInfo method => method.Name,
            RecordedOperandKind.ConstructorInfo when operand.Value is ConstructorInfo ctor => ctor.Name,
            RecordedOperandKind.Type when operand.Value is Type type => type.Name,
            RecordedOperandKind.Int32 or RecordedOperandKind.Int64 or RecordedOperandKind.Single or RecordedOperandKind.Double or RecordedOperandKind.String => operand.Value?.ToString() ?? string.Empty,
            _ => operand.Value?.ToString() ?? string.Empty
        };
    }
}
