using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
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

    private const string AsyncTaskOfIntCode = """
import System.Threading.Tasks.*

class C {
    async Compute(value: int) -> Task<int> {
        await Task.Delay(10)
        return value
    }
}
""";

    private const string TopLevelAsyncFunctionCode = """
import System.Threading.Tasks.*

async func Test(value: int) -> Task[Int32] {
    await Task.Delay(5)
    return value
}

let result = await Test(42)
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

        Type? generatedType;
        try
        {
            generatedType = assembly.GetType(typeSymbol.Name, throwOnError: true, ignoreCase: false);
        }
        catch (TypeLoadException ex)
        {
            throw new InvalidOperationException($"Failed to load generated type '{typeSymbol.Name}': {ex.Message}", ex);
        }
        catch (ReflectionTypeLoadException ex)
        {
            var loaderMessages = string.Join(Environment.NewLine, ex.LoaderExceptions.Select(e => e?.ToString() ?? string.Empty));
            throw new InvalidOperationException($"Failed to load generated type '{typeSymbol.Name}': {loaderMessages}", ex);
        }

        Assert.NotNull(generatedType);
        var methodInfo = generatedType!.GetMethod(methodSymbol.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
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
    public void AsyncMethod_WithTaskOfInt_AppliesGenericBuilderMetadata()
    {
        var syntaxTree = SyntaxTree.ParseText(AsyncTaskOfIntCode);
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

        var builderFieldType = Assert.IsAssignableFrom<INamedTypeSymbol>(methodSymbol.AsyncStateMachine!.BuilderField.Type);
        Assert.True(builderFieldType.IsGenericType);
        var builderDefinition = builderFieldType.OriginalDefinition;
        Assert.NotNull(builderDefinition);
        Assert.Equal(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T, builderDefinition!.SpecialType);
        var builderTypeArgument = Assert.Single(builderFieldType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, builderTypeArgument.SpecialType);

        peStream.Position = 0;
        using var peReader = new PEReader(peStream, PEStreamOptions.LeaveOpen);
        var metadataReader = peReader.GetMetadataReader();

        var typeHandle = FindTypeDefinition(metadataReader, typeSymbol.Name);
        var methodHandle = FindMethodDefinition(metadataReader, typeHandle, methodSymbol.Name);

        var builderAttributeHandle = FindCustomAttribute(metadataReader, methodHandle, "System.Runtime.CompilerServices", "AsyncMethodBuilderAttribute");
        var builderAttribute = metadataReader.GetCustomAttribute(builderAttributeHandle);
        var reader = metadataReader.GetBlobReader(builderAttribute.Value);

        Assert.Equal(0x0001, reader.ReadUInt16());
        var builderTypeName = reader.ReadSerializedString();
        Assert.NotNull(builderTypeName);
        Assert.Contains("System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1[[", builderTypeName!);
        Assert.Contains("System.Int32", builderTypeName);
        Assert.Contains("]]", builderTypeName);
        Assert.Equal(0, reader.ReadUInt16());
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
    public void MoveNext_StoresStateAndAwaitersWithoutSpillingStateMachine()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        static bool IsTargetField(string operand)
            => operand == "_state" || operand.StartsWith("<>awaiter", StringComparison.Ordinal);

        var relevantStores = instructions
            .Select((instruction, index) => (instruction, index))
            .Where(pair => pair.instruction.Opcode == OpCodes.Stfld)
            .Where(pair => IsTargetField(FormatOperand(pair.instruction.Operand)))
            .ToArray();

        Assert.NotEmpty(relevantStores);

        foreach (var (instruction, index) in relevantStores)
        {
            var receiverLoadIndex = FindPrecedingLoadArgumentZero(instructions, index);
            Assert.True(receiverLoadIndex >= 0, $"ldarg.0 not found before store to {FormatOperand(instruction.Operand)}.");

            for (var i = receiverLoadIndex + 1; i < index; i++)
            {
                var intermediate = instructions[i];
                Assert.False(IsStoreLocal(intermediate.Opcode),
                    $"State machine spilled before storing {FormatOperand(instruction.Operand)}; found {intermediate.Opcode} at index {i}.");
            }
        }
    }

    [Fact]
    public void MoveNext_ClearsAwaiterFieldsOnResume()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var awaiterStoreCounts = instructions
            .Where(instruction =>
                instruction.Opcode == OpCodes.Stfld &&
                FormatOperand(instruction.Operand).StartsWith("<>awaiter", StringComparison.Ordinal))
            .GroupBy(instruction => FormatOperand(instruction.Operand))
            .ToDictionary(group => group.Key, group => group.Count());

        Assert.NotEmpty(awaiterStoreCounts);
        foreach (var (fieldName, count) in awaiterStoreCounts)
            Assert.True(count >= 2, $"Awaiter field '{fieldName}' was not cleared after resume.");
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
    public void MoveNext_WhenReturningCompletedTaskExpression_CallsParameterlessSetResult()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        await Task.CompletedTask
        return Task.CompletedTask
    }
}
""";

        var (_, instructions) = CaptureAsyncInstructions(source, static generator =>
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
    public void MoveNext_WhenReturningValueExpression_CallsSetResultWithArgument()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setResultIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetResult", StringComparison.Ordinal));

        Assert.True(setResultIndex >= 0, "Builder.SetResult call not found in MoveNext body.");

        var setResultMethod = Assert.IsAssignableFrom<MethodInfo>(instructions[setResultIndex].Operand.Value);
        Assert.Equal(1, setResultMethod.GetParameters().Length);

        Assert.Contains(instructions.Take(setResultIndex), instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.Contains(instructions.Take(setResultIndex), instruction =>
            instruction.Opcode == OpCodes.Ldfld &&
            FormatOperand(instruction.Operand) == "_value");
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

    [Fact]
    public void TopLevelAsyncFunction_EmitsStateMachine()
    {
        var syntaxTree = SyntaxTree.ParseText(TopLevelAsyncFunctionCode);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("async", new CompilationOptions(OutputKind.ConsoleApplication))
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
        var functionSyntax = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(functionSyntax));
        Assert.NotNull(methodSymbol.AsyncStateMachine);
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

    private static int FindPrecedingLoadArgumentZero(RecordedInstruction[] instructions, int startIndex)
    {
        for (var i = startIndex - 1; i >= 0; i--)
        {
            if (IsLoadArgumentZero(instructions[i]))
                return i;
        }

        return -1;
    }

    private static bool IsLoadArgumentZero(RecordedInstruction instruction)
    {
        if (instruction.Opcode == OpCodes.Ldarg_0)
            return true;

        if (instruction.Opcode == OpCodes.Ldarg && instruction.Operand.Kind == RecordedOperandKind.Int32 && instruction.Operand.Value is int ldarg && ldarg == 0)
            return true;

        if (instruction.Opcode == OpCodes.Ldarg_S && instruction.Operand.Kind == RecordedOperandKind.Int32 && instruction.Operand.Value is int ldargS && ldargS == 0)
            return true;

        return false;
    }

    private static bool IsStoreLocal(OpCode opcode)
    {
        return opcode == OpCodes.Stloc
            || opcode == OpCodes.Stloc_0
            || opcode == OpCodes.Stloc_1
            || opcode == OpCodes.Stloc_2
            || opcode == OpCodes.Stloc_3
            || opcode == OpCodes.Stloc_S;
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

    private static TypeDefinitionHandle FindTypeDefinition(MetadataReader reader, string name)
    {
        foreach (var handle in reader.TypeDefinitions)
        {
            var definition = reader.GetTypeDefinition(handle);
            if (reader.GetString(definition.Name) == name)
                return handle;
        }

        throw new InvalidOperationException($"Type '{name}' not found in metadata.");
    }

    private static MethodDefinitionHandle FindMethodDefinition(MetadataReader reader, TypeDefinitionHandle typeHandle, string name)
    {
        var definition = reader.GetTypeDefinition(typeHandle);

        foreach (var methodHandle in definition.GetMethods())
        {
            var method = reader.GetMethodDefinition(methodHandle);
            if (reader.GetString(method.Name) == name)
                return methodHandle;
        }

        throw new InvalidOperationException($"Method '{name}' not found on type '{reader.GetString(definition.Name)}'.");
    }

    private static CustomAttributeHandle FindCustomAttribute(
        MetadataReader reader,
        MethodDefinitionHandle methodHandle,
        string expectedNamespace,
        string expectedName)
    {
        var method = reader.GetMethodDefinition(methodHandle);

        foreach (var attributeHandle in method.GetCustomAttributes())
        {
            var attribute = reader.GetCustomAttribute(attributeHandle);
            var attributeTypeHandle = attribute.Constructor.Kind switch
            {
                HandleKind.MethodDefinition => reader.GetMethodDefinition((MethodDefinitionHandle)attribute.Constructor).GetDeclaringType(),
                HandleKind.MemberReference => reader.GetMemberReference((MemberReferenceHandle)attribute.Constructor).Parent,
                _ => default
            };

            if (attributeTypeHandle.Kind == HandleKind.TypeReference)
            {
                var typeReference = reader.GetTypeReference((TypeReferenceHandle)attributeTypeHandle);
                var name = reader.GetString(typeReference.Name);
                var @namespace = reader.GetString(typeReference.Namespace);

                if (name == expectedName && @namespace == expectedNamespace)
                    return attributeHandle;
            }
            else if (attributeTypeHandle.Kind == HandleKind.TypeDefinition)
            {
                var typeDefinition = reader.GetTypeDefinition((TypeDefinitionHandle)attributeTypeHandle);
                var name = reader.GetString(typeDefinition.Name);
                var @namespace = reader.GetString(typeDefinition.Namespace);

                if (name == expectedName && @namespace == expectedNamespace)
                    return attributeHandle;
            }
        }

        throw new InvalidOperationException($"Custom attribute '{expectedNamespace}.{expectedName}' not found.");
    }
}
