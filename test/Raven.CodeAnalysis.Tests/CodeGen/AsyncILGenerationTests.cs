using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using Raven;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class AsyncILGenerationTests
{
    private readonly ITestOutputHelper _output;

    public AsyncILGenerationTests(ITestOutputHelper output)
    {
        _output = output;
    }

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

    private const string AsyncTaskEntryPointCode = """
import System.Console.*
import System.Threading.Tasks.*

async func Print(label: string, value: int) -> Task {
    await Task.Delay(1)
    WriteLine("${label}:${value}")
}

await Print("first", 1)
WriteLine("done")
""";

    private const string TopLevelAsyncFunctionCode = """
import System.Threading.Tasks.*

async func Test(value: int) -> Task<Int32> {
    await Task.Delay(5)
    return value
}

let result = await Test(42)
""";

    private const string AsyncTaskOfIntEntryPointCode = """
import System.Console.*
import System.Threading.Tasks.*

async func Test(value: int) -> Task<int> {
    await Task.Delay(10)
    return value
}

let value = await Test(42)

WriteLine(value)
""";

    private const string TryAwaitAsyncCode = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        var attempt = try await Task.FromResult(1)
    }
}
""";

    private const string AsyncGenericMethodCode = """
import System.Threading.Tasks.*

class C {
    async Identity<T>(value: T) -> Task<T> {
        await Task.Delay(1)
        return value
    }
}
""";

    [Fact]
    public void AsyncAssembly_PassesIlVerifyWhenToolAvailable()
    {
        if (!IlVerifyTestHelper.TryResolve(_output))
        {
            _output.WriteLine("Skipping IL verification because ilverify was not found.");
            return;
        }

        var assemblyPath = EmitAsyncAssemblyToDisk(AsyncTaskOfIntCode, out var compilation);

        try
        {
            var succeeded = IlVerifyRunner.Verify(null, assemblyPath, compilation);
            Assert.True(succeeded, "IL verification failed. Run ravenc --ilverify for detailed output.");
        }
        finally
        {
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void TryAwaitExpressionAsyncAssembly_PassesIlVerifyWhenToolAvailable()
    {
        if (!IlVerifyTestHelper.TryResolve(_output))
        {
            _output.WriteLine("Skipping IL verification because ilverify was not found.");
            return;
        }

        var (_, instructions) = CaptureAsyncInstructions(TryAwaitAsyncCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var retCount = instructions.Count(instruction => instruction.Opcode == OpCodes.Ret);
        Assert.Equal(1, retCount);

        Assert.Contains(
            instructions,
            instruction => instruction.Opcode == OpCodes.Leave || instruction.Opcode == OpCodes.Leave_S);

        var assemblyPath = EmitAsyncAssemblyToDisk(TryAwaitAsyncCode, out var compilation);

        try
        {
            var succeeded = IlVerifyRunner.Verify(null, assemblyPath, compilation);
            Assert.True(succeeded, "IL verification failed for try-await expression. Run ravenc --ilverify for detailed output.");
        }
        finally
        {
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
        }
    }

    [Fact]
    public void GenericAsyncMethod_RegistersConstructedStateMachine()
    {
        var syntaxTree = SyntaxTree.ParseText(AsyncGenericMethodCode);
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

        var model = compilation.GetSemanticModel(syntaxTree);

        var root = syntaxTree.GetRoot();
        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.Text == "Identity");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));

        var tryRewrite = typeof(CodeGenerator)
            .GetMethod("TryRewriteAsyncMethod", BindingFlags.Instance | BindingFlags.NonPublic)
            ?? throw new InvalidOperationException("Missing TryRewriteAsyncMethod reflection handle.");

        tryRewrite.Invoke(codeGenerator, new object?[]
        {
            model,
            methodSymbol,
            methodSyntax.Body,
            methodSyntax.ExpressionBody
        });

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);

        Assert.True(codeGenerator.TryGetConstructedAsyncStateMachine(stateMachine, out var constructed));

        Assert.True(methodSymbol.ConstructedAsyncStateMachine.HasValue);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol.ConstructedAsyncStateMachine.Value.Type, constructed.Type));

        var definitionGenerator = codeGenerator.GetOrCreateTypeGenerator(stateMachine);
        var constructedGenerator = codeGenerator.GetOrCreateTypeGenerator(constructed.Type);
        Assert.Same(definitionGenerator, constructedGenerator);
    }

    [Fact]
    public void GenericAsyncMethod_MethodGeneratorCapturesAsyncContext()
    {
        var syntaxTree = SyntaxTree.ParseText(AsyncGenericMethodCode);
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
        codeGenerator.Emit(peStream, Stream.Null);

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.Text == "Identity");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);

        var containingGenerator = codeGenerator.GetOrCreateTypeGenerator(methodSymbol.ContainingType);
        var methodGenerator = containingGenerator.GetMethodGenerator(methodSymbol);
        Assert.NotNull(methodGenerator);
        Assert.True(methodGenerator!.AsyncStateMachineContext.HasValue);

        var context = methodGenerator.AsyncStateMachineContext!.Value;
        Assert.Same(methodSymbol, context.AsyncMethod);
        Assert.Same(stateMachine, context.Definition);

        var constructedOpt = methodSymbol.ConstructedAsyncStateMachine;
        Assert.True(constructedOpt.HasValue, "Constructed async state machine missing from method symbol.");
        var constructed = constructedOpt.Value;

        Assert.Equal(constructed.Type, context.Constructed.Type);

        var builderFieldInfo = Assert.IsAssignableFrom<FieldInfo>(codeGenerator.GetMemberBuilder(stateMachine.BuilderField, context));
        var constructedBuilder = context.Constructed.BuilderField.GetFieldInfo(codeGenerator);
        Assert.Equal(constructedBuilder.DeclaringType?.FullName, builderFieldInfo.DeclaringType?.FullName);
        Assert.Equal(constructedBuilder.FieldType, builderFieldInfo.FieldType);

        var stateGenerator = codeGenerator.GetOrCreateTypeGenerator(stateMachine);
        var moveNextGenerator = stateGenerator.GetMethodGenerator(stateMachine.MoveNextMethod);
        Assert.NotNull(moveNextGenerator);
        Assert.True(moveNextGenerator!.AsyncStateMachineContext.HasValue);
        Assert.Equal(context.Constructed.Type, moveNextGenerator.AsyncStateMachineContext!.Value.Constructed.Type);
    }

    [Fact]
    public void GenericAsyncStateMachine_RegistersClonedTypeParameterMetadata()
    {
        using var metadata = EmitAsyncMetadata(AsyncGenericMethodCode);

        var reader = metadata.MetadataReader;
        var stateMachineDefinition = reader.GetTypeDefinition(metadata.StateMachineHandle);

        var genericParameters = stateMachineDefinition
            .GetGenericParameters()
            .Select(reader.GetGenericParameter)
            .ToArray();

        Assert.Single(genericParameters);
        var typeParameter = genericParameters[0];
        Assert.Equal("T", reader.GetString(typeParameter.Name));

        var builderField = stateMachineDefinition
            .GetFields()
            .Select(reader.GetFieldDefinition)
            .Single(field => reader.StringComparer.Equals(field.Name, "_builder"));

        var builderSignature = reader.GetBlobReader(builderField.Signature);
        var builderHeader = builderSignature.ReadSignatureHeader();
        Assert.Equal(SignatureKind.Field, builderHeader.Kind);

        var builderTypeCode = builderSignature.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.GenericTypeInstance, builderTypeCode);

        var builderTypeKind = builderSignature.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.TypeHandle, builderTypeKind);

        var builderHandle = builderSignature.ReadTypeHandle();
        var builderTypeName = GetTypeQualifiedName(reader, builderHandle);
        Assert.Equal("System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1", builderTypeName);

        var builderArity = builderSignature.ReadCompressedInteger();
        Assert.Equal(1, builderArity);

        var builderArgumentCode = builderSignature.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.GenericTypeParameter, builderArgumentCode);

        var builderArgumentOrdinal = builderSignature.ReadCompressedInteger();
        Assert.Equal(0, builderArgumentOrdinal);

        var valueField = stateMachineDefinition
            .GetFields()
            .Select(reader.GetFieldDefinition)
            .Single(field => reader.StringComparer.Equals(field.Name, "_value"));

        var valueSignature = reader.GetBlobReader(valueField.Signature);
        var valueHeader = valueSignature.ReadSignatureHeader();
        Assert.Equal(SignatureKind.Field, valueHeader.Kind);

        var valueTypeCode = valueSignature.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.GenericTypeParameter, valueTypeCode);

        var valueOrdinal = valueSignature.ReadCompressedInteger();
        Assert.Equal(0, valueOrdinal);
    }

    [Fact]
    public void GenericAsyncStateMachine_Metadata_NameIncludesMethodArity()
    {
        using var metadata = EmitAsyncMetadata(AsyncGenericMethodCode);

        var reader = metadata.MetadataReader;
        var definition = reader.GetTypeDefinition(metadata.StateMachineHandle);

        var typeName = reader.GetString(definition.Name);
        Assert.EndsWith("`1", typeName, StringComparison.Ordinal);

        Assert.Equal(metadata.MethodSymbol.TypeParameters.Length, definition.GetGenericParameters().Count());
    }

    [Fact]
    public void GenericAsyncStateMachine_Metadata_UsesGenericMethodParameterTokens()
    {
        using var metadata = EmitAsyncMetadata(AsyncGenericMethodCode);

        var reader = metadata.MetadataReader;

        var startHandle = FindMemberReference(
            reader,
            methodName: "Start",
            parentTypeName: "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1",
            expectedGenericParameterCount: 1,
            expectedParameterCount: 1);

        var startMember = reader.GetMemberReference(startHandle);
        var startSignature = reader.GetBlobReader(startMember.Signature);
        var startHeader = startSignature.ReadSignatureHeader();
        Assert.True(startHeader.Kind == SignatureKind.Method && startHeader.IsGeneric);

        var startGenericCount = startSignature.ReadCompressedInteger();
        Assert.Equal(1, startGenericCount);

        var startParameterCount = startSignature.ReadCompressedInteger();
        Assert.Equal(1, startParameterCount);

        Assert.Equal(SignatureTypeCode.Void, startSignature.ReadSignatureTypeCode());

        Assert.Equal(SignatureTypeCode.ByReference, startSignature.ReadSignatureTypeCode());
        Assert.Equal(SignatureTypeCode.GenericMethodParameter, startSignature.ReadSignatureTypeCode());
        Assert.Equal(0, startSignature.ReadCompressedInteger());

        var awaitHandle = FindMemberReference(
            reader,
            methodName: "AwaitUnsafeOnCompleted",
            parentTypeName: "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1",
            expectedGenericParameterCount: 2,
            expectedParameterCount: 2);

        var awaitMember = reader.GetMemberReference(awaitHandle);
        var awaitSignature = reader.GetBlobReader(awaitMember.Signature);
        var awaitHeader = awaitSignature.ReadSignatureHeader();
        Assert.True(awaitHeader.Kind == SignatureKind.Method && awaitHeader.IsGeneric);

        var awaitGenericCount = awaitSignature.ReadCompressedInteger();
        Assert.Equal(2, awaitGenericCount);

        var awaitParameterCount = awaitSignature.ReadCompressedInteger();
        Assert.Equal(2, awaitParameterCount);

        Assert.Equal(SignatureTypeCode.Void, awaitSignature.ReadSignatureTypeCode());

        for (var ordinal = 0; ordinal < 2; ordinal++)
        {
            Assert.Equal(SignatureTypeCode.ByReference, awaitSignature.ReadSignatureTypeCode());
            Assert.Equal(SignatureTypeCode.GenericMethodParameter, awaitSignature.ReadSignatureTypeCode());
            Assert.Equal(ordinal, awaitSignature.ReadCompressedInteger());
        }
    }

    [Fact]
    public void GenericAsyncMethod_StartCall_UsesConstructedStateMachine()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncGenericMethodCode, static generator =>
            generator.MethodSymbol.Name == "Identity" &&
            generator.MethodSymbol.ContainingType is SourceNamedTypeSymbol);

        var startCall = instructions.First(instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("Start", StringComparison.Ordinal));

        var startInfo = Assert.IsAssignableFrom<MethodInfo>(startCall.Operand.Value);
        Assert.True(startInfo.DeclaringType!.IsGenericType);
        var builderArgument = Assert.Single(startInfo.DeclaringType!.GetGenericArguments());
        AssertGenericTypeParameter(builderArgument, "T");

        var parameter = Assert.Single(startInfo.GetParameters());
        var parameterType = parameter.ParameterType;
        if (parameterType.IsByRef)
            parameterType = parameterType.GetElementType()!;

        Assert.True(parameterType.IsGenericParameter);
    }

    [Fact]
    public void GenericAsyncMethod_StartCall_ReusesStateMachineLocalAddress()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncGenericMethodCode, static generator =>
            generator.MethodSymbol.Name == "Identity" &&
            generator.MethodSymbol.ContainingType is SourceNamedTypeSymbol);

        var startIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("Start", StringComparison.Ordinal));

        Assert.True(startIndex >= 2, "Start invocation sequence too short.");

        var builderReceiver = instructions[startIndex - 2];
        Assert.Equal(OpCodes.Ldflda, builderReceiver.Opcode);
        Assert.Equal("_builder", FormatOperand(builderReceiver.Operand));

        var builderReceiverLoad = instructions[startIndex - 3];
        Assert.True(builderReceiverLoad.Opcode == OpCodes.Ldloca_S || builderReceiverLoad.Opcode == OpCodes.Ldloca);

        var stateMachineArgument = instructions[startIndex - 1];
        Assert.True(stateMachineArgument.Opcode == OpCodes.Ldloca_S || stateMachineArgument.Opcode == OpCodes.Ldloca);

        var builderOperand = Assert.IsAssignableFrom<FieldInfo>(builderReceiver.Operand.Value);
        Assert.NotNull(builderOperand.DeclaringType);
        Assert.True(builderOperand.DeclaringType!.IsGenericType);
    }

    [Fact]
    public void GenericAsyncStateMachine_MoveNext_UsesConstructedBuilderMetadata()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncGenericMethodCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var builderLoad = instructions.First(instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            string.Equals(FormatOperand(instruction.Operand), "_builder", StringComparison.Ordinal));

        var builderType = GetFieldOperandType(builderLoad.Operand);
        Assert.True(builderType.IsGenericType || builderType.IsGenericTypeDefinition);
        var builderArguments = builderType.GetGenericArguments();
        Assert.Single(builderArguments);
        var builderArgument = builderArguments[0];
        AssertGenericTypeParameter(builderArgument, "T");

        var awaitCall = instructions.First(instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("AwaitUnsafeOnCompleted", StringComparison.Ordinal));

        var awaitInfo = Assert.IsAssignableFrom<MethodInfo>(awaitCall.Operand.Value);
        Assert.True(awaitInfo.DeclaringType!.IsGenericType);
        var builderGeneric = Assert.Single(awaitInfo.DeclaringType!.GetGenericArguments());
        AssertGenericTypeParameter(builderGeneric, "T");

        var awaitGenericArguments = awaitInfo.GetGenericArguments();
        Assert.Equal(2, awaitGenericArguments.Length);
        var stateMachineGeneric = awaitGenericArguments[1];
        Assert.True(stateMachineGeneric.IsGenericParameter);
    }

    [Fact]
    public void GenericAsyncStateMachine_SetStateMachine_PassesReceiverByReference()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncGenericMethodCode, static generator =>
            generator.MethodSymbol.Name == "SetStateMachine" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var builderCall = instructions.First(instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name == nameof(AsyncTaskMethodBuilder.SetStateMachine));

        var methodInfo = Assert.IsAssignableFrom<MethodInfo>(builderCall.Operand.Value);
        Assert.True(methodInfo.DeclaringType!.IsGenericType);
        var builderArgument = Assert.Single(methodInfo.DeclaringType!.GetGenericArguments());
        AssertGenericTypeParameter(builderArgument, "T");

        var parameter = Assert.Single(methodInfo.GetParameters());
        Assert.Equal(typeof(IAsyncStateMachine), parameter.ParameterType);
    }

    [Fact]
    public void AsyncStateMachine_BuilderFieldMetadataUsesGenericBuilder()
    {
        using var metadata = EmitAsyncMetadata(AsyncTaskOfIntCode);

        var reader = metadata.MetadataReader;
        var stateMachineDefinition = reader.GetTypeDefinition(metadata.StateMachineHandle);

        var builderField = stateMachineDefinition
            .GetFields()
            .Select(reader.GetFieldDefinition)
            .Single(field => reader.StringComparer.Equals(field.Name, "_builder"));

        var signatureReader = reader.GetBlobReader(builderField.Signature);
        var header = signatureReader.ReadSignatureHeader();
        Assert.Equal(SignatureKind.Field, header.Kind);

        var fieldTypeCode = signatureReader.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.GenericTypeInstance, fieldTypeCode);

        var genericTypeKind = signatureReader.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.TypeHandle, genericTypeKind);

        var genericTypeHandle = signatureReader.ReadTypeHandle();
        var genericTypeName = GetTypeQualifiedName(reader, genericTypeHandle);
        Assert.Equal("System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1", genericTypeName);

        var arity = signatureReader.ReadCompressedInteger();
        Assert.Equal(1, arity);

        var argumentTypeCode = signatureReader.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.TypeHandle, argumentTypeCode);

        var argumentHandle = signatureReader.ReadTypeHandle();
        var argumentTypeName = GetTypeQualifiedName(reader, argumentHandle);
        Assert.Equal("System.Int32", argumentTypeName);
    }

    [Fact]
    public void AsyncStateMachine_MethodMetadataMatchesExpectedSignatures()
    {
        using var metadata = EmitAsyncMetadata(AsyncTaskOfIntCode);

        var reader = metadata.MetadataReader;
        var stateMachineDefinition = reader.GetTypeDefinition(metadata.StateMachineHandle);

        var moveNext = stateMachineDefinition
            .GetMethods()
            .Select(reader.GetMethodDefinition)
            .Single(method => reader.StringComparer.Equals(method.Name, "MoveNext"));

        var moveNextSignature = reader.GetBlobReader(moveNext.Signature);
        var moveNextHeader = moveNextSignature.ReadSignatureHeader();
        Assert.Equal(SignatureKind.Method, moveNextHeader.Kind);
        Assert.Equal(SignatureCallingConvention.Default, moveNextHeader.CallingConvention);
        Assert.False(moveNextHeader.IsGeneric);

        var moveNextParameterCount = moveNextSignature.ReadCompressedInteger();
        Assert.Equal(0, moveNextParameterCount);
        Assert.Equal(SignatureTypeCode.Void, moveNextSignature.ReadSignatureTypeCode());

        var setStateMachine = stateMachineDefinition
            .GetMethods()
            .Select(reader.GetMethodDefinition)
            .Single(method => reader.StringComparer.Equals(method.Name, "SetStateMachine"));

        var setSignature = reader.GetBlobReader(setStateMachine.Signature);
        var setHeader = setSignature.ReadSignatureHeader();
        Assert.Equal(SignatureKind.Method, setHeader.Kind);
        Assert.Equal(SignatureCallingConvention.Default, setHeader.CallingConvention);

        var parameterCount = setSignature.ReadCompressedInteger();
        Assert.Equal(1, parameterCount);
        Assert.Equal(SignatureTypeCode.Void, setSignature.ReadSignatureTypeCode());

        var parameterTypeCode = setSignature.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.TypeHandle, parameterTypeCode);

        var parameterHandle = setSignature.ReadTypeHandle();
        var parameterTypeName = GetTypeQualifiedName(reader, parameterHandle);
        Assert.Equal("System.Runtime.CompilerServices.IAsyncStateMachine", parameterTypeName);
    }

    [Fact]
    public void AsyncMethod_TaskOfInt_ReturnTypeMetadataIsClosedGeneric()
    {
        using var metadata = EmitAsyncMetadata(AsyncTaskOfIntCode);

        var reader = metadata.MetadataReader;
        var containingType = metadata.MethodSymbol.ContainingType
            ?? throw new InvalidOperationException("Async method is missing a containing type.");
        var typeHandle = FindTypeDefinition(reader, containingType);
        var methodHandle = FindMethodDefinition(reader, typeHandle, metadata.MethodSymbol.Name);
        var methodDefinition = reader.GetMethodDefinition(methodHandle);

        var returnTypeSymbol = metadata.MethodSymbol.ReturnType;
        var display = returnTypeSymbol.ToDisplayStringKeywordAware(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal("System.Threading.Tasks.Task<int>", display);

        var signatureReader = reader.GetBlobReader(methodDefinition.Signature);
        var header = signatureReader.ReadSignatureHeader();
        Assert.Equal(SignatureKind.Method, header.Kind);

        var parameterCount = signatureReader.ReadCompressedInteger();
        Assert.Equal(1, parameterCount);

        var returnTypeCode = signatureReader.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.GenericTypeInstance, returnTypeCode);

        var genericTypeKind = signatureReader.ReadSignatureTypeCode();
        Assert.Equal(SignatureTypeCode.TypeHandle, genericTypeKind);

        var taskHandle = signatureReader.ReadTypeHandle();
        var taskTypeName = GetTypeQualifiedName(reader, taskHandle);
        Assert.Equal("System.Threading.Tasks.Task`1", taskTypeName);

        var arity = signatureReader.ReadCompressedInteger();
        Assert.Equal(1, arity);

        var argumentTypeCode = signatureReader.ReadSignatureTypeCode();
        if (argumentTypeCode == SignatureTypeCode.TypeHandle)
        {
            var argumentHandle = signatureReader.ReadTypeHandle();
            var argumentName = GetTypeQualifiedName(reader, argumentHandle);
            Assert.Equal("System.Int32", argumentName);
        }
        else
        {
            Assert.Equal(SignatureTypeCode.Int32, argumentTypeCode);
        }
    }

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
    public void GenericAsyncStateMachine_MoveNext_DuplicatesReceiverAcrossStateAndBuilder()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncGenericMethodCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var stateStoreIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Stfld && FormatOperand(instruction.Operand) == "_state");

        Assert.True(stateStoreIndex >= 3, "State machine store sequence too short.");

        Assert.Equal(OpCodes.Ldarg_0, instructions[stateStoreIndex - 3].Opcode);
        Assert.Equal(OpCodes.Dup, instructions[stateStoreIndex - 2].Opcode);

        var builderAccess = instructions[stateStoreIndex + 1];
        Assert.Equal(OpCodes.Ldflda, builderAccess.Opcode);
        Assert.Equal("_builder", FormatOperand(builderAccess.Operand));

        var awaitCallIndex = Array.FindIndex(instructions, stateStoreIndex + 1, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("AwaitUnsafeOnCompleted", StringComparison.Ordinal));

        Assert.True(awaitCallIndex > stateStoreIndex, "AwaitUnsafeOnCompleted call not found after state store.");
    }

    [Fact]
    public void GenericAsyncStateMachine_SetStateMachine_UsesConstructedBuilderMetadata()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncGenericMethodCode, static generator =>
            generator.MethodSymbol.Name == "SetStateMachine" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var builderLoadIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderLoadIndex > 0, "Builder address load not found.");

        var receiverLoad = instructions[builderLoadIndex - 1];
        Assert.Equal(OpCodes.Ldarg_0, receiverLoad.Opcode);

        var setStateMachineCall = instructions.First(instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetStateMachine", StringComparison.Ordinal));

        var methodInfo = Assert.IsAssignableFrom<MethodInfo>(setStateMachineCall.Operand.Value);
        Assert.True(methodInfo.DeclaringType!.IsGenericType);
        var builderArgument = Assert.Single(methodInfo.DeclaringType!.GetGenericArguments());
        AssertGenericTypeParameter(builderArgument, "T");

        var parameter = Assert.Single(methodInfo.GetParameters());
        Assert.Equal(typeof(IAsyncStateMachine), parameter.ParameterType);
    }

    [Fact]
    public void AsyncEntryPoint_WithTaskOfInt_ExecutesSuccessfully()
    {
        var syntaxTree = SyntaxTree.ParseText(AsyncTaskOfIntEntryPointCode);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("async_entry", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = ReflectionEmitILBuilderFactory.Instance
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        peStream.Position = 0;
        var assembly = Assembly.Load(peStream.ToArray());

        var programType = assembly.GetType("Program", throwOnError: true, ignoreCase: false);
        Assert.NotNull(programType);

        var main = programType!.GetMethod("Main", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(main);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);

            var returnValue = main!.Invoke(null, new object?[] { Array.Empty<string>() });
            var awaitedResult = Assert.IsType<int>(returnValue);
            Assert.Equal(42, awaitedResult);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString().Replace("\r\n", "\n").Trim();
        Assert.Equal("42", output);
    }

    [Fact]
    public void AsyncEntryPoint_WithTask_ExecutesSuccessfully()
    {
        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var projectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");

        var sourcePath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.rav");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.dll");
        var runtimeConfigPath = Path.ChangeExtension(assemblyPath, ".runtimeconfig.json");

        File.WriteAllText(sourcePath, AsyncTaskEntryPointCode);

        try
        {
            var compilerArgs = $"run --project \"{projectPath}\" -- {sourcePath} -o {assemblyPath}";
            var compilerInfo = new ProcessStartInfo("dotnet", compilerArgs)
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = repoRoot
            };

            using (var compilerProcess = Process.Start(compilerInfo) ?? throw new InvalidOperationException("Failed to start ravenc."))
            {
                var compilerStdOut = compilerProcess.StandardOutput.ReadToEnd();
                var compilerStdErr = compilerProcess.StandardError.ReadToEnd();
                compilerProcess.WaitForExit();

                if (compilerProcess.ExitCode != 0)
                {
                    _output.WriteLine("ravenc stdout:");
                    _output.WriteLine(compilerStdOut);
                    _output.WriteLine("ravenc stderr:");
                    _output.WriteLine(compilerStdErr);
                }

                Assert.Equal(0, compilerProcess.ExitCode);
            }

            var runInfo = new ProcessStartInfo("dotnet", assemblyPath)
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = Path.GetDirectoryName(assemblyPath)
            };

            using var runProcess = Process.Start(runInfo) ?? throw new InvalidOperationException("Failed to start dotnet.");
            var runStdOut = runProcess.StandardOutput.ReadToEnd();
            var runStdErr = runProcess.StandardError.ReadToEnd();
            runProcess.WaitForExit();

            if (runProcess.ExitCode != 0 || !string.IsNullOrWhiteSpace(runStdErr))
            {
                _output.WriteLine("dotnet stdout:");
                _output.WriteLine(runStdOut);
                _output.WriteLine("dotnet stderr:");
                _output.WriteLine(runStdErr);
            }

            Assert.Equal(0, runProcess.ExitCode);
            Assert.True(string.IsNullOrWhiteSpace(runStdErr), "dotnet emitted unexpected stderr output.");

            var normalizedOutput = runStdOut.Replace("\r\n", "\n").Trim();
            Assert.Equal("first:1\ndone", normalizedOutput);
        }
        finally
        {
            if (File.Exists(sourcePath))
                File.Delete(sourcePath);
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
            if (File.Exists(runtimeConfigPath))
                File.Delete(runtimeConfigPath);
        }
    }

    [Fact]
    public void AsyncEntryPoint_WithTaskOfInt_ExecutesViaCliSuccessfully()
    {
        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var projectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");

        var sourcePath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.rav");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.dll");
        var runtimeConfigPath = Path.ChangeExtension(assemblyPath, ".runtimeconfig.json");

        File.WriteAllText(sourcePath, AsyncTaskOfIntEntryPointCode);

        try
        {
            var compilerArgs = $"run --project \"{projectPath}\" -- {sourcePath} -o {assemblyPath}";
            var compilerInfo = new ProcessStartInfo("dotnet", compilerArgs)
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = repoRoot
            };

            using (var compilerProcess = Process.Start(compilerInfo) ?? throw new InvalidOperationException("Failed to start ravenc."))
            {
                var compilerStdOut = compilerProcess.StandardOutput.ReadToEnd();
                var compilerStdErr = compilerProcess.StandardError.ReadToEnd();
                compilerProcess.WaitForExit();

                if (compilerProcess.ExitCode != 0)
                {
                    _output.WriteLine("ravenc stdout:");
                    _output.WriteLine(compilerStdOut);
                    _output.WriteLine("ravenc stderr:");
                    _output.WriteLine(compilerStdErr);
                }

                Assert.Equal(0, compilerProcess.ExitCode);
            }

            var runInfo = new ProcessStartInfo("dotnet", assemblyPath)
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = Path.GetDirectoryName(assemblyPath)
            };

            using var runProcess = Process.Start(runInfo) ?? throw new InvalidOperationException("Failed to start dotnet.");
            var runStdOut = runProcess.StandardOutput.ReadToEnd();
            var runStdErr = runProcess.StandardError.ReadToEnd();
            runProcess.WaitForExit();

            if (runProcess.ExitCode != 42 || !string.IsNullOrWhiteSpace(runStdErr))
            {
                _output.WriteLine("dotnet stdout:");
                _output.WriteLine(runStdOut);
                _output.WriteLine("dotnet stderr:");
                _output.WriteLine(runStdErr);
            }

            Assert.Equal(42, runProcess.ExitCode);
            Assert.True(string.IsNullOrWhiteSpace(runStdErr), "dotnet emitted unexpected stderr output.");

            var normalizedOutput = runStdOut.Replace("\r\n", "\n").Trim();
            Assert.Equal("42", normalizedOutput);
        }
        finally
        {
            if (File.Exists(sourcePath))
                File.Delete(sourcePath);
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
            if (File.Exists(runtimeConfigPath))
                File.Delete(runtimeConfigPath);
        }
    }

    [Fact]
    public void AsyncEntryPoint_MainBridge_AwaitsMainAsync()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntEntryPointCode, static generator =>
            generator.MethodSymbol.Name == "Main" &&
            generator.MethodSymbol.ContainingType is SourceNamedTypeSymbol type && type.Name == "Program");

        Assert.Contains(instructions, instruction =>
            instruction.Operand.Value is MethodInfo method && method.Name == "MainAsync");

        Assert.Contains(instructions, instruction =>
            instruction.Operand.Value is MethodInfo method && method.Name == "GetAwaiter");

        Assert.Contains(instructions, instruction =>
            instruction.Operand.Value is MethodInfo method && method.Name == "GetResult");
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
    public void MoveNext_SetResult_BorrowsReceiverBeforeBuilderLoad()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setResultIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetResult", StringComparison.Ordinal));

        Assert.True(setResultIndex >= 0, "Builder.SetResult call not found in MoveNext body.");

        var builderAddressIndex = Array.FindLastIndex(instructions, setResultIndex, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderAddressIndex > 0, "Builder field address not loaded before SetResult call.");

        var receiverLoadIndex = FindPrecedingLoadArgumentZero(instructions, builderAddressIndex);
        Assert.True(receiverLoadIndex >= 0, "ldarg.0 not found before builder field address load.");

        var betweenReceiverAndBuilder = instructions[(receiverLoadIndex + 1)..builderAddressIndex];
        Assert.Contains(betweenReceiverAndBuilder, instruction => instruction.Opcode == OpCodes.Dup);
    }

    [Fact]
    public void MoveNext_InvokesClosedGenericSetResultOverload()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setResultInstruction = instructions.First(instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetResult", StringComparison.Ordinal));

        var methodInfo = Assert.IsAssignableFrom<MethodInfo>(setResultInstruction.Operand.Value);

        Assert.True(methodInfo.DeclaringType!.IsGenericType);
        var genericArgument = Assert.Single(methodInfo.DeclaringType!.GetGenericArguments());
        Assert.Equal(typeof(int), genericArgument);

        var parameters = methodInfo.GetParameters();
        var parameter = Assert.Single(parameters);
        Assert.Equal(typeof(int), parameter.ParameterType);
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
    public void MoveNext_AwaitUnsafeOnCompleted_UsesBuilderAddress()
    {
        var (_, instructions) = CaptureAsyncInstructions(static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var awaitUnsafeIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("AwaitUnsafeOnCompleted", StringComparison.Ordinal));

        Assert.True(awaitUnsafeIndex >= 0, "AwaitUnsafeOnCompleted call not found in MoveNext body.");

        var builderAddressIndex = Array.FindLastIndex(instructions, awaitUnsafeIndex, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderAddressIndex >= 0, "Builder field address not loaded before AwaitUnsafeOnCompleted call.");

        for (var i = builderAddressIndex + 1; i < awaitUnsafeIndex; i++)
        {
            Assert.NotEqual(OpCodes.Stloc, instructions[i].Opcode);
        }

        var stateMachineAddressIndex = Array.FindLastIndex(instructions, awaitUnsafeIndex, instruction =>
            instruction.Opcode == OpCodes.Ldarga || instruction.Opcode == OpCodes.Ldarga_S);

        Assert.True(stateMachineAddressIndex >= 0, "State machine address not loaded before AwaitUnsafeOnCompleted call.");

        for (var i = stateMachineAddressIndex + 1; i < awaitUnsafeIndex; i++)
        {
            Assert.NotEqual(OpCodes.Stloc, instructions[i].Opcode);
        }
    }

    [Fact]
    public void MoveNext_AwaitUnsafeOnCompleted_ReusesStateMachineForAwaiter()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var builderAddressIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderAddressIndex >= 0, "Builder field address not found.");

        var awaiterAddressIndex = Array.FindIndex(instructions, builderAddressIndex + 1, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand).StartsWith("<>awaiter", StringComparison.Ordinal));

        Assert.True(awaiterAddressIndex > builderAddressIndex, "Awaiter address load not found after builder access.");

        var awaitCallIndex = Array.FindIndex(instructions, builderAddressIndex + 1, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("AwaitUnsafeOnCompleted", StringComparison.Ordinal));

        Assert.True(awaitCallIndex > awaiterAddressIndex, "AwaitUnsafeOnCompleted call not found after awaiter address load.");

        var betweenBuilderAndAwait = instructions[(builderAddressIndex + 1)..awaitCallIndex];
        Assert.Contains(betweenBuilderAndAwait, instruction => instruction.Opcode == OpCodes.Ldarg_0);

        var awaiterLoadInstruction = instructions[awaiterAddressIndex];
        Assert.Equal(OpCodes.Ldflda, awaiterLoadInstruction.Opcode);
    }


    [Fact]
    public void MoveNext_AwaitUnsafeOnCompleted_BorrowsReceiverForBuilder()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var builderAddressIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderAddressIndex > 0, "Builder address load not found before await scheduling.");

        var receiverLoadIndex = FindPrecedingLoadArgumentZero(instructions, builderAddressIndex);
        Assert.True(receiverLoadIndex >= 0, "ldarg.0 not found before builder field address load.");

        var betweenReceiverAndBuilder = instructions[(receiverLoadIndex + 1)..builderAddressIndex];
        Assert.Contains(betweenReceiverAndBuilder, instruction => instruction.Opcode == OpCodes.Dup);
    }

    [Fact]
    public void MoveNext_StampsTerminalStateBeforeCompletion()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setResultIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetResult", StringComparison.Ordinal));

        Assert.True(setResultIndex >= 0, "Builder.SetResult call not found in MoveNext body.");

        var stateStoreIndex = Array.FindLastIndex(instructions, setResultIndex, instruction =>
            instruction.Opcode == OpCodes.Stfld &&
            FormatOperand(instruction.Operand) == "_state");

        Assert.True(stateStoreIndex >= 0, "Terminal state store not found before SetResult.");

        var stateConstantIndex = stateStoreIndex - 1;
        Assert.InRange(stateConstantIndex, 0, instructions.Length - 1);

        var constantInstruction = instructions[stateConstantIndex];
        Assert.True(
            constantInstruction.Opcode == OpCodes.Ldc_I4 || constantInstruction.Opcode == OpCodes.Ldc_I4_S,
            $"Unexpected opcode loading terminal state: {constantInstruction.Opcode}.");
        Assert.Equal(-2, Assert.IsType<int>(constantInstruction.Operand.Value));

        var receiverLoadIndex = FindPrecedingLoadArgumentZero(instructions, stateStoreIndex);
        Assert.True(receiverLoadIndex >= 0, "ldarg.0 not found before terminal state store.");
        Assert.True(receiverLoadIndex < stateConstantIndex, "Receiver load must precede terminal state constant.");
    }


    [Fact]
    public void MoveNext_SetException_BorrowsReceiverBeforeBuilderLoad()
    {
        var (_, instructions) = CaptureAsyncInstructions(AsyncTaskOfIntCode, static generator =>
            generator.MethodSymbol.Name == "MoveNext" &&
            generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol);

        var setExceptionIndex = Array.FindIndex(instructions, instruction =>
            instruction.Opcode == OpCodes.Call &&
            instruction.Operand.Value is MethodInfo method &&
            method.Name.Contains("SetException", StringComparison.Ordinal));

        Assert.True(setExceptionIndex >= 0, "Builder.SetException call not found in MoveNext body.");

        var builderAddressIndex = Array.FindLastIndex(instructions, setExceptionIndex, instruction =>
            instruction.Opcode == OpCodes.Ldflda &&
            FormatOperand(instruction.Operand) == "_builder");

        Assert.True(builderAddressIndex > 0, "Builder address not loaded before SetException call.");

        var receiverLoadIndex = FindPrecedingLoadArgumentZero(instructions, builderAddressIndex);
        Assert.True(receiverLoadIndex >= 0, "ldarg.0 not found before builder field address load.");

        var betweenReceiverAndBuilder = instructions[(receiverLoadIndex + 1)..builderAddressIndex];
        Assert.Contains(betweenReceiverAndBuilder, instruction => instruction.Opcode == OpCodes.Dup);
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

        var stateMachine = compilation.GetSynthesizedAsyncStateMachineTypes()
            .Single(machine => machine.AsyncMethod.Name == "Compute");

        Assert.NotNull(stateMachine.MoveNextBody);

        var awaiterType = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.TaskAwaiter")
            ?? throw new InvalidOperationException("TaskAwaiter type not found in compilation references.");

        var injectedField = stateMachine.AddHoistedLocal("<>awaiter_injected", awaiterType);

        var stateFieldInfo = Assert.IsAssignableFrom<FieldInfo>(codeGenerator.GetMemberBuilder(stateMachine.StateField));
        var builderFieldInfo = Assert.IsAssignableFrom<FieldInfo>(codeGenerator.GetMemberBuilder(stateMachine.BuilderField));
        var injectedFieldInfo = Assert.IsAssignableFrom<FieldInfo>(codeGenerator.GetMemberBuilder(injectedField));

        Assert.Equal("_state", stateFieldInfo.Name);
        Assert.Equal("_builder", builderFieldInfo.Name);
        Assert.Equal("<>awaiter_injected", injectedFieldInfo.Name);
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

    private static void AssertGenericTypeParameter(Type type, string expectedName)
    {
        Assert.True(type.IsGenericParameter, $"Expected generic parameter '{expectedName}', but found '{type}'.");
        Assert.Equal(expectedName, type.Name);
    }

    private static Type GetFieldOperandType(RecordedOperand operand)
        => operand.Kind switch
        {
            RecordedOperandKind.FieldInfo => ((FieldInfo)operand.Value!).FieldType,
            RecordedOperandKind.FieldBuilder => ((FieldBuilder)operand.Value!).FieldType,
            _ => throw new InvalidOperationException($"Operand '{operand.Kind}' does not describe a field.")
        };

    private static string EmitAsyncAssemblyToDisk(string source, out Compilation compilation)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references =
        [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        compilation = Compilation.Create("async-ilverify", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = ReflectionEmitILBuilderFactory.Instance
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        var outputPath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.dll");
        peStream.Position = 0;

        using (var fileStream = File.Create(outputPath))
        {
            peStream.CopyTo(fileStream);
        }

        return outputPath;
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

        if (instruction.Opcode == OpCodes.Ldarga && instruction.Operand.Kind == RecordedOperandKind.Int32 && instruction.Operand.Value is int ldarga && ldarga == 0)
            return true;

        if (instruction.Opcode == OpCodes.Ldarga_S && instruction.Operand.Kind == RecordedOperandKind.Int32 && instruction.Operand.Value is int ldargaS && ldargaS == 0)
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

    private static MemberReferenceHandle FindMemberReference(
        MetadataReader reader,
        string methodName,
        string parentTypeName,
        int expectedGenericParameterCount,
        int expectedParameterCount)
    {
        foreach (var handle in reader.MemberReferences)
        {
            var member = reader.GetMemberReference(handle);

            if (!reader.StringComparer.Equals(member.Name, methodName))
                continue;

            var parentName = GetMemberParentQualifiedName(reader, member.Parent);
            if (!string.Equals(parentName, parentTypeName, StringComparison.Ordinal))
                continue;

            var signature = reader.GetBlobReader(member.Signature);
            var header = signature.ReadSignatureHeader();

            if (header.Kind != SignatureKind.Method || !header.IsGeneric)
                continue;

            var genericParameterCount = signature.ReadCompressedInteger();
            if (genericParameterCount != expectedGenericParameterCount)
                continue;

            var parameterCount = signature.ReadCompressedInteger();
            if (parameterCount != expectedParameterCount)
                continue;

            return handle;
        }

        throw new InvalidOperationException($"Member reference '{methodName}' on '{parentTypeName}' not found.");
    }

    private static string GetMemberParentQualifiedName(MetadataReader reader, EntityHandle handle)
    {
        return handle.Kind switch
        {
            HandleKind.TypeDefinition or HandleKind.TypeReference => GetTypeQualifiedName(reader, handle),
            HandleKind.TypeSpecification => GetTypeSpecificationQualifiedName(reader, (TypeSpecificationHandle)handle),
            _ => string.Empty
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

    private static TypeDefinitionHandle FindTypeDefinition(MetadataReader reader, INamedTypeSymbol typeSymbol)
    {
        var metadataName = typeSymbol.MetadataName;

        foreach (var handle in reader.TypeDefinitions)
        {
            var definition = reader.GetTypeDefinition(handle);
            if (reader.StringComparer.Equals(definition.Name, metadataName))
                return handle;
        }

        var simpleName = typeSymbol.Name;

        foreach (var handle in reader.TypeDefinitions)
        {
            var definition = reader.GetTypeDefinition(handle);
            if (reader.StringComparer.Equals(definition.Name, simpleName))
                return handle;
        }

        throw new InvalidOperationException($"Type '{typeSymbol}' not found in metadata.");
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

    private static AsyncMetadataContext EmitAsyncMetadata(string source)
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

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var classDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var methodDeclaration = classDeclaration.Members.OfType<MethodDeclarationSyntax>().Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = ReflectionEmitILBuilderFactory.Instance
        };

        var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        peStream.Position = 0;
        var peReader = new PEReader(peStream, PEStreamOptions.LeaveOpen);
        var metadataReader = peReader.GetMetadataReader();

        var stateMachineHandle = FindTypeDefinition(metadataReader, methodSymbol.AsyncStateMachine!);

        return new AsyncMetadataContext(compilation, methodSymbol, peStream, peReader, metadataReader, stateMachineHandle);
    }

    private static string GetTypeQualifiedName(MetadataReader reader, EntityHandle handle)
    {
        return handle.Kind switch
        {
            HandleKind.TypeReference => GetQualifiedName(
                reader.GetTypeReference((TypeReferenceHandle)handle).Namespace,
                reader.GetTypeReference((TypeReferenceHandle)handle).Name,
                reader),
            HandleKind.TypeDefinition => GetQualifiedName(
                reader.GetTypeDefinition((TypeDefinitionHandle)handle).Namespace,
                reader.GetTypeDefinition((TypeDefinitionHandle)handle).Name,
                reader),
            _ => throw new NotSupportedException($"Unsupported type handle kind '{handle.Kind}'.")
        };
    }

    private static string GetQualifiedName(StringHandle namespaceHandle, StringHandle nameHandle, MetadataReader reader)
    {
        var ns = reader.GetString(namespaceHandle);
        var name = reader.GetString(nameHandle);
        return string.IsNullOrEmpty(ns) ? name : $"{ns}.{name}";
    }

    private static string GetTypeSpecificationQualifiedName(MetadataReader reader, TypeSpecificationHandle handle)
    {
        var specification = reader.GetTypeSpecification(handle);
        var blob = reader.GetBlobReader(specification.Signature);
        _ = blob.ReadSignatureHeader();

        var typeCode = blob.ReadSignatureTypeCode();
        return typeCode switch
        {
            SignatureTypeCode.GenericTypeInstance => GetTypeSpecificationQualifiedNameFromGenericInstance(reader, ref blob),
            SignatureTypeCode.TypeHandle => GetTypeQualifiedName(reader, blob.ReadTypeHandle()),
            _ => throw new NotSupportedException($"Unsupported type specification code '{typeCode}'.")
        };
    }

    private static string GetTypeSpecificationQualifiedNameFromGenericInstance(MetadataReader reader, ref BlobReader blob)
    {
        _ = blob.ReadSignatureTypeCode();
        var underlyingHandle = blob.ReadTypeHandle();
        return GetTypeQualifiedName(reader, underlyingHandle);
    }

    private sealed class AsyncMetadataContext : IDisposable
    {
        public AsyncMetadataContext(
            Compilation compilation,
            SourceMethodSymbol methodSymbol,
            MemoryStream peStream,
            PEReader peReader,
            MetadataReader metadataReader,
            TypeDefinitionHandle stateMachineHandle)
        {
            Compilation = compilation;
            MethodSymbol = methodSymbol;
            PeStream = peStream;
            PeReader = peReader;
            MetadataReader = metadataReader;
            StateMachineHandle = stateMachineHandle;
        }

        public Compilation Compilation { get; }

        public SourceMethodSymbol MethodSymbol { get; }

        public MemoryStream PeStream { get; }

        public PEReader PeReader { get; }

        public MetadataReader MetadataReader { get; }

        public TypeDefinitionHandle StateMachineHandle { get; }

        public void Dispose()
        {
            PeReader.Dispose();
            PeStream.Dispose();
        }
    }
}
