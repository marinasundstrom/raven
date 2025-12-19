using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.InteropServices;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class OptionalParameterSemanticTests : CompilationTestBase
{
    [Fact]
    public void Invocation_OmitsOptionalArgument_UsesDefaultValue()
    {
        const string source = """
class Calculator {
    static Add(value: int = 42) -> int {
        return value
    }
}

let result = Calculator.Add()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Add");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Single(arguments);
        var literal = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        Assert.Equal(42, literal.Value);

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(42, parameter.ExplicitDefaultValue);
    }

    [Fact]
    public void Invocation_UsesDefaultParameterValueAttribute_FromMetadata()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        return Library.Optional()
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var metadataAssembly = Assembly.LoadFrom(((PortableExecutableReference)metadataReference).FilePath);
        var reflectedType = metadataAssembly.GetType("Raven.Metadata.DefaultParameterValueAttributeFixture.Library");
        Assert.NotNull(reflectedType);

        var reflectedParameter = reflectedType!.GetMethod("Optional")!.GetParameters().Single();
        Assert.Equal(Type.Missing, reflectedParameter.RawDefaultValue);

        var reflectedAttribute = reflectedParameter.GetCustomAttributesData()
            .Single(attribute => attribute.AttributeType == typeof(DefaultParameterValueAttribute));
        Assert.Equal(123, reflectedAttribute.ConstructorArguments[0].Value);

        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Optional");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Single(arguments);
        var literal = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        Assert.Equal(123, literal.Value);

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(123, parameter.ExplicitDefaultValue);
    }

    [Fact]
    public void Invocation_UsesDefaultParameterValueAttribute_NotConvertible_ReportsDiagnostic()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        return Library.OptionalString()
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixtureWithMismatchedDefault());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal(CompilerDiagnostics.OptionalParameterDefaultValueCannotConvert.Id, diagnostic.Id);

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OptionalString");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        var argument = Assert.Single(arguments);
        Assert.IsType<BoundErrorExpression>(argument);

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(123, parameter.ExplicitDefaultValue);
    }

    private static byte[] CreateDefaultParameterValueAttributeFixture()
    {
        var metadataBuilder = new MetadataBuilder();
        var ilBuilder = new BlobBuilder();
        var methodBodyStream = new MethodBodyStreamEncoder(ilBuilder);
        var moduleVersionId = Guid.NewGuid();

        var coreLibAssembly = typeof(object).Assembly.GetName();
        var coreLibReference = metadataBuilder.AddAssemblyReference(
            name: metadataBuilder.GetOrAddString(coreLibAssembly.Name!),
            version: coreLibAssembly.Version!,
            culture: coreLibAssembly.CultureName is null ? default : metadataBuilder.GetOrAddString(coreLibAssembly.CultureName),
            publicKeyOrToken: metadataBuilder.GetOrAddBlob(coreLibAssembly.GetPublicKeyToken() ?? Array.Empty<byte>()),
            flags: 0,
            hashValue: default);

        var interopAssembly = typeof(DefaultParameterValueAttribute).Assembly.GetName();
        var interopReference = metadataBuilder.AddAssemblyReference(
            name: metadataBuilder.GetOrAddString(interopAssembly.Name!),
            version: interopAssembly.Version!,
            culture: interopAssembly.CultureName is null ? default : metadataBuilder.GetOrAddString(interopAssembly.CultureName),
            publicKeyOrToken: metadataBuilder.GetOrAddBlob(interopAssembly.GetPublicKeyToken() ?? Array.Empty<byte>()),
            flags: 0,
            hashValue: default);

        var systemNamespace = metadataBuilder.GetOrAddString("System");
        var interopNamespace = metadataBuilder.GetOrAddString("System.Runtime.InteropServices");

        var objectType = metadataBuilder.AddTypeReference(coreLibReference, systemNamespace, metadataBuilder.GetOrAddString("Object"));
        var defaultParameterValueAttribute = metadataBuilder.AddTypeReference(interopReference, interopNamespace, metadataBuilder.GetOrAddString(nameof(DefaultParameterValueAttribute)));
        var optionalAttribute = metadataBuilder.AddTypeReference(interopReference, interopNamespace, metadataBuilder.GetOrAddString(nameof(OptionalAttribute)));

        var parameterlessAttributeSignature = new BlobBuilder();
        parameterlessAttributeSignature.WriteByte((byte)SignatureCallingConvention.Default | (byte)SignatureAttributes.Instance);
        parameterlessAttributeSignature.WriteCompressedInteger(0);
        parameterlessAttributeSignature.WriteByte((byte)SignatureTypeCode.Void);

        var optionalAttributeConstructor = metadataBuilder.AddMemberReference(
            parent: optionalAttribute,
            name: metadataBuilder.GetOrAddString(".ctor"),
            signature: metadataBuilder.GetOrAddBlob(parameterlessAttributeSignature));

        var defaultParameterValueSignature = new BlobBuilder();
        defaultParameterValueSignature.WriteByte((byte)SignatureCallingConvention.Default | (byte)SignatureAttributes.Instance);
        defaultParameterValueSignature.WriteCompressedInteger(1);
        defaultParameterValueSignature.WriteByte((byte)SignatureTypeCode.Void);
        defaultParameterValueSignature.WriteByte((byte)SignatureTypeCode.Object);

        var defaultParameterValueConstructor = metadataBuilder.AddMemberReference(
            parent: defaultParameterValueAttribute,
            name: metadataBuilder.GetOrAddString(".ctor"),
            signature: metadataBuilder.GetOrAddBlob(defaultParameterValueSignature));

        metadataBuilder.AddModule(
            generation: 0,
            moduleName: metadataBuilder.GetOrAddString("DefaultParameterValueAttributeFixture"),
            mvid: metadataBuilder.GetOrAddGuid(moduleVersionId),
            encId: default,
            encBaseId: default);

        metadataBuilder.AddAssembly(
            name: metadataBuilder.GetOrAddString("DefaultParameterValueAttributeFixture"),
            version: new Version(1, 0, 0, 0),
            culture: default,
            publicKey: default,
            flags: 0,
            hashAlgorithm: AssemblyHashAlgorithm.Sha256);

        metadataBuilder.AddTypeDefinition(
            attributes: TypeAttributes.NotPublic | TypeAttributes.Sealed | TypeAttributes.Abstract,
            @namespace: default,
            name: metadataBuilder.GetOrAddString("<Module>"),
            baseType: default,
            fieldList: MetadataTokens.FieldDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.Field) + 1),
            methodList: MetadataTokens.MethodDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.MethodDef) + 1));

        var methodList = MetadataTokens.MethodDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.MethodDef) + 1);
        _ = metadataBuilder.AddTypeDefinition(
            attributes: TypeAttributes.Public | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit,
            @namespace: metadataBuilder.GetOrAddString("Raven.Metadata.DefaultParameterValueAttributeFixture"),
            name: metadataBuilder.GetOrAddString("Library"),
            baseType: objectType,
            fieldList: MetadataTokens.FieldDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.Field) + 1),
            methodList: methodList);

        var returnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var valueParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("value"), 1);

        var optionalAttributeValue = new BlobBuilder();
        optionalAttributeValue.WriteUInt16(1);
        optionalAttributeValue.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(valueParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValue));

        var defaultParameterValue = new BlobBuilder();
        defaultParameterValue.WriteUInt16(1);
        defaultParameterValue.WriteByte((byte)SerializationTypeCode.Int32);
        defaultParameterValue.WriteInt32(123);
        defaultParameterValue.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(valueParameter, defaultParameterValueConstructor, metadataBuilder.GetOrAddBlob(defaultParameterValue));

        var methodSignature = new BlobBuilder();
        methodSignature.WriteByte((byte)SignatureCallingConvention.Default);
        methodSignature.WriteCompressedInteger(1);
        methodSignature.WriteByte((byte)SignatureTypeCode.Void);
        methodSignature.WriteByte((byte)SignatureTypeCode.Int32);

        var il = new BlobBuilder();
        var instructionEncoder = new InstructionEncoder(il);
        instructionEncoder.OpCode(ILOpCode.Ret);

        var methodBody = methodBodyStream.AddMethodBody(instructionEncoder);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("Optional"),
            signature: metadataBuilder.GetOrAddBlob(methodSignature),
            bodyOffset: methodBody,
            parameterList: returnParameter);

        var peHeaderBuilder = new PEHeaderBuilder(imageCharacteristics: Characteristics.Dll);
        var metadataRootBuilder = new MetadataRootBuilder(metadataBuilder);
        var peBuilder = new ManagedPEBuilder(peHeaderBuilder, metadataRootBuilder, ilBuilder, entryPoint: default(MethodDefinitionHandle));

        var peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);

        using var peStream = new MemoryStream();
        peBlob.WriteContentTo(peStream);
        return peStream.ToArray();
    }

    private static byte[] CreateDefaultParameterValueAttributeFixtureWithMismatchedDefault()
    {
        var metadataBuilder = new MetadataBuilder();
        var ilBuilder = new BlobBuilder();
        var methodBodyStream = new MethodBodyStreamEncoder(ilBuilder);
        var moduleVersionId = Guid.NewGuid();

        var coreLibAssembly = typeof(object).Assembly.GetName();
        var coreLibReference = metadataBuilder.AddAssemblyReference(
            name: metadataBuilder.GetOrAddString(coreLibAssembly.Name!),
            version: coreLibAssembly.Version!,
            culture: coreLibAssembly.CultureName is null ? default : metadataBuilder.GetOrAddString(coreLibAssembly.CultureName),
            publicKeyOrToken: metadataBuilder.GetOrAddBlob(coreLibAssembly.GetPublicKeyToken() ?? Array.Empty<byte>()),
            flags: 0,
            hashValue: default);

        var interopAssembly = typeof(DefaultParameterValueAttribute).Assembly.GetName();
        var interopReference = metadataBuilder.AddAssemblyReference(
            name: metadataBuilder.GetOrAddString(interopAssembly.Name!),
            version: interopAssembly.Version!,
            culture: interopAssembly.CultureName is null ? default : metadataBuilder.GetOrAddString(interopAssembly.CultureName),
            publicKeyOrToken: metadataBuilder.GetOrAddBlob(interopAssembly.GetPublicKeyToken() ?? Array.Empty<byte>()),
            flags: 0,
            hashValue: default);

        var systemNamespace = metadataBuilder.GetOrAddString("System");
        var interopNamespace = metadataBuilder.GetOrAddString("System.Runtime.InteropServices");

        var objectType = metadataBuilder.AddTypeReference(coreLibReference, systemNamespace, metadataBuilder.GetOrAddString("Object"));
        var stringType = metadataBuilder.AddTypeReference(coreLibReference, systemNamespace, metadataBuilder.GetOrAddString("String"));
        var defaultParameterValueAttribute = metadataBuilder.AddTypeReference(interopReference, interopNamespace, metadataBuilder.GetOrAddString(nameof(DefaultParameterValueAttribute)));
        var optionalAttribute = metadataBuilder.AddTypeReference(interopReference, interopNamespace, metadataBuilder.GetOrAddString(nameof(OptionalAttribute)));

        var parameterlessAttributeSignature = new BlobBuilder();
        parameterlessAttributeSignature.WriteByte((byte)SignatureCallingConvention.Default | (byte)SignatureAttributes.Instance);
        parameterlessAttributeSignature.WriteCompressedInteger(0);
        parameterlessAttributeSignature.WriteByte((byte)SignatureTypeCode.Void);

        var optionalAttributeConstructor = metadataBuilder.AddMemberReference(
            parent: optionalAttribute,
            name: metadataBuilder.GetOrAddString(".ctor"),
            signature: metadataBuilder.GetOrAddBlob(parameterlessAttributeSignature));

        var defaultParameterValueSignature = new BlobBuilder();
        defaultParameterValueSignature.WriteByte((byte)SignatureCallingConvention.Default | (byte)SignatureAttributes.Instance);
        defaultParameterValueSignature.WriteCompressedInteger(1);
        defaultParameterValueSignature.WriteByte((byte)SignatureTypeCode.Void);
        defaultParameterValueSignature.WriteByte((byte)SignatureTypeCode.Object);

        var defaultParameterValueConstructor = metadataBuilder.AddMemberReference(
            parent: defaultParameterValueAttribute,
            name: metadataBuilder.GetOrAddString(".ctor"),
            signature: metadataBuilder.GetOrAddBlob(defaultParameterValueSignature));

        metadataBuilder.AddModule(
            generation: 0,
            moduleName: metadataBuilder.GetOrAddString("DefaultParameterValueAttributeFixture"),
            mvid: metadataBuilder.GetOrAddGuid(moduleVersionId),
            encId: default,
            encBaseId: default);

        metadataBuilder.AddAssembly(
            name: metadataBuilder.GetOrAddString("DefaultParameterValueAttributeFixture"),
            version: new Version(1, 0, 0, 0),
            culture: default,
            publicKey: default,
            flags: 0,
            hashAlgorithm: AssemblyHashAlgorithm.Sha256);

        metadataBuilder.AddTypeDefinition(
            attributes: TypeAttributes.NotPublic | TypeAttributes.Sealed | TypeAttributes.Abstract,
            @namespace: default,
            name: metadataBuilder.GetOrAddString("<Module>"),
            baseType: default,
            fieldList: MetadataTokens.FieldDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.Field) + 1),
            methodList: MetadataTokens.MethodDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.MethodDef) + 1));

        var methodList = MetadataTokens.MethodDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.MethodDef) + 1);
        _ = metadataBuilder.AddTypeDefinition(
            attributes: TypeAttributes.Public | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit,
            @namespace: metadataBuilder.GetOrAddString("Raven.Metadata.DefaultParameterValueAttributeFixture"),
            name: metadataBuilder.GetOrAddString("Library"),
            baseType: objectType,
            fieldList: MetadataTokens.FieldDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.Field) + 1),
            methodList: methodList);

        var returnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var valueParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("value"), 1);

        var optionalAttributeValue = new BlobBuilder();
        optionalAttributeValue.WriteUInt16(1);
        optionalAttributeValue.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(valueParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValue));

        var defaultParameterValue = new BlobBuilder();
        defaultParameterValue.WriteUInt16(1);
        defaultParameterValue.WriteByte((byte)SerializationTypeCode.Int32);
        defaultParameterValue.WriteInt32(123);
        defaultParameterValue.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(valueParameter, defaultParameterValueConstructor, metadataBuilder.GetOrAddBlob(defaultParameterValue));

        var methodSignature = new BlobBuilder();
        methodSignature.WriteByte((byte)SignatureCallingConvention.Default);
        methodSignature.WriteCompressedInteger(1);
        methodSignature.WriteByte((byte)SignatureTypeCode.Void);
        methodSignature.WriteByte((byte)SignatureTypeCode.String);

        var il = new BlobBuilder();
        var instructionEncoder = new InstructionEncoder(il);
        instructionEncoder.OpCode(ILOpCode.Ldnull);
        instructionEncoder.OpCode(ILOpCode.Ret);

        var methodBody = methodBodyStream.AddMethodBody(instructionEncoder);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OptionalString"),
            signature: metadataBuilder.GetOrAddBlob(methodSignature),
            bodyOffset: methodBody,
            parameterList: returnParameter);

        var peHeaderBuilder = new PEHeaderBuilder(imageCharacteristics: Characteristics.Dll);
        var metadataRootBuilder = new MetadataRootBuilder(metadataBuilder);
        var peBuilder = new ManagedPEBuilder(peHeaderBuilder, metadataRootBuilder, ilBuilder, entryPoint: default(MethodDefinitionHandle));

        var peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);

        using var peStream = new MemoryStream();
        peBlob.WriteContentTo(peStream);
        return peStream.ToArray();
    }
}
