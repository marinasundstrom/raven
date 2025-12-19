using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Runtime.InteropServices;
using System.Runtime.Loader;

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

        var metadataImage = CreateDefaultParameterValueAttributeFixture();
        var metadataReference = MetadataReference.CreateFromImage(metadataImage);
        var loadContext = new AssemblyLoadContext("DefaultParameterValueAttributeFixture", isCollectible: true);
        using var metadataStream = new MemoryStream(metadataImage);
        var metadataAssembly = loadContext.LoadFromStream(metadataStream);
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

        loadContext.Unload();
    }

    [Fact]
    public void Invocation_UsesDefaultParameterValueAttribute_WithNamedArguments()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        Library.OptionalNamed(trailing: 456, first: 1)
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OptionalNamed");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Equal(3, arguments.Length);

        var first = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        Assert.Equal(1, first.Value);

        var optional = Assert.IsType<BoundLiteralExpression>(arguments[1]);
        Assert.Equal(123, optional.Value);

        var trailing = Assert.IsType<BoundLiteralExpression>(arguments[2]);
        Assert.Equal(456, trailing.Value);
    }

    [Fact]
    public void Invocation_UsesDefaultParameterValueAttribute_InOverloadResolution()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        Library.OverloadedOptional(1)
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OverloadedOptional");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Equal(2, arguments.Length);
        var suppliedArgument = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        Assert.Equal(1, suppliedArgument.Value);

        var synthesizedDefault = Assert.IsType<BoundLiteralExpression>(arguments[1]);
        Assert.Equal(321, synthesizedDefault.Value);

        Assert.Equal("OverloadedOptional", boundInvocation.Method.Name);
        Assert.Equal(2, boundInvocation.Method.Parameters.Length);
        var optionalParameter = boundInvocation.Method.Parameters[1];
        Assert.True(optionalParameter.HasExplicitDefaultValue);
        Assert.Equal(321, optionalParameter.ExplicitDefaultValue);
    }

    [Fact]
    public void Invocation_UsesDefaultParameterValueAttribute_WithEnum()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        Library.OptionalEnum()
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OptionalEnum");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Single(arguments);
        var defaultValue = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        Assert.Equal(typeof(DayOfWeek).Name, defaultValue.Type!.Name);
        Assert.Equal((int)DayOfWeek.Wednesday, Assert.IsType<int>(defaultValue.Value));

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal((int)DayOfWeek.Wednesday, parameter.ExplicitDefaultValue);
    }

    [Fact]
    public void Invocation_UsesOptionalAttribute_DefaultsToTypeDefault()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        Library.OptionalAttributeOnly()
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OptionalAttributeOnly");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        var synthesizedDefault = Assert.IsType<BoundLiteralExpression>(Assert.Single(arguments));
        Assert.Equal(0, synthesizedDefault.Value);

        var optionalParameter = boundInvocation.Method.Parameters.Single();
        Assert.True(optionalParameter.HasExplicitDefaultValue);
        Assert.Equal(0, optionalParameter.ExplicitDefaultValue);
    }

    [Fact]
    public void Invocation_UsesOptionalAttribute_DefaultStructUsesDefaultLiteral()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        Library.OptionalStruct()
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OptionalStruct");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var argument = Assert.Single(boundInvocation.Arguments);
        var synthesizedDefault = Assert.IsType<BoundDefaultValueExpression>(argument);

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(default(Guid), parameter.ExplicitDefaultValue);
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter.Type, synthesizedDefault.Type));
    }

    [Fact]
    public void Invocation_UsesOptionalAttribute_DefaultReferenceUsesNullLiteral()
    {
        const string source = """
import Raven.Metadata.DefaultParameterValueAttributeFixture.*

class C {
    InvokeMetadata() {
        Library.OptionalReference()
    }
}
""";

        var metadataReference = MetadataReference.CreateFromImage(CreateDefaultParameterValueAttributeFixture());
        var (compilation, tree) = CreateCompilation(source, references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "OptionalReference");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var argument = Assert.Single(boundInvocation.Arguments);
        var literal = Assert.IsType<BoundLiteralExpression>(argument);

        Assert.Equal(BoundLiteralExpressionKind.NullLiteral, literal.Kind);
        Assert.Null(literal.Value);

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Null(parameter.ExplicitDefaultValue);
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter.Type, literal.ConvertedType));
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
        var valueType = metadataBuilder.AddTypeReference(coreLibReference, systemNamespace, metadataBuilder.GetOrAddString("ValueType"));
        var dayOfWeekType = metadataBuilder.AddTypeReference(coreLibReference, systemNamespace, metadataBuilder.GetOrAddString("DayOfWeek"));
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

        var structMethodList = MetadataTokens.MethodDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.MethodDef) + 1);
        var structFieldList = MetadataTokens.FieldDefinitionHandle(metadataBuilder.GetRowCount(TableIndex.Field) + 1);
        var optionalStructType = metadataBuilder.AddTypeDefinition(
            attributes: TypeAttributes.Public | TypeAttributes.SequentialLayout | TypeAttributes.Sealed | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit,
            @namespace: metadataBuilder.GetOrAddString("Raven.Metadata.DefaultParameterValueAttributeFixture"),
            name: metadataBuilder.GetOrAddString("OptionalStruct"),
            baseType: valueType,
            fieldList: structFieldList,
            methodList: structMethodList);

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

        var secondReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var firstParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString("first"), 1);
        var optionalParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("optional"), 2);
        var trailingParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("trailing"), 3);

        var overloadedReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var overloadedValueParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString("value"), 1);
        var overloadedOptionalParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("optional"), 2);

        var overloadedDoubleReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var overloadedDoubleValueParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString("value"), 1);

        var optionalOnlyReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var optionalOnlyParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("value"), 1);

        var optionalStructReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var optionalStructParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("value"), 1);

        var optionalReferenceReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var optionalReferenceParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("value"), 1);

        var optionalEnumReturnParameter = metadataBuilder.AddParameter(ParameterAttributes.None, metadataBuilder.GetOrAddString(string.Empty), 0);
        var optionalEnumParameter = metadataBuilder.AddParameter(ParameterAttributes.Optional, metadataBuilder.GetOrAddString("value"), 1);

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

        var optionalAttributeValueSecond = new BlobBuilder();
        optionalAttributeValueSecond.WriteUInt16(1);
        optionalAttributeValueSecond.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(optionalParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValueSecond));

        var optionalAttributeValueThird = new BlobBuilder();
        optionalAttributeValueThird.WriteUInt16(1);
        optionalAttributeValueThird.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(trailingParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValueThird));

        var optionalAttributeValueFourth = new BlobBuilder();
        optionalAttributeValueFourth.WriteUInt16(1);
        optionalAttributeValueFourth.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(optionalOnlyParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValueFourth));

        var optionalAttributeValueFifth = new BlobBuilder();
        optionalAttributeValueFifth.WriteUInt16(1);
        optionalAttributeValueFifth.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(optionalStructParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValueFifth));

        var optionalAttributeValueSixth = new BlobBuilder();
        optionalAttributeValueSixth.WriteUInt16(1);
        optionalAttributeValueSixth.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(optionalReferenceParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValueSixth));

        var optionalAttributeValueNinth = new BlobBuilder();
        optionalAttributeValueNinth.WriteUInt16(1);
        optionalAttributeValueNinth.WriteUInt16(0);
        metadataBuilder.AddCustomAttribute(optionalEnumParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(optionalAttributeValueNinth));

        var methodSignature = new BlobBuilder();
        methodSignature.WriteByte((byte)SignatureCallingConvention.Default);
        methodSignature.WriteCompressedInteger(1);
        methodSignature.WriteByte((byte)SignatureTypeCode.Void);
        methodSignature.WriteByte((byte)SignatureTypeCode.Int32);

        var optionalMethodSignature = new BlobBuilder();
        optionalMethodSignature.WriteByte((byte)SignatureCallingConvention.Default);
        optionalMethodSignature.WriteCompressedInteger(3);
        optionalMethodSignature.WriteByte((byte)SignatureTypeCode.Void);
        optionalMethodSignature.WriteByte((byte)SignatureTypeCode.Int32);
        optionalMethodSignature.WriteByte((byte)SignatureTypeCode.Int32);
        optionalMethodSignature.WriteByte((byte)SignatureTypeCode.Int32);

        var overloadedOptionalSignature = new BlobBuilder();
        overloadedOptionalSignature.WriteByte((byte)SignatureCallingConvention.Default);
        overloadedOptionalSignature.WriteCompressedInteger(2);
        overloadedOptionalSignature.WriteByte((byte)SignatureTypeCode.Void);
        overloadedOptionalSignature.WriteByte((byte)SignatureTypeCode.Int32);
        overloadedOptionalSignature.WriteByte((byte)SignatureTypeCode.Int32);

        var overloadedDoubleSignature = new BlobBuilder();
        overloadedDoubleSignature.WriteByte((byte)SignatureCallingConvention.Default);
        overloadedDoubleSignature.WriteCompressedInteger(1);
        overloadedDoubleSignature.WriteByte((byte)SignatureTypeCode.Void);
        overloadedDoubleSignature.WriteByte((byte)SignatureTypeCode.Double);

        var optionalOnlySignature = new BlobBuilder();
        optionalOnlySignature.WriteByte((byte)SignatureCallingConvention.Default);
        optionalOnlySignature.WriteCompressedInteger(1);
        optionalOnlySignature.WriteByte((byte)SignatureTypeCode.Void);
        optionalOnlySignature.WriteByte((byte)SignatureTypeCode.Int32);

        var optionalStructSignature = new BlobBuilder();
        new BlobEncoder(optionalStructSignature)
            .MethodSignature(SignatureCallingConvention.Default)
            .Parameters(
                parameterCount: 1,
                returnType: static returnType => returnType.Void(),
                parameters: parameters =>
                {
                    parameters.AddParameter().Type().Type(optionalStructType, isValueType: true);
                });

        var optionalReferenceSignature = new BlobBuilder();
        optionalReferenceSignature.WriteByte((byte)SignatureCallingConvention.Default);
        optionalReferenceSignature.WriteCompressedInteger(1);
        optionalReferenceSignature.WriteByte((byte)SignatureTypeCode.String);
        optionalReferenceSignature.WriteByte((byte)SignatureTypeCode.String);

        var optionalEnumSignature = new BlobBuilder();
        new BlobEncoder(optionalEnumSignature)
            .MethodSignature(SignatureCallingConvention.Default)
            .Parameters(
                parameterCount: 1,
                returnType: static returnType => returnType.Void(),
                parameters: parameters => parameters.AddParameter().Type().Type(dayOfWeekType, isValueType: true));

        var il = new BlobBuilder();
        var instructionEncoder = new InstructionEncoder(il);
        instructionEncoder.OpCode(ILOpCode.Ret);

        var methodBody = methodBodyStream.AddMethodBody(instructionEncoder);

        var optionalReferenceIl = new BlobBuilder();
        var optionalReferenceInstructionEncoder = new InstructionEncoder(optionalReferenceIl);
        optionalReferenceInstructionEncoder.OpCode(ILOpCode.Ldarg_0);
        optionalReferenceInstructionEncoder.OpCode(ILOpCode.Ret);

        var optionalReferenceMethodBody = methodBodyStream.AddMethodBody(optionalReferenceInstructionEncoder);

        var optionalDefaultValue = new BlobBuilder();
        optionalDefaultValue.WriteUInt16(1);
        optionalDefaultValue.WriteByte((byte)SerializationTypeCode.Int32);
        optionalDefaultValue.WriteInt32(123);
        optionalDefaultValue.WriteUInt16(0);

        var trailingDefaultValue = new BlobBuilder();
        trailingDefaultValue.WriteUInt16(1);
        trailingDefaultValue.WriteByte((byte)SerializationTypeCode.Int32);
        trailingDefaultValue.WriteInt32(456);
        trailingDefaultValue.WriteUInt16(0);

        var overloadedOptionalAttributeValue = new BlobBuilder();
        overloadedOptionalAttributeValue.WriteUInt16(1);
        overloadedOptionalAttributeValue.WriteUInt16(0);

        var overloadedDefaultValue = new BlobBuilder();
        overloadedDefaultValue.WriteUInt16(1);
        overloadedDefaultValue.WriteByte((byte)SerializationTypeCode.Int32);
        overloadedDefaultValue.WriteInt32(321);
        overloadedDefaultValue.WriteUInt16(0);

        var optionalEnumValue = new BlobBuilder();
        optionalEnumValue.WriteUInt16(1);
        optionalEnumValue.WriteByte((byte)SerializationTypeCode.Int32);
        optionalEnumValue.WriteInt32((int)DayOfWeek.Wednesday);
        optionalEnumValue.WriteUInt16(0);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("Optional"),
            signature: metadataBuilder.GetOrAddBlob(methodSignature),
            bodyOffset: methodBody,
            parameterList: returnParameter);

        metadataBuilder.AddCustomAttribute(optionalParameter, defaultParameterValueConstructor, metadataBuilder.GetOrAddBlob(optionalDefaultValue));
        metadataBuilder.AddCustomAttribute(trailingParameter, defaultParameterValueConstructor, metadataBuilder.GetOrAddBlob(trailingDefaultValue));

        metadataBuilder.AddCustomAttribute(overloadedOptionalParameter, optionalAttributeConstructor, metadataBuilder.GetOrAddBlob(overloadedOptionalAttributeValue));
        metadataBuilder.AddCustomAttribute(overloadedOptionalParameter, defaultParameterValueConstructor, metadataBuilder.GetOrAddBlob(overloadedDefaultValue));
        metadataBuilder.AddCustomAttribute(optionalEnumParameter, defaultParameterValueConstructor, metadataBuilder.GetOrAddBlob(optionalEnumValue));

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OptionalNamed"),
            signature: metadataBuilder.GetOrAddBlob(optionalMethodSignature),
            bodyOffset: methodBody,
            parameterList: secondReturnParameter);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OverloadedOptional"),
            signature: metadataBuilder.GetOrAddBlob(overloadedOptionalSignature),
            bodyOffset: methodBody,
            parameterList: overloadedReturnParameter);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OverloadedOptional"),
            signature: metadataBuilder.GetOrAddBlob(overloadedDoubleSignature),
            bodyOffset: methodBody,
            parameterList: overloadedDoubleReturnParameter);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OptionalAttributeOnly"),
            signature: metadataBuilder.GetOrAddBlob(optionalOnlySignature),
            bodyOffset: methodBody,
            parameterList: optionalOnlyReturnParameter);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OptionalStruct"),
            signature: metadataBuilder.GetOrAddBlob(optionalStructSignature),
            bodyOffset: methodBody,
            parameterList: optionalStructReturnParameter);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OptionalReference"),
            signature: metadataBuilder.GetOrAddBlob(optionalReferenceSignature),
            bodyOffset: optionalReferenceMethodBody,
            parameterList: optionalReferenceReturnParameter);

        metadataBuilder.AddMethodDefinition(
            attributes: MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            implAttributes: MethodImplAttributes.IL | MethodImplAttributes.Managed,
            name: metadataBuilder.GetOrAddString("OptionalEnum"),
            signature: metadataBuilder.GetOrAddBlob(optionalEnumSignature),
            bodyOffset: methodBody,
            parameterList: optionalEnumReturnParameter);

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
