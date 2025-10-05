using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;

using Raven.CodeAnalysis.CodeGen.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodGenerator
{
    private readonly List<(IParameterSymbol Symbol, ParameterBuilder Builder)> _parameterBuilders = new();
    private bool _bodyEmitted;
    private Compilation _compilation;
    private TypeGenerator.LambdaClosure? _lambdaClosure;

    public MethodGenerator(TypeGenerator typeGenerator, IMethodSymbol methodSymbol, MetadataMethodDefinition metadataMethod, IILBuilderFactory? ilBuilderFactory = null)
    {
        TypeGenerator = typeGenerator;
        MethodSymbol = methodSymbol;
        MetadataMethod = metadataMethod ?? throw new ArgumentNullException(nameof(metadataMethod));
        ILBuilderFactory = ilBuilderFactory ?? typeGenerator.CodeGen.ILBuilderFactory;
    }

    public IILBuilderFactory ILBuilderFactory { get; }

    internal void SetLambdaClosure(TypeGenerator.LambdaClosure closure)
    {
        _lambdaClosure = closure;
    }

    public Compilation Compilation => _compilation ??= TypeGenerator.Compilation;
    public TypeGenerator TypeGenerator { get; }
    public IMethodSymbol MethodSymbol { get; }
    public MetadataMethodDefinition MetadataMethod { get; }
    public MethodBase MethodBase { get; private set; }
    internal TypeGenerator.LambdaClosure? LambdaClosure => _lambdaClosure;
    public bool IsEntryPointCandidate { get; private set; }
    internal bool HasEmittedBody => _bodyEmitted;

    internal void DefineMethodBuilder()
    {
        var targetTypeBuilder = _lambdaClosure?.TypeBuilder ?? TypeGenerator.TypeBuilder
            ?? throw new InvalidOperationException("Type builder must be defined before creating method builders.");

        var isExplicitInterfaceImplementation = MethodSymbol.MethodKind == MethodKind.ExplicitInterfaceImplementation
            || !MethodSymbol.ExplicitInterfaceImplementations.IsDefaultOrEmpty;

        MethodAttributes attributes = MethodAttributes.HideBySig | GetMethodAccessibilityAttributes(MethodSymbol);

        if (_lambdaClosure is not null)
            attributes = (attributes & ~MethodAttributes.MemberAccessMask) | MethodAttributes.Public;

        if (MethodSymbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet)
            attributes |= MethodAttributes.SpecialName;

        var isInterfaceMethod = TypeGenerator.TypeSymbol is INamedTypeSymbol named && named.TypeKind == TypeKind.Interface;
        var hasInterfaceBody = isInterfaceMethod && !MethodSymbol.IsStatic && HasInterfaceMethodBody(MethodSymbol);

        if (isExplicitInterfaceImplementation)
        {
            attributes |= MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.NewSlot;
        }
        else if (isInterfaceMethod && !MethodSymbol.IsStatic)
        {
            attributes |= MethodAttributes.Virtual | MethodAttributes.NewSlot;

            if (!hasInterfaceBody)
                attributes |= MethodAttributes.Abstract;
        }
        else
        {
            if (TypeGenerator.ImplementsInterfaceMethod(MethodSymbol))
            {
                attributes |= MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.NewSlot;
            }
            else
            {
                if (MethodSymbol.IsAbstract)
                {
                    attributes |= MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.NewSlot;
                }
                else if (MethodSymbol.IsVirtual)
                {
                    attributes |= MethodAttributes.Virtual;

                    if (!MethodSymbol.IsOverride)
                        attributes |= MethodAttributes.NewSlot;
                }

                if (MethodSymbol.IsOverride && MethodSymbol.IsSealed)
                    attributes |= MethodAttributes.Final;
            }
        }

        if (MethodSymbol.IsStatic)
            attributes |= MethodAttributes.Static;

        var metadataMethod = MetadataMethod;
        metadataMethod.SetAttributes(attributes);
        metadataMethod.SetImplementationAttributes(MethodImplAttributes.IL);
        metadataMethod.SetReturnType(MethodSymbol.ReturnType);
        metadataMethod.SetRequiresNullableAttributeOnReturn(MetadataNullability.RequiresNullableAttribute(MethodSymbol.ReturnType));
        metadataMethod.SetCustomAttributes(MethodSymbol.GetAttributes());
        metadataMethod.SetReturnAttributes(MethodSymbol.GetReturnTypeAttributes());
        metadataMethod.SetParameters(TypeGenerator.BuildParameterMetadata(MethodSymbol.Parameters));

        var parameterTypes = Array.Empty<Type>();

        if (MethodSymbol.IsConstructor && !MethodSymbol.IsNamedConstructor)
        {
            parameterTypes = BuildParameterTypes();

            if (MethodSymbol.IsStatic)
                MethodBase = targetTypeBuilder.DefineTypeInitializer();
            else
                MethodBase = targetTypeBuilder
                    .DefineConstructor(attributes, CallingConventions.Standard, parameterTypes);
        }
        else
        {
            var methodBuilder = targetTypeBuilder
                .DefineMethod(MethodSymbol.Name,
                    attributes, CallingConventions.Standard);

            MethodBase = methodBuilder;

            if (!MethodSymbol.TypeParameters.IsDefaultOrEmpty)
            {
                var genericBuilders = methodBuilder.DefineGenericParameters(MethodSymbol.TypeParameters.Select(tp => tp.Name).ToArray());
                TypeGenerator.CodeGen.RegisterGenericParameters(MethodSymbol.TypeParameters, genericBuilders);
            }

            var returnType = MethodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
                ? Compilation.GetSpecialType(SpecialType.System_Void).GetClrType(TypeGenerator.CodeGen)
                : ResolveClrType(MethodSymbol.ReturnType);

            methodBuilder.SetReturnType(returnType);

            parameterTypes = BuildParameterTypes();

            if (parameterTypes.Length > 0)
                methodBuilder.SetParameters(parameterTypes);
        }

        ParameterBuilder? returnParamBuilder = MethodBase is MethodBuilder methodBuilderInstance
            ? methodBuilderInstance.DefineParameter(0, ParameterAttributes.Retval, null)
            : ((ConstructorBuilder)MethodBase).DefineParameter(0, ParameterAttributes.Retval, null);

        if (MethodSymbol.ReturnType.IsUnion)
        {
            var type = MethodSymbol.ReturnType;
            CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
            returnParamBuilder.SetCustomAttribute(customAttributeBuilder);
        }

        var nullableReturnAttr = TypeGenerator.CodeGen.CreateNullableAttribute(MethodSymbol.ReturnType);
        if (nullableReturnAttr is not null)
            returnParamBuilder.SetCustomAttribute(nullableReturnAttr);

        TypeGenerator.CodeGen.ApplyCustomAttributes(MethodSymbol.GetReturnTypeAttributes(), attribute => returnParamBuilder.SetCustomAttribute(attribute));

        int i = 1;

        foreach (var parameterSymbol in MethodSymbol.Parameters)
        {
            var attrs = TypeGenerator.GetParameterAttributes(parameterSymbol);

            ParameterBuilder parameterBuilder;
            if (MethodBase is MethodBuilder mb)
                parameterBuilder = mb.DefineParameter(i, attrs, parameterSymbol.Name);
            else
                parameterBuilder = ((ConstructorBuilder)MethodBase).DefineParameter(i, attrs, parameterSymbol.Name);

            if (parameterSymbol.Type.IsUnion)
            {
                var type = parameterSymbol.Type;
                CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
                parameterBuilder.SetCustomAttribute(customAttributeBuilder);
            }

            var nullableAttr = TypeGenerator.CodeGen.CreateNullableAttribute(parameterSymbol.Type);
            if (nullableAttr is not null)
                parameterBuilder.SetCustomAttribute(nullableAttr);

            TypeGenerator.CodeGen.ApplyCustomAttributes(parameterSymbol.GetAttributes(), attribute => parameterBuilder.SetCustomAttribute(attribute));

            _parameterBuilders.Add((parameterSymbol, parameterBuilder));
            i++;
        }

        Action<CustomAttributeBuilder> applyMethodAttribute = MethodBase switch
        {
            MethodBuilder methodBuilder => methodBuilder.SetCustomAttribute,
            ConstructorBuilder constructorBuilder => constructorBuilder.SetCustomAttribute,
            _ => throw new InvalidOperationException("Unexpected method base type for attribute emission.")
        };

        TypeGenerator.CodeGen.ApplyCustomAttributes(MethodSymbol.GetAttributes(), applyMethodAttribute);

        ApplyAsyncStateMachineMetadata(applyMethodAttribute);

        if (TypeGenerator.CodeGen.Compilation.IsEntryPointCandidate(MethodSymbol))
        {
            IsEntryPointCandidate = true;
            metadataMethod.SetIsEntryPointCandidate(true);
        }

        Type[] BuildParameterTypes()
        {
            var builder = new List<Type>();

            if (_lambdaClosure is not null && MethodSymbol.IsStatic)
                builder.Add(_lambdaClosure.TypeBuilder);

            foreach (var parameter in MethodSymbol.Parameters)
            {
                var clrType = ResolveClrType(parameter.Type);
                if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                    clrType = clrType.MakeByRefType();
                builder.Add(clrType);
            }

            return builder.ToArray();
        }
    }

    private void ApplyAsyncStateMachineMetadata(Action<CustomAttributeBuilder> applyMethodAttribute)
    {
        if (MethodSymbol is not SourceMethodSymbol sourceMethod)
            return;

        var stateMachine = sourceMethod.AsyncStateMachine;
        if (stateMachine is null)
            return;

        var codeGen = TypeGenerator.CodeGen;
        var compilation = Compilation;

        var systemTypeSymbol = compilation.GetSpecialType(SpecialType.System_Type);
        if (systemTypeSymbol is IErrorTypeSymbol)
            return;

        var systemType = systemTypeSymbol.GetClrType(codeGen);

        var stateMachineAttributeSymbol = compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncStateMachineAttribute);
        if (stateMachineAttributeSymbol is not IErrorTypeSymbol)
        {
            var attributeType = stateMachineAttributeSymbol.GetClrType(codeGen);
            var stateMachineCtor = attributeType.GetConstructor(new[] { systemType });
            if (stateMachineCtor is not null)
            {
                var stateMachineTypeName = GetAssemblyQualifiedMetadataName(stateMachine);
                ApplyMethodCustomAttribute(stateMachineCtor, stateMachineTypeName);
            }
        }

        var builderAttributeSymbol = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.AsyncMethodBuilderAttribute");
        if (builderAttributeSymbol is not null && builderAttributeSymbol.TypeKind != TypeKind.Error)
        {
            var attributeType = builderAttributeSymbol.GetClrType(codeGen);
            var builderCtor = attributeType.GetConstructor(new[] { systemType });
            if (builderCtor is not null)
            {
                var builderTypeName = GetAssemblyQualifiedMetadataName(stateMachine.BuilderField.Type);
                ApplyMethodCustomAttribute(builderCtor, builderTypeName);
            }
        }
    }

    private void ApplyMethodCustomAttribute(ConstructorInfo constructor, string assemblyQualifiedTypeName)
    {
        var blob = CreateTypeArgumentAttributeBlob(assemblyQualifiedTypeName);

        switch (MethodBase)
        {
            case MethodBuilder methodBuilder:
                methodBuilder.SetCustomAttribute(constructor, blob);
                break;
            case ConstructorBuilder constructorBuilder:
                constructorBuilder.SetCustomAttribute(constructor, blob);
                break;
            default:
                throw new InvalidOperationException("Unexpected method base type for attribute emission.");
        }
    }

    private string GetAssemblyQualifiedMetadataName(ITypeSymbol typeSymbol)
    {
        var metadataName = typeSymbol.ToFullyQualifiedMetadataName();
        var assemblyName = typeSymbol.ContainingAssembly?.Name;

        return string.IsNullOrEmpty(assemblyName)
            ? metadataName
            : $"{metadataName}, {assemblyName}";
    }

    private static byte[] CreateTypeArgumentAttributeBlob(string assemblyQualifiedTypeName)
    {
        using var stream = new MemoryStream();

        stream.WriteByte(0x01); // Prolog
        stream.WriteByte(0x00);

        WriteSerString(stream, assemblyQualifiedTypeName);

        stream.WriteByte(0x00); // Named argument count (0)
        stream.WriteByte(0x00);

        return stream.ToArray();
    }

    private static void WriteSerString(Stream stream, string value)
    {
        if (value is null)
        {
            stream.WriteByte(0xFF);
            return;
        }

        var bytes = Encoding.UTF8.GetBytes(value);
        WriteCompressedUInt(stream, (uint)bytes.Length);
        stream.Write(bytes, 0, bytes.Length);
    }

    private static void WriteCompressedUInt(Stream stream, uint value)
    {
        if (value <= 0x7F)
        {
            stream.WriteByte((byte)value);
        }
        else if (value <= 0x3FFF)
        {
            stream.WriteByte((byte)((value >> 8) | 0x80));
            stream.WriteByte((byte)(value & 0xFF));
        }
        else if (value <= 0x1FFFFFFF)
        {
            stream.WriteByte((byte)((value >> 24) | 0xC0));
            stream.WriteByte((byte)((value >> 16) & 0xFF));
            stream.WriteByte((byte)((value >> 8) & 0xFF));
            stream.WriteByte((byte)(value & 0xFF));
        }
        else
        {
            throw new ArgumentOutOfRangeException(nameof(value), "Value is too large to be represented as a compressed unsigned integer.");
        }
    }

    private static MethodAttributes GetMethodAccessibilityAttributes(IMethodSymbol methodSymbol)
    {
        return methodSymbol.DeclaredAccessibility switch
        {
            Accessibility.Public => MethodAttributes.Public,
            Accessibility.Private => MethodAttributes.Private,
            Accessibility.Internal => MethodAttributes.Assembly,
            Accessibility.ProtectedAndProtected => MethodAttributes.Family,
            Accessibility.ProtectedOrInternal => MethodAttributes.FamORAssem,
            Accessibility.ProtectedAndInternal => MethodAttributes.FamANDAssem,
            _ => MethodAttributes.Private
        };
    }

    private CustomAttributeBuilder CreateUnionTypeAttribute(ITypeSymbol type)
    {
        var types = (type as IUnionTypeSymbol).Types
            .Select(x => x is LiteralTypeSymbol lit ? lit.ConstantValue : (object)ResolveClrType(x))
            .ToArray();
        var constructor = TypeGenerator.CodeGen.TypeUnionAttributeType!.
            GetConstructor(new[] { typeof(object[]) });
        CustomAttributeBuilder customAttributeBuilder = new CustomAttributeBuilder(constructor!, [types]);
        return customAttributeBuilder;
    }

    public IEnumerable<ParameterBuilder> GetParameterBuilders()
    {
        foreach (var (_, builder) in _parameterBuilders)
            yield return builder;
    }

    public ParameterBuilder GetParameterBuilder(IParameterSymbol parameterSymbol)
    {
        foreach (var (symbol, builder) in _parameterBuilders)
        {
            if (ReferenceEquals(symbol, parameterSymbol) || SymbolEqualityComparer.Default.Equals(symbol, parameterSymbol))
                return builder;
        }

        throw new KeyNotFoundException($"Missing parameter builder for '{parameterSymbol.Name}'.");
    }

    public void EmitBody()
    {
        if (_bodyEmitted)
            return;

        var isInterfaceMethod = TypeGenerator.TypeSymbol is INamedTypeSymbol named && named.TypeKind == TypeKind.Interface;

        if (isInterfaceMethod && !MethodSymbol.IsStatic && !HasInterfaceMethodBody(MethodSymbol))
        {
            _bodyEmitted = true;
            return;
        }

        var bodyGenerator = new MethodBodyGenerator(this);
        bodyGenerator.Emit();
        _bodyEmitted = true;
    }

    internal void EmitLambdaBody(BoundLambdaExpression lambda, TypeGenerator.LambdaClosure? closure)
    {
        if (_bodyEmitted)
            return;

        var bodyGenerator = new MethodBodyGenerator(this);
        bodyGenerator.EmitLambda(lambda, closure);
        _bodyEmitted = true;
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(TypeGenerator.CodeGen);
    }

    public override string ToString() => this.MethodSymbol.ToDisplayString();

    private static bool HasInterfaceMethodBody(IMethodSymbol methodSymbol)
    {
        foreach (var syntaxRef in methodSymbol.DeclaringSyntaxReferences)
        {
            var syntax = syntaxRef.GetSyntax();
            switch (syntax)
            {
                case MethodDeclarationSyntax methodDecl when methodDecl.Body is not null || methodDecl.ExpressionBody is not null:
                    return true;
                case AccessorDeclarationSyntax accessorDecl when accessorDecl.Body is not null || accessorDecl.ExpressionBody is not null:
                    return true;
            }
        }

        return false;
    }
}
