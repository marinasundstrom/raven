using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodGenerator
{
    private readonly List<(IParameterSymbol Symbol, ParameterBuilder Builder)> _parameterBuilders = new();
    private bool _bodyEmitted;
    private Compilation _compilation;
    private TypeGenerator.LambdaClosure? _lambdaClosure;
    private ImmutableArray<ITypeParameterSymbol> _liftedExtensionParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    private GenericTypeParameterBuilder[]? _liftedExtensionBuilders;

    public MethodGenerator(TypeGenerator typeGenerator, IMethodSymbol methodSymbol, IILBuilderFactory? ilBuilderFactory = null)
    {
        TypeGenerator = typeGenerator;
        MethodSymbol = methodSymbol;
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

            var liftedTypeParameters = TypeGenerator.GetExtensionTypeParameters();
            var methodTypeParameters = MethodSymbol.TypeParameters;
            if (!liftedTypeParameters.IsDefaultOrEmpty &&
                methodTypeParameters.Length >= liftedTypeParameters.Length &&
                methodTypeParameters.Take(liftedTypeParameters.Length)
                    .Select(tp => tp.Name)
                    .SequenceEqual(liftedTypeParameters.Select(tp => tp.Name), StringComparer.Ordinal))
            {
                liftedTypeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
            }
            if (!liftedTypeParameters.IsDefaultOrEmpty || !methodTypeParameters.IsDefaultOrEmpty)
            {
                var allParameters = liftedTypeParameters.AddRange(methodTypeParameters);
                var genericBuilders = methodBuilder.DefineGenericParameters(allParameters.Select(tp => tp.Name).ToArray());

                if (!liftedTypeParameters.IsDefaultOrEmpty)
                {
                    var liftedBuilders = genericBuilders.Take(liftedTypeParameters.Length).ToArray();
                    _liftedExtensionParameters = liftedTypeParameters;
                    _liftedExtensionBuilders = liftedBuilders;
                    TypeGenerator.CodeGen.RegisterGenericParameters(liftedTypeParameters, liftedBuilders);
                }

                if (!methodTypeParameters.IsDefaultOrEmpty)
                {
                    var methodBuilders = genericBuilders.Skip(liftedTypeParameters.Length).ToArray();
                    TypeGenerator.CodeGen.RegisterGenericParameters(methodTypeParameters, methodBuilders);
                }
            }

            try
            {
                var returnType = MethodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
                    ? Compilation.GetSpecialType(SpecialType.System_Void).GetClrType(TypeGenerator.CodeGen)
                    : ResolveClrType(MethodSymbol.ReturnType);

                methodBuilder.SetReturnType(returnType);

                parameterTypes = BuildParameterTypes();

                if (parameterTypes.Length > 0)
                    methodBuilder.SetParameters(parameterTypes);
            }
            finally
            {
                if (!_liftedExtensionParameters.IsDefaultOrEmpty)
                    TypeGenerator.CodeGen.UnregisterGenericParameters(_liftedExtensionParameters);
            }
        }

        ParameterBuilder? returnParamBuilder = MethodBase is MethodBuilder methodBuilderInstance
            ? methodBuilderInstance.DefineParameter(0, ParameterAttributes.Retval, null)
            : ((ConstructorBuilder)MethodBase).DefineParameter(0, ParameterAttributes.Retval, null);

        if (MethodSymbol.ReturnType.IsTypeUnion)
        {
            var type = MethodSymbol.ReturnType;
            CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
            returnParamBuilder.SetCustomAttribute(customAttributeBuilder);
        }

        var nullableReturnAttr = TypeGenerator.CodeGen.CreateNullableAttribute(MethodSymbol.ReturnType);
        if (nullableReturnAttr is not null)
            returnParamBuilder.SetCustomAttribute(nullableReturnAttr);

        var tupleReturnAttr = TypeGenerator.CodeGen.CreateTupleElementNamesAttribute(MethodSymbol.ReturnType);
        if (tupleReturnAttr is not null)
            returnParamBuilder.SetCustomAttribute(tupleReturnAttr);

        TypeGenerator.CodeGen.ApplyCustomAttributes(MethodSymbol.GetReturnTypeAttributes(), attribute => returnParamBuilder.SetCustomAttribute(attribute));

        int i = 1;

        foreach (var parameterSymbol in MethodSymbol.Parameters)
        {
            ParameterAttributes attrs = ParameterAttributes.None;
            if (parameterSymbol.RefKind == RefKind.Out)
                attrs |= ParameterAttributes.Out;
            else if (parameterSymbol.RefKind == RefKind.In)
                attrs |= ParameterAttributes.In;

            ParameterBuilder parameterBuilder;
            if (MethodBase is MethodBuilder mb)
                parameterBuilder = mb.DefineParameter(i, attrs, parameterSymbol.Name);
            else
                parameterBuilder = ((ConstructorBuilder)MethodBase).DefineParameter(i, attrs, parameterSymbol.Name);

            if (parameterSymbol.Type.IsTypeUnion)
            {
                var type = parameterSymbol.Type;
                CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
                parameterBuilder.SetCustomAttribute(customAttributeBuilder);
            }

            var nullableAttr = TypeGenerator.CodeGen.CreateNullableAttribute(parameterSymbol.Type);
            if (nullableAttr is not null)
                parameterBuilder.SetCustomAttribute(nullableAttr);

            var tupleNamesAttr = TypeGenerator.CodeGen.CreateTupleElementNamesAttribute(parameterSymbol.Type);
            if (tupleNamesAttr is not null)
                parameterBuilder.SetCustomAttribute(tupleNamesAttr);

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
        TypeGenerator.ApplyExtensionMarkerNameAttribute(MethodSymbol, applyMethodAttribute);

        ApplyAsyncStateMachineMetadata(applyMethodAttribute);

        if (TypeGenerator.CodeGen.Compilation.IsEntryPointCandidate(MethodSymbol))
        {
            IsEntryPointCandidate = true;
        }

        Type[] BuildParameterTypes()
        {
            var builder = new List<Type>();

            if (_lambdaClosure is not null && MethodSymbol.IsStatic)
                builder.Add(_lambdaClosure.TypeBuilder);

            foreach (var parameter in MethodSymbol.Parameters)
            {
                var clrType = ResolveClrType(parameter.Type);
                if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter)
                {
                    if (!clrType.IsByRef)
                        clrType = clrType.MakeByRefType();
                }
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
                var builderTypeSymbol = stateMachine.SubstituteStateMachineTypeParameters(stateMachine.BuilderField.Type);
                var builderTypeName = GetAssemblyQualifiedMetadataName(builderTypeSymbol);
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
        try
        {
            var runtimeType = typeSymbol.GetClrType(TypeGenerator.CodeGen);
            if (runtimeType.AssemblyQualifiedName is string qualified && (!runtimeType.IsGenericType || !runtimeType.IsGenericTypeDefinition))
                return qualified;
        }
        catch
        {
            // Fall back to metadata formatting below when runtime types are unavailable (e.g., synthesized state machine types).
        }

        if (typeSymbol is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var definition = named.ConstructedFrom;
            var metadataName = definition.ToFullyQualifiedMetadataName();

            var builder = new StringBuilder(metadataName);
            builder.Append('[');

            for (var i = 0; i < named.TypeArguments.Length; i++)
            {
                if (i > 0)
                    builder.Append(',');

                builder.Append('[');
                builder.Append(GetAssemblyQualifiedMetadataName(named.TypeArguments[i]));
                builder.Append(']');
            }

            builder.Append(']');

            var assemblyName = named.ContainingAssembly?.Name;
            return string.IsNullOrEmpty(assemblyName)
                ? builder.ToString()
                : $"{builder}, {assemblyName}";
        }

        var simpleName = typeSymbol.ToFullyQualifiedMetadataName();
        var simpleAssembly = typeSymbol.ContainingAssembly?.Name;

        return string.IsNullOrEmpty(simpleAssembly)
            ? simpleName
            : $"{simpleName}, {simpleAssembly}";
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
        var types = (type as ITypeUnionSymbol).Types
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

        if (!_liftedExtensionParameters.IsDefaultOrEmpty && _liftedExtensionBuilders is not null)
            TypeGenerator.CodeGen.RegisterGenericParameters(_liftedExtensionParameters, _liftedExtensionBuilders);

        try
        {
            var bodyGenerator = new MethodBodyGenerator(this);
            bodyGenerator.Emit();
        }
        finally
        {
            if (!_liftedExtensionParameters.IsDefaultOrEmpty)
                TypeGenerator.CodeGen.UnregisterGenericParameters(_liftedExtensionParameters);
        }
        _bodyEmitted = true;
    }

    internal void EmitLambdaBody(BoundLambdaExpression lambda, TypeGenerator.LambdaClosure? closure)
    {
        if (_bodyEmitted)
            return;

        if (!_liftedExtensionParameters.IsDefaultOrEmpty && _liftedExtensionBuilders is not null)
            TypeGenerator.CodeGen.RegisterGenericParameters(_liftedExtensionParameters, _liftedExtensionBuilders);

        var bodyGenerator = new MethodBodyGenerator(this);

        BoundBlockStatement? rewrittenBody = null;
        ITypeSymbol? closureSelfType = null;

        if (lambda.Symbol is SourceLambdaSymbol sourceLambda && sourceLambda.IsAsync)
        {
            var block = ConvertToBlockStatement(sourceLambda, lambda.Body);
            if (closure is not null)
            {
                closureSelfType = closure.Symbol;
            }

            var rewritten = AsyncLowerer.Rewrite(sourceLambda, block, selfType: closureSelfType);
            if (rewritten.StateMachine is not null)
            {
                if (!TypeGenerator.CodeGen.TryGetRuntimeTypeForSymbol(rewritten.StateMachine, out _))
                {
                    var stateMachineGenerator = TypeGenerator.CodeGen.GetOrCreateTypeGenerator(rewritten.StateMachine);
                    stateMachineGenerator.DefineTypeBuilder();
                    stateMachineGenerator.DefineMemberBuilders();
                    stateMachineGenerator.EmitMemberILBodies();
                    stateMachineGenerator.CreateType();
                }
            }

            rewrittenBody = rewritten.Body;
        }

        try
        {
            bodyGenerator.EmitLambda(lambda, closure, rewrittenBody);
            _bodyEmitted = true;
        }
        finally
        {
            if (!_liftedExtensionParameters.IsDefaultOrEmpty)
                TypeGenerator.CodeGen.UnregisterGenericParameters(_liftedExtensionParameters);
        }
    }

    private static BoundBlockStatement ConvertToBlockStatement(SourceLambdaSymbol lambda, BoundExpression body)
    {
        if (body is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        if (lambda.ReturnType.SpecialType == SpecialType.System_Unit)
            return new BoundBlockStatement(new[] { new BoundExpressionStatement(body) });

        return new BoundBlockStatement(new[] { new BoundReturnStatement(body) });
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
                case OperatorDeclarationSyntax operatorDecl when operatorDecl.Body is not null || operatorDecl.ExpressionBody is not null:
                    return true;
                case ConversionOperatorDeclarationSyntax conversionDecl when conversionDecl.Body is not null || conversionDecl.ExpressionBody is not null:
                    return true;
                case AccessorDeclarationSyntax accessorDecl when accessorDecl.Body is not null || accessorDecl.ExpressionBody is not null:
                    return true;
            }
        }

        return false;
    }
}
