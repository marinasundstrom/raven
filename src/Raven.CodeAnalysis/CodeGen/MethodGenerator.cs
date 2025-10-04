using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodGenerator
{
    private readonly List<(IParameterSymbol Symbol, ParameterBuilder Builder)> _parameterBuilders = new();
    private bool _bodyEmitted;
    private Compilation _compilation;
    private TypeGenerator.LambdaClosure? _lambdaClosure;

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

        var asyncStateMachineAttributeSymbol = compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncStateMachineAttribute);
        if (asyncStateMachineAttributeSymbol is INamedTypeSymbol asyncStateMachineAttribute)
        {
            var attributeType = asyncStateMachineAttribute.GetClrType(codeGen);
            var constructor = attributeType.GetConstructor(new[] { typeof(Type) });
            if (constructor is not null)
            {
                var stateMachineType = stateMachine.GetClrType(codeGen);
                var attributeBuilder = new CustomAttributeBuilder(constructor, new object[] { stateMachineType });
                applyMethodAttribute(attributeBuilder);
            }
        }

        var builderAttributeSymbol = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.AsyncMethodBuilderAttribute");
        if (builderAttributeSymbol is INamedTypeSymbol builderAttribute)
        {
            var attributeType = builderAttribute.GetClrType(codeGen);
            var constructor = attributeType?.GetConstructor(new[] { typeof(Type) });
            if (attributeType is not null && constructor is not null)
            {
                var builderType = stateMachine.BuilderField.Type.GetClrType(codeGen);
                var attributeBuilder = new CustomAttributeBuilder(constructor, new object[] { builderType });
                applyMethodAttribute(attributeBuilder);
            }
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
