
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodGenerator
{
    private readonly IDictionary<ISymbol, ParameterBuilder> _parameterBuilders = new Dictionary<ISymbol, ParameterBuilder>(SymbolEqualityComparer.Default);
    private bool _bodyEmitted;
    private Compilation _compilation;
    private TypeGenerator.LambdaClosure? _lambdaClosure;

    public MethodGenerator(TypeGenerator typeGenerator, IMethodSymbol methodSymbol)
    {
        TypeGenerator = typeGenerator;
        MethodSymbol = methodSymbol;
    }

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
        var returnType = MethodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
            ? Compilation.GetSpecialType(SpecialType.System_Void).GetClrType(TypeGenerator.CodeGen)
            : ResolveClrType(MethodSymbol.ReturnType);

        var parameterTypesBuilder = new List<Type>();

        if (_lambdaClosure is not null)
            parameterTypesBuilder.Add(_lambdaClosure.TypeBuilder);

        foreach (var parameter in MethodSymbol.Parameters)
        {
            var clrType = ResolveClrType(parameter.Type);
            if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                clrType = clrType.MakeByRefType();
            parameterTypesBuilder.Add(clrType);
        }

        var parameterTypes = parameterTypesBuilder.ToArray();

        var isExplicitInterfaceImplementation = MethodSymbol.MethodKind == MethodKind.ExplicitInterfaceImplementation
            || !MethodSymbol.ExplicitInterfaceImplementations.IsDefaultOrEmpty;

        MethodAttributes attributes = MethodAttributes.HideBySig | GetMethodAccessibilityAttributes(MethodSymbol);

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

        if (MethodSymbol.IsConstructor && !MethodSymbol.IsNamedConstructor)
        {
            if (MethodSymbol.IsStatic)
                MethodBase = TypeGenerator.TypeBuilder!.DefineTypeInitializer();
            else
                MethodBase = TypeGenerator.TypeBuilder!
                    .DefineConstructor(attributes, CallingConventions.Standard, parameterTypes);
        }
        else
        {
            MethodBase = TypeGenerator.TypeBuilder!
                .DefineMethod(MethodSymbol.Name,
                    attributes, CallingConventions.Standard,
                    returnType,
                    parameterTypes);
        }

        ParameterBuilder? returnParamBuilder = MethodBase is MethodBuilder methodBuilder
            ? methodBuilder.DefineParameter(0, ParameterAttributes.Retval, null)
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

        int i = 1;

        if (_lambdaClosure is not null)
        {
            if (MethodBase is MethodBuilder closureBuilder)
                closureBuilder.DefineParameter(i, ParameterAttributes.None, "<closure>");
            else
                ((ConstructorBuilder)MethodBase).DefineParameter(i, ParameterAttributes.None, "<closure>");

            i++;
        }

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

            _parameterBuilders[parameterSymbol] = parameterBuilder;
            i++;
        }

        if (MethodSymbol.Name == "Main")
        {
            IsEntryPointCandidate = true;
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

    public IEnumerable<ParameterBuilder> GetParameterBuilders() => _parameterBuilders.Values;

    public ParameterBuilder GetParameterBuilder(IParameterSymbol parameterSymbol) => _parameterBuilders[parameterSymbol];

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
