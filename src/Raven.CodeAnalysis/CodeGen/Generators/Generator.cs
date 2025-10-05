using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.CodeGen;

internal abstract class Generator
{
    static readonly Dictionary<SyntaxTree, SemanticModel> _semanticModels = new Dictionary<SyntaxTree, SemanticModel>();

    public Generator(Generator? parent = null)
    {
        Parent = parent;
    }

    public Generator? Parent { get; }

    public Compilation Compilation => MethodBodyGenerator.Compilation;

    public MethodGenerator MethodGenerator => MethodBodyGenerator.MethodGenerator;

    public virtual MethodBodyGenerator MethodBodyGenerator => Parent!.MethodBodyGenerator;

    public IMethodSymbol MethodSymbol => MethodBodyGenerator.MethodSymbol;

    public IILBuilder ILGenerator => MethodBodyGenerator.ILGenerator;

    public RuntimeTypeResolver RuntimeTypeResolver => MethodBodyGenerator.RuntimeTypeResolver;

    public virtual void Emit()
    {

    }

    public virtual void AddLocal(ILocalSymbol localSymbol, IILocal builder)
    {
        Parent?.AddLocal(localSymbol, builder);
    }

    public virtual IILocal? GetLocal(ILocalSymbol localSymbol)
    {
        return Parent?.GetLocal(localSymbol);
    }

    public virtual IEnumerable<ILocalSymbol> EnumerateLocalsToDispose()
    {
        return Parent?.EnumerateLocalsToDispose() ?? Enumerable.Empty<ILocalSymbol>();
    }

    public void EmitDispose(ImmutableArray<ILocalSymbol> locals)
    {
        if (locals.IsDefaultOrEmpty || locals.Length == 0)
            return;

        var disposableType = Compilation.GetSpecialType(SpecialType.System_IDisposable);
        if (disposableType.TypeKind == TypeKind.Error)
            return;

        var disposableClr = ResolveClrType(disposableType);
        var disposeMethod = disposableClr.GetMethod(nameof(IDisposable.Dispose), Type.EmptyTypes)
            ?? throw new InvalidOperationException("Missing IDisposable.Dispose method.");

        for (int i = locals.Length - 1; i >= 0; i--)
        {
            EmitDispose(locals[i], disposableClr, disposeMethod);
        }
    }

    private void EmitDispose(ILocalSymbol local, Type disposableClr, MethodInfo disposeMethod)
    {
        if (local.Type is null || local.Type.TypeKind == TypeKind.Error)
            return;

        var localBuilder = GetLocal(local);
        if (localBuilder is null)
            return;

        if (local.Type.IsReferenceType || local.Type.TypeKind == TypeKind.Null)
        {
            var skipLabel = ILGenerator.DefineLabel();
            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            ILGenerator.Emit(OpCodes.Brfalse, skipLabel);
            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            ILGenerator.Emit(OpCodes.Callvirt, disposeMethod);
            ILGenerator.MarkLabel(skipLabel);
        }
        else
        {
            var clrType = ResolveClrType(local.Type);
            ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
            ILGenerator.Emit(OpCodes.Constrained, clrType);
            ILGenerator.Emit(OpCodes.Callvirt, disposeMethod);
        }
    }

    protected BoundNode GetBoundNode(SyntaxNode syntaxNode)
    {
        SemanticModel semanticModel = ResolveSemanticModel(syntaxNode);
        return semanticModel.GetBoundNode(syntaxNode);
    }

    protected BoundExpression GetBoundNode(ExpressionSyntax expression)
    {
        SemanticModel semanticModel = ResolveSemanticModel(expression);
        return semanticModel.GetBoundNode(expression) ?? throw new InvalidCastException("Cannot cast {0} to {2}.");
    }

    protected SymbolInfo GetSymbolInfo(SyntaxNode syntaxNode)
    {
        SemanticModel semanticModel = ResolveSemanticModel(syntaxNode);
        return semanticModel.GetSymbolInfo(syntaxNode);
    }

    protected T? GetDeclaredSymbol<T>(SyntaxNode syntaxNode)
        where T : class, ISymbol
    {
        SemanticModel semanticModel = ResolveSemanticModel(syntaxNode);
        return semanticModel.GetDeclaredSymbol(syntaxNode) as T;
    }

    protected TypeInfo GetTypeInfo(ExpressionSyntax expression)
    {
        SemanticModel semanticModel = ResolveSemanticModel(expression);
        return semanticModel.GetTypeInfo(expression);
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(MethodGenerator.TypeGenerator.CodeGen);
    }

    protected bool ShouldBoxForStorage(ITypeSymbol? storageType, BoundExpression valueExpression, out ITypeSymbol? runtimeValueType)
    {
        runtimeValueType = null;

        if (storageType is null || valueExpression is null)
            return false;

        runtimeValueType = GetRuntimeValueType(valueExpression);
        return ShouldBoxForStorage(storageType, runtimeValueType);
    }

    protected bool ShouldBoxForStorage(ITypeSymbol? storageType, BoundExpression valueExpression)
        => ShouldBoxForStorage(storageType, valueExpression, out _);

    protected bool TryEmitBoxForStorage(ITypeSymbol? storageType, BoundExpression valueExpression)
    {
        if (storageType is null)
            return false;

        if (ShouldBoxForStorage(storageType, valueExpression, out var runtimeValueType) &&
            runtimeValueType is not null)
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(runtimeValueType));
            return true;
        }

        var fallbackType = valueExpression.Type;
        if (ShouldBoxForStorage(storageType, fallbackType) &&
            fallbackType is not null)
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(fallbackType));
            return true;
        }

        return false;
    }

    protected bool ShouldBoxForStorage(ITypeSymbol? storageType, ITypeSymbol? valueType)
    {
        if (storageType is null || valueType is null)
            return false;

        if (storageType.TypeKind == TypeKind.Error || valueType.TypeKind == TypeKind.Error)
            return false;

        var valueClrType = ResolveClrType(valueType);
        if (!valueClrType.IsValueType)
            return false;

        var storageClrType = ResolveClrType(storageType);
        return !storageClrType.IsValueType;
    }

    protected ITypeSymbol? GetRuntimeValueType(BoundExpression expression)
    {
        var current = expression;

        while (true)
        {
            switch (current)
            {
                case BoundParenthesizedExpression parenthesized:
                    current = parenthesized.Expression;
                    continue;

                case BoundCastExpression cast:
                    if (cast.Conversion.IsIdentity || cast.Conversion.IsAlias)
                    {
                        current = cast.Expression;
                        continue;
                    }

                    if (cast.Conversion.IsBoxing)
                        return cast.Type;

                    if (TryGetUnionOperandValueType(cast) is { } unionOperand)
                        return unionOperand;

                    return cast.Type;

                case BoundAsExpression asExpression:
                    if (asExpression.Conversion.IsIdentity || asExpression.Conversion.IsAlias)
                    {
                        current = asExpression.Expression;
                        continue;
                    }

                    return asExpression.Type;

                default:
                    return current.Type;
            }
        }
    }

    private ITypeSymbol? TryGetUnionOperandValueType(BoundCastExpression cast)
    {
        if (cast.Type is not IUnionTypeSymbol)
            return null;

        var operandType = cast.Expression.Type;
        if (operandType is null || operandType.TypeKind == TypeKind.Error)
            return null;

        var operandClr = ResolveClrType(operandType);
        if (!operandClr.IsValueType)
            return null;

        var targetClr = ResolveClrType(cast.Type);
        if (targetClr.IsValueType)
            return null;

        return operandType;
    }

    public MemberInfo? GetMemberBuilder(SourceSymbol sourceSymbol) => MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(sourceSymbol);

    private SemanticModel ResolveSemanticModel(SyntaxNode syntaxNode)
    {
        var syntaxTree = syntaxNode.SyntaxTree!;

        if (!_semanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            semanticModel = Compilation.GetSemanticModel(syntaxTree);
            _semanticModels[syntaxTree] = semanticModel;
        }

        return semanticModel;
    }
}
