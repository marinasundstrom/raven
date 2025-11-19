using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodBodyGenerator
{
    private TypeBuilder _typeBuilder;
    private MethodBase _methodBase;
    private Compilation _compilation;
    private IMethodSymbol _methodSymbol;
    private TypeGenerator.LambdaClosure? _lambdaClosure;
    private readonly Dictionary<ILabelSymbol, ILLabel> _labels = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<ILabelSymbol, Scope> _labelScopes = new(SymbolEqualityComparer.Default);
    private ILLabel? _returnLabel;
    private IILocal? _returnValueLocal;

    public MethodBodyGenerator(MethodGenerator methodGenerator)
    {
        MethodGenerator = methodGenerator;
    }

    public Compilation Compilation => _compilation ??= MethodGenerator.Compilation;
    public MethodGenerator MethodGenerator { get; }
    public IMethodSymbol MethodSymbol => _methodSymbol ??= MethodGenerator.MethodSymbol;
    public TypeBuilder TypeBuilder => _typeBuilder ??= MethodGenerator.TypeGenerator.TypeBuilder!;
    public MethodBase MethodBase => _methodBase ??= MethodGenerator.MethodBase;

    private BaseGenerator baseGenerator;
    private Scope scope;

    public IILBuilder ILGenerator { get; private set; }

    internal bool TryGetCapturedField(ISymbol symbol, out FieldBuilder fieldBuilder)
    {
        if (_lambdaClosure is null)
        {
            fieldBuilder = default!;
            return false;
        }

        return _lambdaClosure.TryGetField(symbol, out fieldBuilder);
    }

    internal ILLabel GetOrCreateLabel(ILabelSymbol labelSymbol)
    {
        if (!_labels.TryGetValue(labelSymbol, out var label))
        {
            label = ILGenerator.DefineLabel();
            _labels[labelSymbol] = label;
        }

        return label;
    }

    internal ILLabel GetOrCreateReturnLabel()
    {
        return _returnLabel ??= ILGenerator.DefineLabel();
    }

    internal IILocal EnsureReturnValueLocal()
    {
        if (_returnValueLocal is not null)
            return _returnValueLocal;

        var returnType = MethodSymbol.ReturnType;
        if (returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
            throw new InvalidOperationException("Void-like methods do not require a return value local.");

        var clrType = returnType.GetClrType(Compilation);
        _returnValueLocal = ILGenerator.DeclareLocal(clrType);
        return _returnValueLocal;
    }

    internal bool TryGetReturnValueLocal(out IILocal? local)
    {
        local = _returnValueLocal;
        return local is not null;
    }

    internal void RegisterLabelScope(ILabelSymbol labelSymbol, Scope scope)
    {
        _labelScopes[labelSymbol] = scope;
    }

    internal Scope? GetLabelScope(ILabelSymbol labelSymbol)
    {
        return _labelScopes.TryGetValue(labelSymbol, out var scope)
            ? scope
            : null;
    }

    internal void EmitLoadClosure()
    {
        if (_lambdaClosure is null)
            throw new InvalidOperationException("No closure parameter available for this lambda.");

        ILGenerator.Emit(OpCodes.Ldarg_0);
    }

    public void Emit()
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = MethodGenerator.ILBuilderFactory.Create(MethodGenerator);

        if (MethodSymbol is SynthesizedMainMethodSymbol mainSymbol && mainSymbol.AsyncImplementation is { } asyncImplementation)
        {
            EmitTopLevelMainBridge(mainSymbol, asyncImplementation);
            return;
        }

        if (MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
        {
            EmitAsyncStateMachineMethod(asyncStateMachine);
            return;
        }

        if (MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol iteratorType)
        {
            EmitIteratorMethod(iteratorType);
            return;
        }

        if (MethodSymbol.MethodKind == MethodKind.PropertyGet &&
            MethodSymbol.ContainingType.TryGetDiscriminatedUnionCase() is not null &&
            MethodSymbol.ContainingSymbol is SourcePropertySymbol unionCaseProperty &&
            unionCaseProperty.BackingField is SourceFieldSymbol unionCaseField)
        {
            EmitUnionCasePropertyGetter(unionCaseProperty, unionCaseField);
            return;
        }

        if (MethodSymbol.MethodKind == MethodKind.Conversion &&
            MethodSymbol.ReturnType.TryGetDiscriminatedUnion() is not null &&
            MethodSymbol.Parameters.Length == 1 &&
            MethodSymbol.Parameters[0].Type.TryGetDiscriminatedUnionCase() is not null &&
            MethodSymbol.ContainingType is SourceDiscriminatedUnionSymbol conversionUnion)
        {
            EmitDiscriminatedUnionConversion(conversionUnion);
            return;
        }

        if (MethodSymbol.ContainingType is SourceDiscriminatedUnionSymbol tryGetUnion &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Boolean &&
            MethodSymbol.Parameters.Length == 1 &&
            MethodSymbol.Parameters[0].RefKind == RefKind.Ref &&
            MethodSymbol.Parameters[0].Type.TryGetDiscriminatedUnionCase() is { } tryGetCase &&
            MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            MethodSymbol.Name.StartsWith("TryGet", StringComparison.Ordinal))
        {
            EmitDiscriminatedUnionTryGetMethod(tryGetUnion, tryGetCase, MethodSymbol.Parameters[0]);
            return;
        }

        if (MethodSymbol.Name == nameof(object.ToString) &&
            MethodSymbol.Parameters.Length == 0 &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_String)
        {
            if (MethodSymbol.ContainingType is SourceDiscriminatedUnionCaseTypeSymbol caseType)
            {
                EmitUnionCaseToString(caseType);
                return;
            }

            if (MethodSymbol.ContainingType is SourceDiscriminatedUnionSymbol unionType)
            {
                EmitDiscriminatedUnionToString(unionType);
                return;
            }
        }

        var syntaxReference = MethodSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var syntax = syntaxReference.GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        BoundBlockStatement? boundBody = syntax switch
        {
            MethodDeclarationSyntax m when m.Body != null => semanticModel.GetBoundNode(m.Body) as BoundBlockStatement,
            FunctionStatementSyntax l when l.Body != null => semanticModel.GetBoundNode(l.Body) as BoundBlockStatement,
            BaseConstructorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body) as BoundBlockStatement,
            AccessorDeclarationSyntax a when a.Body != null => semanticModel.GetBoundNode(a.Body) as BoundBlockStatement,
            _ => null
        };

        BoundExpression? expressionBody = syntax switch
        {
            MethodDeclarationSyntax m when m.ExpressionBody is not null
                => semanticModel.GetBoundNode(m.ExpressionBody.Expression) as BoundExpression,
            BaseConstructorDeclarationSyntax c when c.ExpressionBody is not null
                => semanticModel.GetBoundNode(c.ExpressionBody.Expression) as BoundExpression,
            AccessorDeclarationSyntax a when a.ExpressionBody is not null
                => semanticModel.GetBoundNode(a.ExpressionBody.Expression) as BoundExpression,
            FunctionStatementSyntax l when l.ExpressionBody is not null
                => semanticModel.GetBoundNode(l.ExpressionBody.Expression) as BoundExpression,
            _ => null
        };

        if (boundBody != null)
            DeclareLocals(boundBody);

        switch (syntax)
        {
            case CompilationUnitSyntax compilationUnit:
                if (MethodSymbol is SourceMethodSymbol &&
                    semanticModel.GetBoundNode(compilationUnit) is BoundBlockStatement topLevelBody)
                {
                    DeclareLocals(topLevelBody);
                    EmitMethodBlock(topLevelBody);
                    break;
                }

                foreach (var localDeclStmt in syntax.DescendantNodes()
                    .OfType<LocalDeclarationStatementSyntax>())
                {
                    foreach (var localDeclarator in localDeclStmt.Declaration.Declarators)
                    {
                        var localSymbol = GetDeclaredSymbol<ILocalSymbol>(localDeclarator);
                        if (localSymbol?.Type is null)
                            continue;

                        var clrType = ResolveClrType(localSymbol.Type);
                        var builder = ILGenerator.DeclareLocal(clrType);
                        builder.SetLocalSymInfo(localSymbol.Name);

                        scope.AddLocal(localSymbol, builder);
                    }
                }

                var topLevelDisposables = ImmutableArray.CreateBuilder<ILocalSymbol>();

                foreach (var usingDeclStmt in syntax.DescendantNodes()
                    .OfType<UsingDeclarationStatementSyntax>())
                {
                    foreach (var localDeclarator in usingDeclStmt.Declaration.Declarators)
                    {
                        var localSymbol = GetDeclaredSymbol<ILocalSymbol>(localDeclarator);
                        if (localSymbol?.Type is null)
                            continue;

                        var clrType = ResolveClrType(localSymbol.Type);
                        var builder = ILGenerator.DeclareLocal(clrType);
                        builder.SetLocalSymInfo(localSymbol.Name);

                        scope.AddLocal(localSymbol, builder);

                        if (usingDeclStmt.Parent is GlobalStatementSyntax)
                            topLevelDisposables.Add(localSymbol);
                    }
                }

                foreach (var localFunctionStmt in compilationUnit.DescendantNodes().OfType<FunctionStatementSyntax>())
                {
                    var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
                    if (methodSymbol is null)
                        continue;
                    if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
                        continue;
                    EmitFunction(localFunctionStmt);
                }

                var statements = GetTopLevelStatements(compilationUnit);
                EmitIL(statements, topLevelDisposables.ToImmutable());
                break;

            case FunctionStatementSyntax functionStatement:
                if (boundBody != null)
                {
                    EmitMethodBlock(boundBody);
                }
                else if (expressionBody is not null)
                {
                    EmitExpressionBody(expressionBody);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case MethodDeclarationSyntax methodDeclaration:
                if (boundBody != null)
                    EmitMethodBlock(boundBody);
                else if (expressionBody is not null)
                    EmitExpressionBody(expressionBody);
                else
                    ILGenerator.Emit(OpCodes.Ret);
                break;

            case BaseConstructorDeclarationSyntax constructorDeclaration:
                var ordinaryConstr = !MethodSymbol.IsNamedConstructor;

                if (ordinaryConstr && !MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);

                if (boundBody != null)
                    EmitMethodBlock(boundBody, includeImplicitReturn: false);
                else if (expressionBody is not null)
                    EmitExpressionBody(expressionBody, includeReturn: !ordinaryConstr);

                if (ordinaryConstr)
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case AccessorDeclarationSyntax accessorDeclaration:
                if (boundBody != null)
                {
                    EmitMethodBlock(boundBody);
                }
                else if (expressionBody is not null)
                {
                    if (MethodSymbol.MethodKind == MethodKind.PropertyGet)
                    {
                        new ExpressionGenerator(baseGenerator, expressionBody).Emit();
                    }
                    else
                    {
                        if (expressionBody is BoundAssignmentExpression assignment)
                        {
                            var stmt = new BoundAssignmentStatement(assignment);
                            new StatementGenerator(baseGenerator, stmt).Emit();
                        }
                        else
                        {
                            var stmt = new BoundExpressionStatement(expressionBody);
                            new StatementGenerator(baseGenerator, stmt).Emit();
                        }
                    }

                    ILGenerator.Emit(OpCodes.Ret);
                }
                else if (MethodSymbol.ContainingSymbol is SourcePropertySymbol propertySymbol &&
                         propertySymbol.BackingField is SourceFieldSymbol backingField)
                {
                    EmitAutoPropertyAccessor(accessorDeclaration, propertySymbol, backingField);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case ClassDeclarationSyntax:
                if (!MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);

                ILGenerator.Emit(OpCodes.Ret);
                break;
            case UnionCaseClauseSyntax:
                EmitUnionCaseConstructor();
                break;

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
        }
    }

    private void EmitTopLevelMainBridge(SynthesizedMainMethodSymbol mainMethod, SynthesizedMainAsyncMethodSymbol asyncImplementation)
    {
        if (MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(asyncImplementation) is not MethodInfo asyncMethodInfo)
            throw new NotSupportedException("Async entry point implementation is not defined.");

        if (MethodSymbol.Parameters.Length == 1)
            ILGenerator.Emit(OpCodes.Ldarg_0);

        ILGenerator.Emit(OpCodes.Call, asyncMethodInfo);

        var returnType = asyncMethodInfo.ReturnType;
        var getAwaiter = returnType.GetMethod("GetAwaiter", Type.EmptyTypes)
            ?? throw new NotSupportedException("Async entry point does not expose GetAwaiter().");

        var callAwaiterOpCode = returnType.IsValueType ? OpCodes.Call : OpCodes.Callvirt;
        ILGenerator.Emit(callAwaiterOpCode, getAwaiter);

        var awaiterType = getAwaiter.ReturnType;
        var awaiterLocal = ILGenerator.DeclareLocal(awaiterType);
        awaiterLocal.SetLocalSymInfo("awaiter");
        ILGenerator.Emit(OpCodes.Stloc, awaiterLocal);
        ILGenerator.Emit(OpCodes.Ldloca, awaiterLocal);

        var getResult = awaiterType.GetMethod("GetResult", Type.EmptyTypes)
            ?? throw new NotSupportedException("Awaiter does not expose GetResult().");
        ILGenerator.Emit(OpCodes.Call, getResult);

        if (mainMethod.ReturnType.SpecialType == SpecialType.System_Unit)
        {
            if (getResult.ReturnType != typeof(void))
                ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitIteratorMethod(SynthesizedIteratorTypeSymbol iteratorType)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.Constructor))
        {
            EmitConstructorInitializer();
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var body = GetIteratorBody(iteratorType);
        if (body is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        DeclareLocals(body);
        EmitMethodBlock(body);
    }

    private void EmitAsyncStateMachineMethod(SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, asyncStateMachine.Constructor))
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var body = GetAsyncStateMachineBody(asyncStateMachine);
        if (body is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        DeclareLocals(body);
        EmitMethodBlock(body);
    }

    private BoundBlockStatement? GetAsyncStateMachineBody(SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, asyncStateMachine.MoveNextMethod))
            return asyncStateMachine.MoveNextBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, asyncStateMachine.SetStateMachineMethod))
            return asyncStateMachine.SetStateMachineBody;

        return null;
    }

    private BoundBlockStatement? GetIteratorBody(SynthesizedIteratorTypeSymbol iteratorType)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.MoveNextMethod))
            return iteratorType.MoveNextBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.DisposeMethod))
            return iteratorType.DisposeBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.ResetMethod))
            return iteratorType.ResetBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.CurrentProperty.GetMethod))
            return iteratorType.CurrentGetterBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.NonGenericCurrentProperty.GetMethod))
            return iteratorType.NonGenericCurrentGetterBody;

        if (iteratorType.GenericGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.GenericGetEnumeratorMethod))
            return iteratorType.GenericGetEnumeratorBody;

        if (iteratorType.NonGenericGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.NonGenericGetEnumeratorMethod))
            return iteratorType.NonGenericGetEnumeratorBody;

        return null;
    }

    public void EmitLambda(BoundLambdaExpression lambda, TypeGenerator.LambdaClosure? closure)
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = MethodGenerator.ILBuilderFactory.Create(MethodGenerator);

        _lambdaClosure = closure;

        try
        {
            if (_lambdaClosure is not null)
                InitializeCapturedParameters();

            if (lambda.Body is BoundBlockExpression blockExpression)
            {
                var block = new BoundBlockStatement(blockExpression.Statements);
                DeclareLocals(block);
                EmitMethodBlock(block);
                return;
            }

            var returnStatement = new BoundReturnStatement(lambda.Body);
            EmitStatement(returnStatement);
        }
        finally
        {
            _lambdaClosure = null;
        }
    }

    private void InitializeCapturedParameters()
    {
        if (_lambdaClosure is null)
            return;

        foreach (var parameter in MethodSymbol.Parameters)
        {
            if (!_lambdaClosure.TryGetField(parameter, out var fieldBuilder))
                continue;

            ILGenerator.Emit(OpCodes.Ldarg_0);

            var parameterBuilder = MethodGenerator.GetParameterBuilder(parameter);
            var position = parameterBuilder.Position;
            if (MethodSymbol.IsStatic)
                position -= 1;

            ILGenerator.Emit(OpCodes.Ldarg, position);
            ILGenerator.Emit(OpCodes.Stfld, fieldBuilder);
        }
    }

    private void EmitFieldInitializers(bool isStatic)
    {
        var fields = MethodSymbol.ContainingType!
            .GetMembers()
            .OfType<SourceFieldSymbol>()
            .Where(f => f.IsStatic == isStatic && f.Initializer is not null);

        foreach (var field in fields)
        {
            BoundExpression assignment = new BoundFieldAssignmentExpression(
                isStatic ? null : new BoundSelfExpression(MethodSymbol.ContainingType!),
                field,
                field.Initializer!,
                Compilation.GetSpecialType(SpecialType.System_Unit));

            var statement = new BoundAssignmentStatement((BoundAssignmentExpression)assignment);
            new StatementGenerator(baseGenerator, statement).Emit();
        }
    }

    private void EmitAutoPropertyAccessor(
        AccessorDeclarationSyntax accessorDeclaration,
        SourcePropertySymbol propertySymbol,
        SourceFieldSymbol backingField)
    {
        var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        if (accessorDeclaration.Kind == SyntaxKind.GetAccessorDeclaration)
        {
            if (propertySymbol.IsStatic)
            {
                ILGenerator.Emit(OpCodes.Ldsfld, fieldInfo);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            }
        }
        else
        {
            if (!propertySymbol.IsStatic)
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Ldarg_1);
                ILGenerator.Emit(OpCodes.Stfld, fieldInfo);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Stsfld, fieldInfo);
            }
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitUnionCasePropertyGetter(SourcePropertySymbol propertySymbol, SourceFieldSymbol backingField)
    {
        var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        if (propertySymbol.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Ldsfld, fieldInfo);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitDiscriminatedUnionConversion(SourceDiscriminatedUnionSymbol unionSymbol)
    {
        if (MethodSymbol.Parameters.Length != 1)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var parameter = MethodSymbol.Parameters[0];
        var parameterType = parameter.Type;
        var caseSymbol = parameterType.TryGetDiscriminatedUnionCase();

        if (caseSymbol is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var unionClrType = InstantiateType(
            MethodGenerator.TypeGenerator.TypeBuilder
                ?? ResolveClrType(MethodSymbol.ContainingType!));
        var payloadType = unionSymbol.PayloadField.Type;
        var unionLocal = ILGenerator.DeclareLocal(unionClrType);

        ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
        ILGenerator.Emit(OpCodes.Initobj, unionClrType);

        var discriminatorField = ((SourceFieldSymbol)unionSymbol.DiscriminatorField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4, caseSymbol.Ordinal);
        ILGenerator.Emit(OpCodes.Stfld, discriminatorField);

        var payloadField = ((SourceFieldSymbol)unionSymbol.PayloadField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
        ILGenerator.Emit(OpCodes.Ldarg_0);

        if (parameterType.IsValueType && !payloadType.IsValueType)
        {
            var parameterClrType = ResolveUnionCaseClrType(parameterType);
            ILGenerator.Emit(OpCodes.Box, parameterClrType);
        }

        ILGenerator.Emit(OpCodes.Stfld, payloadField);
        ILGenerator.Emit(OpCodes.Ldloc, unionLocal);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitDiscriminatedUnionTryGetMethod(
        SourceDiscriminatedUnionSymbol unionSymbol,
        IDiscriminatedUnionCaseSymbol caseSymbol,
        IParameterSymbol targetParameter)
    {
        var discriminatorField = ((SourceFieldSymbol)unionSymbol.DiscriminatorField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var payloadField = ((SourceFieldSymbol)unionSymbol.PayloadField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var failLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, discriminatorField);
        ILGenerator.Emit(OpCodes.Ldc_I4, caseSymbol.Ordinal);
        ILGenerator.Emit(OpCodes.Bne_Un_S, failLabel);

        EmitStoreDiscriminatedUnionPayload(targetParameter, unionSymbol.PayloadField.Type, payloadField);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(failLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitStoreDiscriminatedUnionPayload(
        IParameterSymbol parameter,
        ITypeSymbol payloadType,
        FieldInfo payloadField)
    {
        var parameterPosition = GetParameterPosition(parameter);
        ILGenerator.Emit(OpCodes.Ldarg, parameterPosition);
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, payloadField);

        var parameterType = parameter.Type;

        if (parameterType.IsValueType)
        {
            var parameterClrType = ResolveUnionCaseClrType(parameterType);

            if (!payloadType.IsValueType)
                ILGenerator.Emit(OpCodes.Unbox_Any, parameterClrType);

            ILGenerator.Emit(OpCodes.Stobj, parameterClrType);
            return;
        }

        if (payloadType.IsValueType)
        {
            var payloadClrType = ResolveClrType(payloadType);
            ILGenerator.Emit(OpCodes.Box, payloadClrType);
        }

        if (!SymbolEqualityComparer.Default.Equals(parameterType, payloadType))
        {
            var parameterClrType = ResolveClrType(parameterType);
            ILGenerator.Emit(OpCodes.Castclass, parameterClrType);
        }

        ILGenerator.Emit(OpCodes.Stind_Ref);
    }

    private void EmitDiscriminatedUnionToString(SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var payloadField = ((SourceFieldSymbol)unionSymbol.PayloadField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var objectToString = typeof(object).GetMethod(nameof(ToString), Type.EmptyTypes)!;

        var payloadPresentLabel = ILGenerator.DefineLabel();
        var resultPresentLabel = ILGenerator.DefineLabel();
        var storeResultLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        var resultLocal = ILGenerator.DeclareLocal(typeof(string));
        resultLocal.SetLocalSymInfo("result");

        // var result = _<Payload>?.ToString() ?? "<Uninitialized>";
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, payloadField);
        ILGenerator.Emit(OpCodes.Dup);
        ILGenerator.Emit(OpCodes.Brtrue, payloadPresentLabel);

        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Ldnull);
        ILGenerator.Emit(OpCodes.Br, resultPresentLabel);

        ILGenerator.MarkLabel(payloadPresentLabel);
        ILGenerator.Emit(OpCodes.Callvirt, objectToString);

        ILGenerator.MarkLabel(resultPresentLabel);
        ILGenerator.Emit(OpCodes.Dup);
        ILGenerator.Emit(OpCodes.Brtrue, storeResultLabel);

        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Ldstr, "<Uninitialized>");

        ILGenerator.MarkLabel(storeResultLabel);
        ILGenerator.Emit(OpCodes.Stloc, resultLocal);
        ILGenerator.Emit(OpCodes.Br, endLabel);

        ILGenerator.MarkLabel(endLabel);
        ILGenerator.Emit(OpCodes.Ldloc, resultLocal);
        ILGenerator.Emit(OpCodes.Ret);
    }


    private void EmitUnionCaseToString(SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var builderType = typeof(StringBuilder);
        var builderCtor = builderType.GetConstructor(Type.EmptyTypes)!;
        var appendString = builderType.GetMethod(nameof(StringBuilder.Append), new[] { typeof(string) })!;
        var appendChar = builderType.GetMethod(nameof(StringBuilder.Append), new[] { typeof(char) })!;
        var builderToString = builderType.GetMethod(nameof(StringBuilder.ToString), Type.EmptyTypes)!;
        var stringIndexOf = typeof(string).GetMethod(nameof(string.IndexOf), new[] { typeof(char) })!;
        var stringSubstring = typeof(string).GetMethod(nameof(string.Substring), new[] { typeof(int), typeof(int) })!;
        var stringReplace = typeof(string).GetMethod(nameof(string.Replace), new[] { typeof(string), typeof(string) })!;
        var objectToString = typeof(object).GetMethod(nameof(ToString), Type.EmptyTypes)!;
        var typeType = typeof(Type);
        var typeFromHandle = typeType.GetMethod(nameof(Type.GetTypeFromHandle), new[] { typeof(RuntimeTypeHandle) })!;
        var typeNameGetter = typeType.GetProperty(nameof(Type.Name))!.GetMethod!;
        var typeGenericArgumentsGetter = typeType.GetProperty(nameof(Type.GenericTypeArguments))!.GetMethod!;

        var parameterInfos = CollectUnionCaseParameters(caseSymbol);

        var builderLocal = ILGenerator.DeclareLocal(builderType);
        builderLocal.SetLocalSymInfo("builder");
        var unionTypeLocal = ILGenerator.DeclareLocal(typeType);
        unionTypeLocal.SetLocalSymInfo("unionType");
        var nameLocal = ILGenerator.DeclareLocal(typeof(string));
        nameLocal.SetLocalSymInfo("name");
        var tickIndexLocal = ILGenerator.DeclareLocal(typeof(int));
        tickIndexLocal.SetLocalSymInfo("tickIndex");
        var argsLocal = ILGenerator.DeclareLocal(typeof(Type[]));
        argsLocal.SetLocalSymInfo("typeArgs");
        var argsLengthLocal = ILGenerator.DeclareLocal(typeof(int));
        argsLengthLocal.SetLocalSymInfo("typeArgLength");
        var argIndexLocal = ILGenerator.DeclareLocal(typeof(int));
        argIndexLocal.SetLocalSymInfo("typeArgIndex");
        var argTypeLocal = ILGenerator.DeclareLocal(typeType);
        argTypeLocal.SetLocalSymInfo("typeArg");
        var argNameLocal = ILGenerator.DeclareLocal(typeof(string));
        argNameLocal.SetLocalSymInfo("typeArgName");
        IILocal? firstParameterLocal = null;

        if (parameterInfos.Count > 0)
        {
            firstParameterLocal = ILGenerator.DeclareLocal(typeof(bool));
            firstParameterLocal.SetLocalSymInfo("firstParameter");
        }

        ILGenerator.Emit(OpCodes.Newobj, builderCtor);
        ILGenerator.Emit(OpCodes.Stloc, builderLocal);

        var unionClrType = InstantiateType(ResolveClrType(caseSymbol.Union));

        EmitUnionFriendlyName(
            unionClrType,
            builderLocal,
            unionTypeLocal,
            nameLocal,
            tickIndexLocal,
            argsLocal,
            argsLengthLocal,
            argIndexLocal,
            argTypeLocal,
            argNameLocal,
            appendString,
            appendChar,
            typeFromHandle,
            typeNameGetter,
            typeGenericArgumentsGetter,
            stringIndexOf,
            stringSubstring);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'.');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, caseSymbol.Name);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        if (parameterInfos.Count > 0)
        {
            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'(');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Stloc, firstParameterLocal!);

            foreach (var parameter in parameterInfos)
            {
                var skipParameterComma = ILGenerator.DefineLabel();
                ILGenerator.Emit(OpCodes.Ldloc, firstParameterLocal!);
                ILGenerator.Emit(OpCodes.Brtrue_S, skipParameterComma);
                ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
                ILGenerator.Emit(OpCodes.Ldstr, ", ");
                ILGenerator.Emit(OpCodes.Callvirt, appendString);
                ILGenerator.Emit(OpCodes.Pop);
                ILGenerator.MarkLabel(skipParameterComma);

                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Stloc, firstParameterLocal!);

                ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
                ILGenerator.Emit(OpCodes.Ldstr, parameter.Name);
                ILGenerator.Emit(OpCodes.Callvirt, appendString);
                ILGenerator.Emit(OpCodes.Pop);

                ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
                ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'=');
                ILGenerator.Emit(OpCodes.Callvirt, appendChar);
                ILGenerator.Emit(OpCodes.Pop);

                EmitAppendFormattedValue(
                    builderLocal,
                    () =>
                    {
                        ILGenerator.Emit(OpCodes.Ldarg_0);
                        ILGenerator.Emit(OpCodes.Ldfld, parameter.Field);
                    },
                    parameter.Type,
                    appendString,
                    appendChar,
                    stringReplace,
                    objectToString);
            }

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)')');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);
        }

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Callvirt, builderToString);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private List<(string Name, ITypeSymbol Type, FieldInfo Field)> CollectUnionCaseParameters(SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var parameterInfos = new List<(string, ITypeSymbol, FieldInfo)>();

        foreach (var parameter in caseSymbol.ConstructorParameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.Type is null)
                continue;

            if (caseSymbol
                    .GetMembers(parameter.Name)
                    .OfType<IPropertySymbol>()
                    .FirstOrDefault() is not SourcePropertySymbol property)
            {
                continue;
            }

            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
            parameterInfos.Add((parameter.Name, parameter.Type, fieldInfo));
        }

        return parameterInfos;
    }

    private void EmitUnionFriendlyName(
        Type unionClrType,
        IILocal builderLocal,
        IILocal unionTypeLocal,
        IILocal nameLocal,
        IILocal tickIndexLocal,
        IILocal argsLocal,
        IILocal argsLengthLocal,
        IILocal argIndexLocal,
        IILocal argTypeLocal,
        IILocal argNameLocal,
        MethodInfo appendString,
        MethodInfo appendChar,
        MethodInfo typeFromHandle,
        MethodInfo typeNameGetter,
        MethodInfo typeGenericArgumentsGetter,
        MethodInfo stringIndexOf,
        MethodInfo stringSubstring)
    {
        ILGenerator.Emit(OpCodes.Ldtoken, unionClrType);
        ILGenerator.Emit(OpCodes.Call, typeFromHandle);
        ILGenerator.Emit(OpCodes.Stloc, unionTypeLocal);

        ILGenerator.Emit(OpCodes.Ldloc, unionTypeLocal);
        ILGenerator.Emit(OpCodes.Callvirt, typeNameGetter);
        ILGenerator.Emit(OpCodes.Stloc, nameLocal);

        ILGenerator.Emit(OpCodes.Ldloc, nameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'`');
        ILGenerator.Emit(OpCodes.Callvirt, stringIndexOf);
        ILGenerator.Emit(OpCodes.Stloc, tickIndexLocal);

        var noTickLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Blt_S, noTickLabel);
        ILGenerator.Emit(OpCodes.Ldloc, nameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Callvirt, stringSubstring);
        ILGenerator.Emit(OpCodes.Stloc, nameLocal);
        ILGenerator.MarkLabel(noTickLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, nameLocal);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, unionTypeLocal);
        ILGenerator.Emit(OpCodes.Callvirt, typeGenericArgumentsGetter);
        ILGenerator.Emit(OpCodes.Stloc, argsLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argsLocal);
        ILGenerator.Emit(OpCodes.Ldlen);
        ILGenerator.Emit(OpCodes.Conv_I4);
        ILGenerator.Emit(OpCodes.Stloc, argsLengthLocal);

        var skipGenericsLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, argsLengthLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ble_S, skipGenericsLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'<');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Stloc, argIndexLocal);

        var typeLoopCheck = ILGenerator.DefineLabel();
        var typeLoopBody = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Br, typeLoopCheck);
        ILGenerator.MarkLabel(typeLoopBody);

        var skipCommaLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Brfalse_S, skipCommaLabel);
        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, ", ");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.MarkLabel(skipCommaLabel);

        ILGenerator.Emit(OpCodes.Ldloc, argsLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Ldelem_Ref);
        ILGenerator.Emit(OpCodes.Stloc, argTypeLocal);

        ILGenerator.Emit(OpCodes.Ldloc, argTypeLocal);
        ILGenerator.Emit(OpCodes.Callvirt, typeNameGetter);
        ILGenerator.Emit(OpCodes.Stloc, argNameLocal);

        ILGenerator.Emit(OpCodes.Ldloc, argNameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'`');
        ILGenerator.Emit(OpCodes.Callvirt, stringIndexOf);
        ILGenerator.Emit(OpCodes.Stloc, tickIndexLocal);

        var argNoTickLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Blt_S, argNoTickLabel);
        ILGenerator.Emit(OpCodes.Ldloc, argNameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Callvirt, stringSubstring);
        ILGenerator.Emit(OpCodes.Stloc, argNameLocal);
        ILGenerator.MarkLabel(argNoTickLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argNameLocal);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Add);
        ILGenerator.Emit(OpCodes.Stloc, argIndexLocal);

        ILGenerator.MarkLabel(typeLoopCheck);
        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argsLengthLocal);
        ILGenerator.Emit(OpCodes.Blt, typeLoopBody);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'>');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.MarkLabel(skipGenericsLabel);
    }

    private void EmitAppendFormattedValue(
        IILocal builderLocal,
        Action emitLoadValue,
        ITypeSymbol valueType,
        MethodInfo appendString,
        MethodInfo appendChar,
        MethodInfo stringReplace,
        MethodInfo objectToString)
    {
        if (valueType.SpecialType == SpecialType.System_String)
        {
            var stringLocal = ILGenerator.DeclareLocal(typeof(string));
            stringLocal.SetLocalSymInfo("stringValue");

            emitLoadValue();
            ILGenerator.Emit(OpCodes.Stloc, stringLocal);

            var nonNullLabel = ILGenerator.DefineLabel();
            var endLabel = ILGenerator.DefineLabel();

            ILGenerator.Emit(OpCodes.Ldloc, stringLocal);
            ILGenerator.Emit(OpCodes.Brtrue_S, nonNullLabel);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldstr, "null");
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Br, endLabel);

            ILGenerator.MarkLabel(nonNullLabel);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\"');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldloc, stringLocal);
            ILGenerator.Emit(OpCodes.Ldstr, "\"");
            ILGenerator.Emit(OpCodes.Ldstr, "\\\"");
            ILGenerator.Emit(OpCodes.Callvirt, stringReplace);
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\"');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.MarkLabel(endLabel);
            return;
        }

        var valueLocal = ILGenerator.DeclareLocal(typeof(object));
        valueLocal.SetLocalSymInfo("value");

        emitLoadValue();

        if (valueType.IsValueType)
        {
            var parameterClrType = ResolveClrType(valueType);
            ILGenerator.Emit(OpCodes.Box, parameterClrType);
        }

        ILGenerator.Emit(OpCodes.Stloc, valueLocal);

        var notNullLabel = ILGenerator.DefineLabel();
        var doneLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);
        ILGenerator.Emit(OpCodes.Brtrue_S, notNullLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, "null");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Br, doneLabel);

        ILGenerator.MarkLabel(notNullLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);
        ILGenerator.Emit(OpCodes.Callvirt, objectToString);
        ILGenerator.Emit(OpCodes.Dup);

        var hasTextLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Brtrue_S, hasTextLabel);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Ldstr, "null");
        ILGenerator.MarkLabel(hasTextLabel);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.MarkLabel(doneLabel);
    }

    private void EmitUnionCaseConstructor()
    {
        if (!MethodSymbol.IsConstructor)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        EmitFieldInitializers(MethodSymbol.IsStatic);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitExpressionBody(BoundExpression expression, bool includeReturn = true)
    {
        var returnType = MethodSymbol.ReturnType;

        if (returnType is null || returnType.SpecialType == SpecialType.System_Void)
        {
            EmitExpressionStatement(expression);

            if (includeReturn)
                ILGenerator.Emit(OpCodes.Ret);

            return;
        }

        new ExpressionGenerator(baseGenerator, expression).Emit();

        if (returnType.SpecialType == SpecialType.System_Unit)
        {
            ILGenerator.Emit(OpCodes.Pop);
        }
        else if (expression.Type is { IsValueType: true } expressionType &&
                 (returnType.SpecialType is SpecialType.System_Object || returnType is IUnionTypeSymbol))
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));
        }

        if (includeReturn)
            ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitExpressionStatement(BoundExpression expression)
    {
        var statement = new BoundExpressionStatement(expression);
        new StatementGenerator(baseGenerator, statement).Emit();
    }

    private void EmitFunction(FunctionStatementSyntax localFunctionStmt)
    {
        var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
        if (methodSymbol is null)
            return;

        if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
            return;

        var methodGenerator = new MethodGenerator(MethodGenerator.TypeGenerator, methodSymbol, MethodGenerator.ILBuilderFactory);
        MethodGenerator.TypeGenerator.Add(methodSymbol, methodGenerator);
        methodGenerator.DefineMethodBuilder();
    }

    private void DeclareLocals(BoundBlockStatement block)
    {
        DeclareLocals(scope, block);
    }

    internal void DeclareLocals(Scope targetScope, IEnumerable<BoundStatement> statements)
    {
        var block = statements as BoundBlockStatement ?? new BoundBlockStatement(statements);
        DeclareLocals(targetScope, block);
    }

    private void DeclareLocals(Scope targetScope, BoundBlockStatement block)
    {
        var collector = new LocalCollector(MethodSymbol);
        collector.Visit(block);

        foreach (var localSymbol in collector.Locals)
        {
            // Skip locals without a type. This can occur when the initializer
            // contains an early return, making the declaration unreachable.
            if (localSymbol.Type is null)
                continue;

            // Locals are declared once at the method scope. When the block is emitted
            // we may revisit the same locals to populate nested scopes with the
            // existing builders. Ensure we reuse the builder instead of declaring a
            // duplicate slot.
            var existingBuilder = targetScope.GetLocal(localSymbol);
            if (existingBuilder is not null)
            {
                targetScope.AddLocal(localSymbol, existingBuilder);
                continue;
            }

            var clrType = ResolveClrType(localSymbol.Type);
            var builder = ILGenerator.DeclareLocal(clrType);
            builder.SetLocalSymInfo(localSymbol.Name);
            targetScope.AddLocal(localSymbol, builder);
        }
    }

    private void EmitMethodBlock(BoundBlockStatement block, bool includeImplicitReturn = true)
    {
        EmitBlock(block, treatAsMethodBody: true, includeImplicitReturn);
    }

    private void EmitBoundBlock(BoundBlockStatement block)
    {
        EmitBlock(block, treatAsMethodBody: false, includeImplicitReturn: false);
    }

    private void EmitBlock(BoundBlockStatement block, bool treatAsMethodBody, bool includeImplicitReturn)
    {
        block = Lowerer.LowerBlock(MethodSymbol, block);
        var statements = block.Statements as IReadOnlyList<BoundStatement> ?? block.Statements.ToArray();
        var blockScope = new Scope(scope, block.LocalsToDispose);

        // Locals synthesized during lowering (e.g., iterator state machines) won't
        // be present in the original bound body we used for the initial declaration
        // pass. Ensure we register builders for any newly introduced locals so
        // downstream emitters can load and store them.
        DeclareLocals(blockScope, block);

        for (var i = 0; i < statements.Count; i++)
        {
            var statement = statements[i];

            // If this is the last statement in the block and the method expects a
            // value, treat a bare expression statement as an implicit return. This
            // allows functions to omit an explicit `return` for the final
            // expression, while still emitting any required boxing.
            if (treatAsMethodBody && includeImplicitReturn &&
                i == statements.Count - 1 &&
                MethodSymbol.ReturnType.SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit &&
                statement is BoundExpressionStatement exprStmt)
            {
                var returnStatement = new BoundReturnStatement(exprStmt.Expression);
                new StatementGenerator(blockScope, returnStatement).Emit();
                return;
            }

            new StatementGenerator(blockScope, statement).Emit();
        }

        blockScope.EmitDispose(block.LocalsToDispose);

        if (!treatAsMethodBody || !includeImplicitReturn)
            return;

        if (_returnLabel is ILLabel exitLabel)
        {
            ILGenerator.MarkLabel(exitLabel);

            if (TryGetReturnValueLocal(out var returnValueLocal) && returnValueLocal is not null)
                ILGenerator.Emit(OpCodes.Ldloc, returnValueLocal);

            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var endsWithTerminator = statements.Count > 0 &&
            statements[^1] is BoundReturnStatement or BoundThrowStatement;

        if (!endsWithTerminator && ShouldEmitImplicitReturn())
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
        }
    }

    private sealed class LocalCollector : Raven.CodeAnalysis.BoundTreeWalker
    {
        private readonly ISymbol _containingSymbol;

        public LocalCollector(ISymbol containingSymbol)
        {
            _containingSymbol = containingSymbol;
        }

        public List<ILocalSymbol> Locals { get; } = new();

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            foreach (var d in node.Declarators)
                Locals.Add(d.Local);

            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitAssignmentStatement(BoundAssignmentStatement node)
        {
            if (node.Expression is BoundPatternAssignmentExpression patternAssignment &&
                patternAssignment.Pattern is { } pattern)
            {
                foreach (var designator in pattern.GetDesignators())
                {
                    if (designator is BoundSingleVariableDesignator single &&
                        !Locals.Any(l => SymbolEqualityComparer.Default.Equals(l, single.Local)))
                    {
                        Locals.Add(single.Local);
                    }
                }
            }

            base.VisitAssignmentStatement(node);
        }

    }

    private void EmitIL(IEnumerable<StatementSyntax> statements, ImmutableArray<ILocalSymbol> localsToDispose, bool withReturn = true)
    {
        var statementArray = statements as StatementSyntax[] ?? statements.ToArray();

        if (statementArray.Length == 0)
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var executionScope = localsToDispose.IsDefaultOrEmpty
            ? scope
            : new Scope(scope, localsToDispose);

        var semanticModel = Compilation.GetSemanticModel(statementArray.First().SyntaxTree);

        foreach (var statement in statementArray)
        {
            var boundNode = semanticModel.GetBoundNode(statement) as BoundStatement;

            if (boundNode is null)
                continue;

            boundNode = Lowerer.LowerStatement(MethodSymbol, boundNode);
            new StatementGenerator(executionScope, boundNode).Emit();
        }

        executionScope.EmitDispose(localsToDispose);

        if (withReturn && ShouldEmitImplicitReturn())
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
        }
    }

    private bool ShouldEmitImplicitReturn()
    {
        var returnType = MethodSymbol.ReturnType;
        if (returnType is null)
            return true;

        return returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit;
    }

    private void EmitStatement(BoundStatement statement)
    {
        new StatementGenerator(scope, statement).Emit();
    }

    protected SymbolInfo GetSymbolInfo(SyntaxNode syntaxNode)
    {
        return Compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetSymbolInfo(syntaxNode);
    }

    protected TNode? GetDeclaredSymbol<TNode>(SyntaxNode syntaxNode)
        where TNode : class, ISymbol
    {
        return Compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetDeclaredSymbol(syntaxNode) as TNode;
    }

    private void EmitConstructorInitializer()
    {
        if (MethodSymbol is SourceMethodSymbol sourceMethod)
        {
            if (sourceMethod.ConstructorInitializer is { } initializer)
            {
                new ExpressionGenerator(baseGenerator, initializer).Emit();
                return;
            }

            if (sourceMethod.HasConstructorInitializerSyntax)
                return;
        }

        ILGenerator.Emit(OpCodes.Ldarg_0);
        var baseCtor = GetBaseConstructor();
        ILGenerator.Emit(OpCodes.Call, baseCtor);
    }

    private ConstructorInfo GetBaseConstructor()
    {
        var baseType = MethodSymbol.ContainingType!.BaseType!;
        var ctorSymbol = baseType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0)
            ?? throw new NotSupportedException("Base type requires a parameterless constructor");

        return ctorSymbol.GetClrConstructorInfo(MethodGenerator.TypeGenerator.CodeGen);
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(MethodGenerator.TypeGenerator.CodeGen);
    }

    private static IEnumerable<StatementSyntax> GetTopLevelStatements(CompilationUnitSyntax compilationUnit)
    {
        foreach (var member in compilationUnit.Members)
        {
            switch (member)
            {
                case GlobalStatementSyntax global:
                    yield return global.Statement;
                    break;
                case FileScopedNamespaceDeclarationSyntax fileScoped:
                    foreach (var nestedGlobal in fileScoped.Members.OfType<GlobalStatementSyntax>())
                        yield return nestedGlobal.Statement;
                    break;
            }
        }
    }

    private int GetParameterPosition(IParameterSymbol parameterSymbol)
    {
        var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);
        var position = parameterBuilder.Position;

        if (MethodSymbol.IsStatic)
            position -= 1;

        return position;
    }

    private Type ResolveUnionCaseClrType(ITypeSymbol caseTypeSymbol)
    {
        if (caseTypeSymbol is INamedTypeSymbol namedCase)
        {
            var caseGenerator = MethodGenerator.TypeGenerator.CodeGen.GetOrCreateTypeGenerator(namedCase);
            if (caseGenerator.TypeBuilder is null)
                caseGenerator.DefineTypeBuilder();

            if (caseGenerator.TypeBuilder is not null)
                return InstantiateType(caseGenerator.TypeBuilder);
        }

        return InstantiateType(ResolveClrType(caseTypeSymbol));
    }

    private static Type InstantiateType(Type type)
    {
        if (type is TypeBuilder typeBuilder && typeBuilder.ContainsGenericParameters)
        {
            var parameters = typeBuilder.GetGenericArguments();
            return parameters.Length == 0 ? typeBuilder : typeBuilder.MakeGenericType(parameters);
        }

        if (type.IsGenericTypeDefinition)
        {
            var parameters = type.GetGenericArguments();
            return parameters.Length == 0 ? type : type.MakeGenericType(parameters);
        }

        return type;
    }
}
