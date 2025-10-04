using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

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
                    EmitMethodBlock(boundBody);
                else
                    ILGenerator.Emit(OpCodes.Ret);
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

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
        }
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
                field.Initializer!);

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
                MethodSymbol.ReturnType.SpecialType is not SpecialType.System_Void &&
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
            if (node.Expression is BoundPatternAssignmentExpression patternAssignment)
            {
                foreach (var designator in patternAssignment.Pattern.GetDesignators())
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

        return ctorSymbol switch
        {
            SourceMethodSymbol sm => (ConstructorInfo)MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(sm)!,
            PEMethodSymbol pem => pem.GetConstructorInfo(),
            SubstitutedMethodSymbol sub => sub.GetConstructorInfo(MethodGenerator.TypeGenerator.CodeGen),
            _ => ResolveClrType(baseType).GetConstructor(Type.EmptyTypes)!
        };
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
}
