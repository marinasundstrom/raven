using System;
using System.Collections.Generic;
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
    private readonly HashSet<ILocalSymbol> _capturedLocals = new(SymbolEqualityComparer.Default);

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

    public ILGenerator ILGenerator { get; private set; }

    internal bool TryGetCapturedField(ISymbol symbol, out FieldBuilder fieldBuilder)
    {
        if (_lambdaClosure is null)
        {
            fieldBuilder = default!;
            return false;
        }

        return _lambdaClosure.TryGetField(symbol, out fieldBuilder);
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

        ILGenerator = (MethodBase as MethodBuilder)?.GetILGenerator()
                     ?? (MethodBase as ConstructorBuilder)?.GetILGenerator()
                     ?? throw new InvalidOperationException();

        var syntax = MethodSymbol.DeclaringSyntaxReferences.First().GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        BoundBlockStatement? boundBody = syntax switch
        {
            MethodDeclarationSyntax m when m.Body != null => semanticModel.GetBoundNode(m.Body) as BoundBlockStatement,
            FunctionStatementSyntax l when l.Body != null => semanticModel.GetBoundNode(l.Body) as BoundBlockStatement,
            BaseConstructorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body) as BoundBlockStatement,
            AccessorDeclarationSyntax a when a.Body != null => semanticModel.GetBoundNode(a.Body) as BoundBlockStatement,
            _ => null
        };

        if (boundBody != null)
            DeclareLocals(boundBody);

        switch (syntax)
        {
            case CompilationUnitSyntax compilationUnit:
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

                foreach (var localFunctionStmt in compilationUnit.DescendantNodes().OfType<FunctionStatementSyntax>())
                {
                    var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
                    if (methodSymbol is null)
                        continue;
                    if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
                        continue;
                    EmitFunction(localFunctionStmt);
                }

                var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                    .Select(x => x.Statement);
                EmitIL(statements);
                break;

            case FunctionStatementSyntax functionStatement:
                if (boundBody != null)
                    EmitBoundBlock(boundBody);
                else
                    ILGenerator.Emit(OpCodes.Ret);
                break;

            case MethodDeclarationSyntax methodDeclaration:
                if (boundBody != null)
                    EmitBoundBlock(boundBody);
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
                    EmitBoundBlock(boundBody, false);

                if (ordinaryConstr)
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case AccessorDeclarationSyntax accessorDeclaration:
                if (boundBody != null)
                {
                    EmitBoundBlock(boundBody);
                }
                else if (accessorDeclaration.ExpressionBody is not null)
                {
                    var boundExpr = (BoundExpression)semanticModel.GetBoundNode(accessorDeclaration.ExpressionBody.Expression)!;

                    if (MethodSymbol.MethodKind == MethodKind.PropertyGet)
                    {
                        new ExpressionGenerator(baseGenerator, boundExpr).Emit();
                    }
                    else
                    {
                        if (boundExpr is BoundAssignmentExpression assignment)
                        {
                            var stmt = new BoundAssignmentStatement(assignment);
                            new StatementGenerator(baseGenerator, stmt).Emit();
                        }
                        else
                        {
                            var stmt = new BoundExpressionStatement(boundExpr);
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

    public void EmitLambda(BoundLambdaExpression lambda, TypeGenerator.LambdaClosure? closure)
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = (MethodBase as MethodBuilder)?.GetILGenerator()
                     ?? (MethodBase as ConstructorBuilder)?.GetILGenerator()
                     ?? throw new InvalidOperationException();

        _lambdaClosure = closure;

        try
        {
            if (lambda.Body is BoundBlockExpression blockExpression)
            {
                var block = new BoundBlockStatement(blockExpression.Statements);
                DeclareLocals(block);
                EmitBoundBlock(block);
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

    private void EmitFunction(FunctionStatementSyntax localFunctionStmt)
    {
        var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
        if (methodSymbol is null)
            return;

        if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
            return;

        var methodGenerator = new MethodGenerator(MethodGenerator.TypeGenerator, methodSymbol);
        MethodGenerator.TypeGenerator.Add(methodSymbol, methodGenerator);
        methodGenerator.DefineMethodBuilder();
        methodGenerator.EmitBody();
    }

    internal bool IsCapturedLocal(ILocalSymbol local) => _capturedLocals.Contains(local);

    private static Type MakeStrongBoxType(Type elementType)
    {
        return typeof(System.Runtime.CompilerServices.StrongBox<>).MakeGenericType(elementType);
    }

    internal static FieldInfo GetStrongBoxValueField(Type strongBoxType)
    {
        var definition = strongBoxType.IsGenericType ? strongBoxType.GetGenericTypeDefinition() : strongBoxType;
        return definition.GetField("Value")
               ?? throw new InvalidOperationException($"StrongBox field missing: {strongBoxType.FullName}.Value");
    }

    private void DeclareLocals(BoundBlockStatement block)
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
            var localType = collector.CapturedLocals.Contains(localSymbol)
                ? MakeStrongBoxType(clrType)
                : clrType;

            if (collector.CapturedLocals.Contains(localSymbol))
                _capturedLocals.Add(localSymbol);

            var builder = ILGenerator.DeclareLocal(localType);
            builder.SetLocalSymInfo(localSymbol.Name);
            scope.AddLocal(localSymbol, builder);
        }
    }

    private void EmitBoundBlock(BoundBlockStatement block, bool withReturn = true)
    {
        for (var i = 0; i < block.Statements.Count(); i++)
        {
            var statement = block.Statements.ElementAt(i);

            // If this is the last statement in the block and the method expects a
            // value, treat a bare expression statement as an implicit return. This
            // allows functions to omit an explicit `return` for the final
            // expression, while still emitting any required boxing.
            var isLast = i == block.Statements.Count() - 1;
            if (withReturn && isLast &&
                MethodSymbol.ReturnType.SpecialType is not SpecialType.System_Void &&
                statement is BoundExpressionStatement exprStmt)
            {
                new ExpressionGenerator(baseGenerator, exprStmt.Expression).Emit();

                var expressionType = exprStmt.Expression.Type;
                var returnType = MethodSymbol.ReturnType;

                if (returnType.SpecialType == SpecialType.System_Unit)
                {
                    ILGenerator.Emit(OpCodes.Pop);
                }
                else if (expressionType is not null &&
                         expressionType.IsValueType &&
                         (returnType.SpecialType is SpecialType.System_Object ||
                          returnType is IUnionTypeSymbol))
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));
                }

                ILGenerator.Emit(OpCodes.Ret);
                return;
            }

            EmitStatement(statement);
        }

        if (withReturn)
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

        public HashSet<ILocalSymbol> CapturedLocals { get; } = new(SymbolEqualityComparer.Default);

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            foreach (var d in node.Declarators)
                Locals.Add(d.Local);

            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitLambdaExpression(BoundLambdaExpression node)
        {
            foreach (var symbol in node.CapturedVariables)
            {
                if (symbol is ILocalSymbol local &&
                    SymbolEqualityComparer.Default.Equals(local.ContainingSymbol, _containingSymbol))
                {
                    CapturedLocals.Add(local);
                }
            }

            base.VisitLambdaExpression(node);
        }
    }

    private void EmitIL(IEnumerable<StatementSyntax> statements, bool withReturn = true)
    {
        if (!statements.Any())
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var semanticModel = Compilation.GetSemanticModel(statements.First().SyntaxTree);

        foreach (var statement in statements.ToArray())
        {
            var boundNode = semanticModel.GetBoundNode(statement) as BoundStatement;

            EmitStatement(boundNode);
        }

        if (withReturn)
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
        }
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
}
