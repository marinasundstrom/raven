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

    public void Emit()
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = (MethodBase as MethodBuilder)?.GetILGenerator()
                     ?? (MethodBase as ConstructorBuilder)?.GetILGenerator()
                     ?? throw new InvalidOperationException();

        var syntax = MethodSymbol.DeclaringSyntaxReferences.First().GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        BoundBlockExpression? boundBody = syntax switch
        {
            MethodDeclarationSyntax m when m.Body != null => semanticModel.GetBoundNode(m.Body) as BoundBlockExpression,
            LocalFunctionStatementSyntax l when l.Body != null => semanticModel.GetBoundNode(l.Body) as BoundBlockExpression,
            BaseConstructorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body) as BoundBlockExpression,
            AccessorDeclarationSyntax a when a.Body != null => semanticModel.GetBoundNode(a.Body) as BoundBlockExpression,
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

                        var clrType = ResolveClrType(localSymbol.Type);
                        var builder = ILGenerator.DeclareLocal(clrType);
                        builder.SetLocalSymInfo(localSymbol.Name);

                        scope.AddLocal(localSymbol, builder);
                    }
                }

                foreach (var localFunctionStmt in compilationUnit.DescendantNodes().OfType<LocalFunctionStatementSyntax>())
                {
                    var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
                    if (methodSymbol is null)
                        continue;
                    if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
                        continue;
                    EmitLocalFunction(localFunctionStmt);
                }

                var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                    .Select(x => x.Statement);
                EmitIL(statements);
                break;

            case LocalFunctionStatementSyntax localFunctionStatement:
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
                    ILGenerator.Emit(OpCodes.Ldarg_0);
                    var baseCtor = ResolveClrType(MethodSymbol.ContainingType!.BaseType!).GetConstructor(Type.EmptyTypes);
                    ILGenerator.Emit(OpCodes.Call, baseCtor);
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
                        var stmt = new BoundExpressionStatement(boundExpr);
                        new StatementGenerator(baseGenerator, stmt).Emit();
                    }

                    ILGenerator.Emit(OpCodes.Ret);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case ClassDeclarationSyntax:
                if (!MethodSymbol.IsStatic)
                {
                    ILGenerator.Emit(OpCodes.Ldarg_0);
                    var baseCtor2 = ResolveClrType(MethodSymbol.ContainingType!.BaseType!).GetConstructor(Type.EmptyTypes);
                    ILGenerator.Emit(OpCodes.Call, baseCtor2);
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);

                ILGenerator.Emit(OpCodes.Ret);
                break;

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
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

            var statement = new BoundExpressionStatement(assignment);
            new StatementGenerator(baseGenerator, statement).Emit();
        }
    }

    private void EmitLocalFunction(LocalFunctionStatementSyntax localFunctionStmt)
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

    private void DeclareLocals(BoundBlockExpression block)
    {
        var collector = new LocalCollector();
        collector.Visit(block);

        foreach (var localSymbol in collector.Locals)
        {
            var clrType = ResolveClrType(localSymbol.Type);
            var builder = ILGenerator.DeclareLocal(clrType);
            builder.SetLocalSymInfo(localSymbol.Name);
            scope.AddLocal(localSymbol, builder);
        }
    }

    private void EmitBoundBlock(BoundBlockExpression block, bool withReturn = true)
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

                if (expressionType is not null &&
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
        public List<ILocalSymbol> Locals { get; } = new();

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            foreach (var d in node.Declarators)
                Locals.Add(d.Local);

            base.VisitLocalDeclarationStatement(node);
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

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(MethodGenerator.TypeGenerator.CodeGen);
    }
}
