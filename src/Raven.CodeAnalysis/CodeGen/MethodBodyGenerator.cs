using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodBodyGenerator
{
    private TypeBuilder _typeBuilder;
    private MethodBuilder _methodBuilder;
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
    public MethodBuilder MethodBuilder => _methodBuilder ??= MethodGenerator.MethodBuilder;

    private BaseGenerator baseGenerator;
    private Scope scope;

    public ILGenerator ILGenerator { get; private set; }

    public void Emit()
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = MethodBuilder.GetILGenerator();

        var syntax = MethodSymbol.DeclaringSyntaxReferences.First().GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        foreach (var localDeclStmt in syntax.DescendantNodes()
            //.OfType<GlobalStatementSyntax>()
            //.Select(x => x.Statement)
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

        switch (syntax)
        {
            case CompilationUnitSyntax compilationUnit:
                foreach (var localFunctionStmt in compilationUnit.DescendantNodes().OfType<LocalFunctionStatementSyntax>())
                {
                    EmitLocalFunction(localFunctionStmt);
                }

                var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                    .Select(x => x.Statement);
                EmitIL(statements);
                break;

            case LocalFunctionStatementSyntax localFunctionStatement:
                if (localFunctionStatement.Body != null)
                    EmitIL(localFunctionStatement.Body.Statements.ToList());
                else
                    ILGenerator.Emit(OpCodes.Ret);
                break;

            case MethodDeclarationSyntax methodDeclaration:
                if (methodDeclaration.Body != null)
                    EmitIL(methodDeclaration.Body.Statements.ToList());
                else
                    ILGenerator.Emit(OpCodes.Ret);
                break;

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
        }
    }

    private void EmitLocalFunction(LocalFunctionStatementSyntax localFunctionStmt)
    {
        var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
        if (methodSymbol is null)
            return;

        var methodGenerator = new MethodGenerator(MethodGenerator.TypeGenerator, methodSymbol);
        MethodGenerator.TypeGenerator.Add(methodSymbol, methodGenerator);
        methodGenerator.DefineMethodBuilder();
        methodGenerator.EmitBody();
    }

    private void EmitIL(IEnumerable<StatementSyntax> statements)
    {
        if (!statements.Any())
            return;

        var semanticModel = Compilation.GetSemanticModel(statements.First().SyntaxTree);

        foreach (var statement in statements.ToArray())
        {
            var boundNode = semanticModel.GetBoundNode(statement) as BoundStatement;

            EmitStatement(boundNode);
        }

        ILGenerator.Emit(OpCodes.Nop);
        ILGenerator.Emit(OpCodes.Ret);
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