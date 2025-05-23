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

    public void Generate()
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = MethodBuilder.GetILGenerator();

        var syntax = MethodSymbol.DeclaringSyntaxReferences.First().GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        foreach (var localDeclStmt in syntax.DescendantNodes().OfType<LocalDeclarationStatementSyntax>())
        {
            foreach (var localDeclarator in localDeclStmt.Declaration.Declarators)
            {
                var localSymbol = GetDeclaredSymbol<ILocalSymbol>(localDeclarator);

                var clrType = localSymbol.Type.GetClrType(_compilation);
                var builder = ILGenerator.DeclareLocal(clrType);
                builder.SetLocalSymInfo(localSymbol.Name);

                scope.AddLocal(localSymbol, builder);
            }
        }

        switch (syntax)
        {
            case CompilationUnitSyntax compilationUnit:
                var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                    .Select(x => x.Statement);
                GenerateIL(statements);
                break;

            case MethodDeclarationSyntax methodDeclaration:
                if (methodDeclaration.Body != null)
                    GenerateIL(methodDeclaration.Body.Statements.ToList());
                else
                    ILGenerator.Emit(OpCodes.Ret);
                break;

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
        }
    }

    private void GenerateIL(IEnumerable<StatementSyntax> statements)
    {
        foreach (var statement in statements)
        {
            GenerateStatement(statement);
        }

        ILGenerator.Emit(OpCodes.Nop);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateStatement(StatementSyntax statement)
    {
        new StatementGenerator(scope, statement).Generate();
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
}