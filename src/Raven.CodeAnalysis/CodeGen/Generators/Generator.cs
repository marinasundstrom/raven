using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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

    public ILGenerator ILGenerator => MethodBodyGenerator.ILGenerator;

    public virtual void Emit()
    {

    }

    public virtual void AddLocal(ILocalSymbol localSymbol, LocalBuilder builder)
    {
        Parent?.AddLocal(localSymbol, builder);
    }

    public virtual LocalBuilder? GetLocal(ILocalSymbol localSymbol)
    {
        return Parent?.GetLocal(localSymbol);
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