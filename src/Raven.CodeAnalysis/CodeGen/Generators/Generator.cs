using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal abstract class Generator
{
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

    public virtual void Generate()
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

    protected TypeInfo GetTypeInfo(ExpressionSyntax expression)
    {
        return Compilation
                        .GetSemanticModel(expression.SyntaxTree)
                        .GetTypeInfo(expression);
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is SourceNamedTypeSymbol sourceType)
        {
            // This is a user-defined type, still being built
            return MethodGenerator.TypeGenerator.CodeGen.GetTypeBuilder(sourceType); // TypeBuilder
        }
        else
        {
            return typeSymbol.GetClrType(Compilation); // Already resolved System.Type
        }

        throw new InvalidOperationException($"Unsupported type symbol: {typeSymbol}");
    }

    public MemberInfo? GetMemberBuilder(SourceSymbol sourceSymbol) => MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(sourceSymbol);
}
