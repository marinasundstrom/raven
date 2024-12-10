namespace Raven.CodeAnalysis.Syntax;

public abstract class SyntaxVisitor<TResult>
{
    public virtual TResult VisitNode(SyntaxNode? node)
    {
        return default!;
    }

    public virtual TResult DefaultVisit(SyntaxNode node)
    {
        return default!;
    }

    public virtual TResult VisitIdentifierName(IdentifierNameSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitNamespaceDeclaration(BaseNamespaceDeclarationSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitImportDirective(ImportDirectiveSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitParameter(ParameterSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitLiteralExpression(LiteralExpressionSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitBlock(BlockSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitCompilationUnit(CompilationUnitSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitElseClause(ElseClauseSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitEqualsValue(EqualsValueClauseSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitExpressionStatement(ExpressionStatementSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitGlobalStatement(GlobalStatementSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitIfStatement(IfStatementSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitReturnStatement(ReturnStatementSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitTypeAnnotation(TypeAnnotationSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitParameterList(TypeParameterListSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitType(NameSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitVariableDeclaration(VariableDeclarationSyntax node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        return DefaultVisit(node);
    }
}