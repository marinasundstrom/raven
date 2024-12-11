
namespace Raven.CodeAnalysis.Syntax;

public abstract class SyntaxVisitor
{
    public virtual void Visit(SyntaxNode node)
    {
        DefaultVisit(node);
    }

    public virtual void DefaultVisit(SyntaxNode node)
    {

    }
    
    public virtual void VisitIdentifierName(IdentifierNameSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitNamespaceDeclaration(BaseNamespaceDeclarationSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitImportDirective(ImportDirectiveSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitParameter(ParameterSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitLiteralExpression(LiteralExpressionSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitBlock(BlockSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitCompilationUnit(CompilationUnitSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitElseClause(ElseClauseSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitEqualsValue(EqualsValueClauseSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitExpressionStatement(ExpressionStatementSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitGlobalStatement(GlobalStatementSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitIfStatement(IfStatementSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitReturnStatement(ReturnStatementSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitTypeAnnotation(TypeAnnotationSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitParameterList(TypeParameterListSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitType(NameSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitVariableDeclaration(VariableDeclarationSyntax node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        DefaultVisit(node);
    }
}
