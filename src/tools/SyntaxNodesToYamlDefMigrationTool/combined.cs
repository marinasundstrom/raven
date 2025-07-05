
public partial class AssignmentExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial ExpressionSyntax LeftHandSide { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial ExpressionSyntax RightHandSide { get; }

    internal AssignmentExpressionSyntax(
        InternalSyntax.AssignmentExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public AssignmentExpressionSyntax(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.AssignmentExpressionSyntax(kind, (InternalSyntax.ExpressionSyntax)leftHandSide.Green, operatorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static AssignmentExpressionSyntax AssignmentExpression(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new AssignmentExpressionSyntax(kind, leftHandSide, operatorToken, rightHandSize);
}


public partial class AccessorListSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList<AccessorDeclarationSyntax> Accessors { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    internal AccessorListSyntax(
        InternalSyntax.AccessorListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public AccessorListSyntax(SyntaxToken openBraceToken, SyntaxList<AccessorDeclarationSyntax> accessors, SyntaxToken closeBraceToken)
          : this(
                new InternalSyntax.AccessorListSyntax(openBraceToken.Green, accessors.Green, closeBraceToken.Green), null)
    {

    }

    public AccessorListSyntax(SyntaxList<AccessorDeclarationSyntax> accessors)
        : this(SyntaxFactory.OpenBraceToken, accessors, SyntaxFactory.CloseBraceToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static AccessorListSyntax AccessorList()
    => new AccessorListSyntax(SyntaxList<AccessorDeclarationSyntax>.Empty);

    public static AccessorListSyntax AccessorList(SyntaxList<AccessorDeclarationSyntax> accessors)
        => new AccessorListSyntax(accessors);

    public static AccessorListSyntax AccessorList(SyntaxToken openBraceToken, SyntaxList<AccessorDeclarationSyntax> accessors, SyntaxToken closeBraceToken)
        => new AccessorListSyntax(openBraceToken, accessors, closeBraceToken);
}


public partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken NamespaceKeyword { get; }

    public override partial NameSyntax Name { get; }

    public partial SyntaxToken TerminatorToken { get; }

    public override partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    internal FileScopedNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxTokenList modifiers, NameSyntax name, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(modifiers.Green, SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, SyntaxFactory.SemicolonToken.Green, imports.Green, members.Green), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken)
    : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(modifiers.Green, namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, terminatorToken.Green, InternalSyntax.SyntaxList.Empty, InternalSyntax.SyntaxList.Empty), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(modifiers.Green, namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, terminatorToken.Green, imports.Green, members.Green), (SyntaxNode)null)
    {
    }
}

public static partial class SyntaxFactory
{
    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken)
    => new FileScopedNamespaceDeclarationSyntax(SyntaxTokenList.Empty, namespaceKeyword, name, terminatorToken);

    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
    => new FileScopedNamespaceDeclarationSyntax(SyntaxTokenList.Empty, namespaceKeyword, name, terminatorToken, importDirectives, members);

    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(NameSyntax name, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
        => new FileScopedNamespaceDeclarationSyntax(SyntaxTokenList.Empty, name, importDirectives, members);
}


public partial class EmptyStatementSyntax : StatementSyntax
{
    public partial SyntaxToken TerminatorToken { get; }

    internal EmptyStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public EmptyStatementSyntax(SyntaxToken terminatorToken)
      : this(
            new InternalSyntax.EmptyStatementSyntax(terminatorToken.Green))
    {

    }

    public EmptyStatementSyntax()
      : this(SyntaxFactory.SemicolonToken)
    {

    }
}

public partial class EqualsValueClauseSyntax : SyntaxNode
{
    public partial SyntaxToken EqualsToken { get; }

    public partial ExpressionSyntax Value { get; }

    internal EqualsValueClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public EqualsValueClauseSyntax(SyntaxToken equalsToken, ExpressionSyntax value)
      : this(
            new InternalSyntax.EqualsValueClauseSyntax(equalsToken.Green, (InternalSyntax.ExpressionSyntax)value?.Green))
    {

    }

    public EqualsValueClauseSyntax(ExpressionSyntax value)
      : this(SyntaxFactory.EqualsToken, value)
    {

    }
}

public partial class ParameterListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ParameterSyntax> Parameters { get; }
    public partial SyntaxToken CloseParenToken { get; }

    internal ParameterListSyntax(
        InternalSyntax.ParameterListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ParameterListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ParameterSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.ParameterListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public ParameterListSyntax(SeparatedSyntaxList<ParameterSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }
}

public abstract class ExpressionSyntax : ExpressionOrPatternSyntax
{
    internal ExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public class Missing : ExpressionSyntax
    {
        internal Missing()
            : base(new InternalSyntax.ExpressionSyntax.Missing([]), null, 0)
        {
        }

        internal Missing(
            InternalSyntax.ExpressionSyntax.Missing greenNode,
            SyntaxNode parent = null,
            int position = 0)
            : base(greenNode, parent, position)
        {
        }

        public override void Accept(SyntaxVisitor visitor)
        {
            visitor.DefaultVisit(this);
        }

        public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
        {
            return visitor.DefaultVisit(this);
        }
    }
}


public abstract partial class VariableDesignationSyntax : SyntaxNode
{

    internal VariableDesignationSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}


public partial class SingleVariableDesignationSyntax : VariableDesignationSyntax
{
    public partial SyntaxToken Identifier { get; }

    internal SingleVariableDesignationSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public SingleVariableDesignationSyntax(SyntaxToken identifier)
        : this(new InternalSyntax.SingleVariableDesignationSyntax(identifier.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class CollectionElementSyntax : SyntaxNode
{
    public partial ExpressionSyntax Expression { get; }

    internal CollectionElementSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public CollectionElementSyntax(
        ExpressionSyntax expression)
        : this(new InternalSyntax.CollectionElementSyntax((InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static CollectionElementSyntax CollectionElement(ExpressionSyntax expression)
        => new CollectionElementSyntax(expression);
}


public partial class ArgumentSyntax : SyntaxNode
{
    public partial NameColonSyntax? NameColon { get; }

    public partial ExpressionSyntax Expression { get; }

    internal ArgumentSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ArgumentSyntax(
        NameColonSyntax? nameColon,
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArgumentSyntax((InternalSyntax.NameColonSyntax?)nameColon?.Green, (InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }

    public ArgumentSyntax(
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArgumentSyntax((InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken NamespaceKeyword { get; }

    public override partial NameSyntax Name { get; }

    public partial SyntaxToken OpenBraceToken { get; }

    public override partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken CloseBraceToken { get; }

    public partial SyntaxToken? TerminatorToken { get; }

    internal NamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name,
        SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(modifiers.Green, namespaceKeyword.Green, (InternalSyntax.NameSyntax)name.Green, openBraceToken.Green, imports.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green), (SyntaxNode)null)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxTokenList modifiers, NameSyntax name,
        SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(modifiers.Green, SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.NameSyntax)name.Green, SyntaxFactory.OpenBraceToken.Green, imports.Green, members.Green, SyntaxFactory.CloseBraceToken.Green, null), (SyntaxNode)null)
    {
    }
}

public partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }

    public partial SyntaxToken OperatorToken { get; }

    public partial ExpressionSyntax Expression { get; }

    internal UnaryExpressionSyntax(
        InternalSyntax.UnaryExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public UnaryExpressionSyntax(SyntaxKind kind, SyntaxToken operatorToken, ExpressionSyntax expression)
          : this(
                new InternalSyntax.UnaryExpressionSyntax(kind, operatorToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green), null)
    {

    }
}


public partial class ArrowTypeClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ArrowToken { get; }
    public partial TypeSyntax Type { get; }

    internal ArrowTypeClauseSyntax(
        InternalSyntax.ArrowTypeClauseSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ArrowTypeClauseSyntax(SyntaxToken arrowToken, TypeSyntax type)
          : this(
                new InternalSyntax.ArrowTypeClauseSyntax(arrowToken.Green, (InternalSyntax.TypeSyntax)type.Green), null)
    {

    }

    public ArrowTypeClauseSyntax(TypeSyntax type)
        : this(SyntaxFactory.ArrowToken, type)
    {

    }
}

public partial class SelfExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken SelfKeyword { get; }

    internal SelfExpressionSyntax(
        InternalSyntax.SelfExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public SelfExpressionSyntax(SyntaxToken selfKeyword)
          : this(
                new InternalSyntax.SelfExpressionSyntax(selfKeyword.Green), null)
    {

    }
}

public partial class ClassDeclarationSyntax : TypeDeclarationSyntax
{
    public override int Arity { get; }

    public override partial SyntaxTokenList Modifiers { get; }

    public override partial SyntaxToken Keyword { get; }

    public override partial SyntaxToken Identifier { get; }

    public override partial ParameterListSyntax? ParameterList { get; }

    public override partial SyntaxToken OpenBraceToken { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public override partial SyntaxToken CloseBraceToken { get; }

    public override partial SyntaxToken? TerminatorToken { get; }

    internal ClassDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ClassDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken keyword, SyntaxToken identifier, ParameterListSyntax parameterList, SyntaxToken openBraceToken, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.ClassDeclarationSyntax(modifiers.Green, keyword.Green, identifier.Green, (InternalSyntax.SyntaxList)parameterList.Green, openBraceToken.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green, null))
    {
    }
}

public abstract class ExpressionStatementSyntax : StatementSyntax
{
    public virtual ExpressionSyntax Expression { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }
}

public partial class ExpressionStatementSyntax : ExpressionStatementSyntax
{
    public override partial ExpressionSyntax Expression { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression)
    : this(expression, SyntaxFactory.SemicolonToken)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression, SyntaxToken terminatorToken)
        : this(new InternalSyntax.ExpressionStatementSyntax((InternalSyntax.ExpressionSyntax)expression.Green))
    {
    }
}


public partial class ExpressionStatementSyntax : ExpressionStatementSyntax
{
    public override partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression)
    : this(expression, SyntaxFactory.SemicolonToken)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression, SyntaxToken terminatorToken)
        : this(new InternalSyntax.ExpressionStatementSyntax((InternalSyntax.ExpressionSyntax)expression.Green, terminatorToken.Green))
    {
    }
}


public partial class LocalDeclarationStatementSyntax : StatementSyntax
{
    public partial VariableDeclarationSyntax Declaration { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal LocalDeclarationStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public LocalDeclarationStatementSyntax(VariableDeclarationSyntax declaration, SyntaxToken terminatorToken)
      : this(
            new InternalSyntax.LocalDeclarationStatementSyntax((InternalSyntax.VariableDeclarationSyntax)declaration.Green, terminatorToken.Green))
    {

    }
}


public abstract partial class BaseTypeDeclarationSyntax : MemberDeclarationSyntax
{
    internal BaseTypeDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxToken Identifier { get; }

    public abstract SyntaxToken OpenBraceToken { get; }

    public abstract SyntaxToken CloseBraceToken { get; }

    public abstract SyntaxToken? TerminatorToken { get; }
}


public partial class TupleExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ArgumentSyntax> Arguments { get; }
    public partial SyntaxToken CloseParenToken { get; }

    internal TupleExpressionSyntax(
        InternalSyntax.TupleExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TupleExpressionSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ArgumentSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.TupleExpressionSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public TupleExpressionSyntax(SeparatedSyntaxList<ArgumentSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }
}


public partial class PredefinedTypeSyntax : TypeSyntax
{
    public partial SyntaxToken Keyword { get; }

    internal PredefinedTypeSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent,
        int position)
        : base(greenNode, parent, position)
    {
    }

    public PredefinedTypeSyntax(SyntaxToken keyword)
      : this(
            new InternalSyntax.PredefinedTypeSyntax(keyword.Green), null, 0)
    {

    }
}

public partial class CollectionExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenBracketToken { get; }
    public partial SeparatedSyntaxList<CollectionElementSyntax> Elements { get; }
    public partial SyntaxToken CloseBracketToken { get; }

    internal CollectionExpressionSyntax(

        InternalSyntax.CollectionExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public CollectionExpressionSyntax(SyntaxToken openBracketToken, SeparatedSyntaxList<CollectionElementSyntax> elements, SyntaxToken closeBraceToken)
          : this(
                new InternalSyntax.CollectionExpressionSyntax(openBracketToken.Green, elements.Green, closeBraceToken.Green), null)
    {

    }

    public CollectionExpressionSyntax(SeparatedSyntaxList<CollectionElementSyntax> elements)
        : this(SyntaxFactory.OpenBraceToken, elements, SyntaxFactory.CloseBraceToken)
    {

    }
}


public partial class ConstructorDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken InitKeyword { get; }
    public partial SyntaxToken? Identifier { get; }
    public override partial ParameterListSyntax ParameterList { get; }
    public override partial BlockSyntax? Body { get; }
    public override partial ArrowExpressionClauseSyntax? ExpressionBody { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal ConstructorDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ConstructorDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, BlockSyntax body, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.ConstructorDeclarationSyntax(modifiers.Green, initKeyword.Green, identifier?.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.BlockSyntax)body.Green, terminatorToken?.Green))
    {
    }

    public ConstructorDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.ConstructorDeclarationSyntax(modifiers.Green, initKeyword.Green, identifier?.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody.Green, terminatorToken?.Green, null))
    {
    }

    public ConstructorDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken initKeyword, SyntaxToken? identifier, ParameterListSyntax parameters, BlockSyntax? body, ArrowExpressionClauseSyntax? expressionBody, SyntaxToken? terminatorToken)
: this(new InternalSyntax.ConstructorDeclarationSyntax(modifiers.Green, initKeyword.Green, identifier?.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody?.Green, terminatorToken?.Green, null))
    {
    }
}


public partial class ReturnStatementSyntax : StatementSyntax
{
    public partial SyntaxToken ReturnKeyword { get; }

    public partial ExpressionSyntax? Expression { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal ReturnStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ReturnStatementSyntax(SyntaxToken returnKeyword, ExpressionSyntax? expression, SyntaxToken terminatorToken)
      : this(
            new InternalSyntax.ReturnStatementSyntax(returnKeyword.Green, (InternalSyntax.ExpressionSyntax)expression?.Green, terminatorToken.Green))
    {

    }

    public ReturnStatementSyntax(ExpressionSyntax? expression)
      : this(SyntaxFactory.ReturnKeyword, expression, SyntaxFactory.SemicolonToken)
    {

    }
}

public partial class CompilationUnitSyntax : SyntaxNode
{
    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken EndOfFileToken { get; }

    internal CompilationUnitSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public CompilationUnitSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }

    public CompilationUnitSyntax()
    : this(new Syntax.InternalSyntax.CompilationUnitSyntax(SyntaxList<ImportDirectiveSyntax>.Empty.Green, SyntaxList<MemberDeclarationSyntax>.Empty.Green, SyntaxFactory.EndOfFile.Green), (SyntaxNode)null)
    {
    }

    public CompilationUnitSyntax(SyntaxTree syntaxTree, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken endOfFileToken)
    : this(new Syntax.InternalSyntax.CompilationUnitSyntax(imports.Green, members.Green, endOfFileToken.Green), syntaxTree)
    {
    }

    public CompilationUnitSyntax(SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken? endOfFileToken = null)
        : this(new Syntax.InternalSyntax.CompilationUnitSyntax(imports.Green, members.Green, endOfFileToken?.Green), (SyntaxTree)null)
    {
    }

    internal CompilationUnitSyntax WithSyntaxTree(SyntaxTree syntaxTree)
    {
        return new CompilationUnitSyntax(syntaxTree, Imports, Members, EndOfFileToken);
    }
}

public partial class LiteralExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial SyntaxToken Token { get; }

    internal LiteralExpressionSyntax(GreenNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public LiteralExpressionSyntax(SyntaxKind kind, SyntaxToken token)
         : this(new InternalSyntax.LiteralExpressionSyntax(kind, token.Green))
    {
    }
}


public abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    internal BaseNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public virtual NameSyntax Name { get; }

    public virtual SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public virtual SyntaxList<MemberDeclarationSyntax> Members { get; }
}


public abstract partial class TypeSyntax : ExpressionSyntax
{
    internal TypeSyntax(GreenNode greenNode, SyntaxNode parent, int position) : base(greenNode, parent, position)
    {
    }
}

public abstract class NameSyntax : TypeSyntax
{
    public virtual int Arity => 0;

    internal NameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public abstract class SimpleNameSyntax : NameSyntax
{
    public virtual SyntaxToken Identifier { get; }

    internal SimpleNameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public partial class IdentifierNameSyntax : SimpleNameSyntax
{
    public override partial SyntaxToken Identifier { get; }

    internal IdentifierNameSyntax(
        InternalSyntax.IdentifierNameSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public IdentifierNameSyntax(SyntaxToken identifier)
          : this(
                new InternalSyntax.IdentifierNameSyntax(identifier.Green), null)
    {

    }

    protected override string GetDebuggerDisplay()
        => $"{Kind}: {Identifier.Text}";
}

public partial class GenericNameSyntax : SimpleNameSyntax
{
    public override partial SyntaxToken Identifier { get; }

    public partial TypeArgumentListSyntax TypeArgumentList { get; }

    public override int Arity => TypeArgumentList.Count;

    internal GenericNameSyntax(
        InternalSyntax.GenericNameSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public GenericNameSyntax(SyntaxToken identifier, TypeArgumentListSyntax typeArgumentList)
          : this(
                new InternalSyntax.GenericNameSyntax(identifier.Green, (InternalSyntax.TypeArgumentListSyntax)typeArgumentList.Green), null)
    {

    }
}

public partial class QualifiedNameSyntax : NameSyntax
{
    public partial NameSyntax Left { get; }

    public partial SyntaxToken DotToken { get; }

    public partial SimpleNameSyntax Right { get; }

    internal QualifiedNameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public QualifiedNameSyntax(NameSyntax left, SyntaxToken dotToken, SimpleNameSyntax right)
        : base(new InternalSyntax.QualifiedNameSyntax((InternalSyntax.NameSyntax)left.Green, dotToken.Green, (InternalSyntax.SimpleNameSyntax)right.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class AliasQualifiedNameSyntax : NameSyntax
{
    public partial IdentifierNameSyntax Alias { get; }

    public partial SyntaxToken ColonColonToken { get; }

    public partial SimpleNameSyntax Name { get; }


    internal AliasQualifiedNameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public AliasQualifiedNameSyntax(IdentifierNameSyntax alias, SyntaxToken colonColonToken, SimpleNameSyntax name)
        : base(new InternalSyntax.AliasQualifiedNameSyntax((InternalSyntax.IdentifierNameSyntax)alias.Green, colonColonToken.Green, (InternalSyntax.SimpleNameSyntax)name.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class NullableTypeSyntax : TypeSyntax
{
    public partial TypeSyntax ElementType { get; }

    public partial SyntaxToken QuestionToken { get; }

    internal NullableTypeSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public NullableTypeSyntax(TypeSyntax elementType, SyntaxToken questionToken)
        : this(new InternalSyntax.NullableTypeSyntax((InternalSyntax.TypeSyntax)elementType.Green, questionToken.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class UnionTypeSyntax : TypeSyntax
{
    public partial SeparatedSyntaxList<TypeSyntax> Types { get; }

    internal UnionTypeSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public UnionTypeSyntax(SeparatedSyntaxList<TypeSyntax> types)
        : this(new InternalSyntax.UnionTypeSyntax((InternalSyntax.SyntaxList)types.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class BlockSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList<StatementSyntax> Statements { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    internal BlockSyntax(
        InternalSyntax.BlockSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BlockSyntax(SyntaxToken openBraceToken, SyntaxList<StatementSyntax> statements, SyntaxToken closeBraceToken)
          : this(
                new InternalSyntax.BlockSyntax(openBraceToken.Green, statements.Green, closeBraceToken.Green), null)
    {

    }

    public BlockSyntax(SyntaxList<StatementSyntax> statements)
        : this(SyntaxFactory.OpenBraceToken, statements, SyntaxFactory.CloseBraceToken)
    {

    }
}

public abstract class MemberDeclarationSyntax : SyntaxNode
{
    internal MemberDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxTokenList Modifiers { get; }

    public static implicit operator MemberDeclarationSyntax(StatementSyntax statement) => SyntaxFactory.GlobalStatement(statement);
}


public partial class ParenthesizedExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenParenToken { get; }

    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken CloseParenToken { get; }

    internal ParenthesizedExpressionSyntax(
        InternalSyntax.ParenthesizedExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ParenthesizedExpressionSyntax(SyntaxToken openParenToken, ExpressionSyntax expression, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.ParenthesizedExpressionSyntax(openParenToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green, closeParenToken.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParenthesizedExpressionSyntax ParenthesizedExpression(SyntaxToken openParenToken, ExpressionSyntax expression, SyntaxToken closeParenToken)
        => new ParenthesizedExpressionSyntax(openParenToken, expression, closeParenToken);
}


public abstract partial class TypeDeclarationSyntax : BaseTypeDeclarationSyntax
{
    internal TypeDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract int Arity { get; }

    //public abstract SyntaxList<TypeParameterConstraintClauseSyntax> ConstraintClauses { get; }

    public abstract SyntaxToken Keyword { get; }

    public abstract SyntaxList<MemberDeclarationSyntax> Members { get; }

    public abstract ParameterListSyntax? ParameterList { get; }

    //public abstract TypeParameterListSyntax TypeParameterList { get; }

}



public sealed partial class IfExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken IfKeyword { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial ExpressionSyntax Expression { get; }
    public partial ElseClauseSyntax? ElseClause { get; }

    internal IfExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public IfExpressionSyntax(SyntaxToken ifKeyword, ExpressionSyntax condition, ExpressionSyntax expression)
          : this(
                new InternalSyntax.IfExpressionSyntax(ifKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.ExpressionSyntax)expression.Green, null))
    {

    }

    public IfExpressionSyntax(SyntaxToken ifKeyword, ExpressionSyntax condition, ExpressionSyntax expression, ElseClauseSyntax? elseClause)
      : this(
            new InternalSyntax.IfExpressionSyntax(ifKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.ExpressionSyntax)expression.Green, (InternalSyntax.ElseClauseSyntax?)elseClause?.Green))
    {

    }

    public IfExpressionSyntax(ExpressionSyntax condition, ExpressionSyntax expression)
        : this(SyntaxFactory.IfKeyword, condition, expression)
    {

    }

    public IfExpressionSyntax(ExpressionSyntax condition, ExpressionSyntax expression, ElseClauseSyntax elseClause)
        : this(SyntaxFactory.IfKeyword, condition, expression, elseClause)
    {

    }
}

public static partial class SyntaxFactory
{
    public static IfExpressionSyntax IfStatement(ExpressionSyntax condition, ExpressionSyntax expression)
        => new IfExpressionSyntax(condition, expression);

    public static IfExpressionSyntax IfStatement(SyntaxToken ifKeyword, ExpressionSyntax condition, ExpressionSyntax expression)
        => new IfExpressionSyntax(ifKeyword, condition, expression);
}


public partial class TypeArgumentListSyntax : SyntaxNode
{
    public partial SyntaxToken GreaterThanToken { get; }
    public partial SeparatedSyntaxList<TypeArgumentSyntax> Arguments { get; }
    public partial SyntaxToken LessThanToken { get; }

    public int Count => Arguments.Count;

    internal TypeArgumentListSyntax(
        InternalSyntax.TypeArgumentListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TypeArgumentListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<TypeArgumentSyntax> parameters, SyntaxToken closeParenToken)
        : this(
            new InternalSyntax.TypeArgumentListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public TypeArgumentListSyntax(SeparatedSyntaxList<TypeArgumentSyntax> parameters)
        : this(SyntaxFactory.LessThanToken, parameters, SyntaxFactory.GreaterThanToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static TypeArgumentListSyntax TypeArgumentList(SyntaxToken lessThanToken, SeparatedSyntaxList<TypeArgumentSyntax> arguments, SyntaxToken greaterThanToken)
        => new TypeArgumentListSyntax(lessThanToken, arguments, greaterThanToken);

    public static TypeArgumentListSyntax TypeArgumentList(SeparatedSyntaxList<TypeArgumentSyntax> arguments)
        => TypeArgumentList(SyntaxFactory.LessThanToken, arguments, SyntaxFactory.GreaterThanToken);
}


public partial class BracketedArgumentListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenBracketToken { get; }
    public partial SeparatedSyntaxList<ArgumentSyntax> Arguments { get; }
    public partial SyntaxToken CloseBracketToken { get; }

    internal BracketedArgumentListSyntax(
        InternalSyntax.BracketedArgumentListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BracketedArgumentListSyntax(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> parameters, SyntaxToken closeBracketToken)
          : this(
                new InternalSyntax.BracketedArgumentListSyntax(openBracketToken.Green, parameters.Green, closeBracketToken.Green), null)
    {

    }

    public BracketedArgumentListSyntax(SeparatedSyntaxList<ArgumentSyntax> parameters)
        : this(SyntaxFactory.OpenBracketToken, parameters, SyntaxFactory.CloseBracketToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static BracketedArgumentListSyntax BracketedArgumentList(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> arguments, SyntaxToken closeBracketToken)
        => new BracketedArgumentListSyntax(openBracketToken, arguments, closeBracketToken);

    public static BracketedArgumentListSyntax BracketedArgumentList(SeparatedSyntaxList<ArgumentSyntax> arguments)
        => BracketedArgumentList(SyntaxFactory.OpenBracketToken, arguments, SyntaxFactory.CloseBracketToken);
}


public partial class MemberAccessExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }

    public partial ExpressionSyntax? Expression { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial SimpleNameSyntax Name { get; }

    public bool IsTarget => Expression is null;

    internal MemberAccessExpressionSyntax(
        InternalSyntax.MemberAccessExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MemberAccessExpressionSyntax(SyntaxKind kind, ExpressionSyntax? expression, SyntaxToken operatorToken, SimpleNameSyntax name)
        : this(
            new InternalSyntax.MemberAccessExpressionSyntax(
                kind,
                (InternalSyntax.ExpressionSyntax?)expression?.Green,
                operatorToken.Green,
                (InternalSyntax.SimpleNameSyntax)name.Green),
            null)
    {
    }

    public MemberAccessExpressionSyntax(SyntaxKind kind, ExpressionSyntax? expression, SimpleNameSyntax name)
        : this(kind, expression, SyntaxFactory.DotToken, name)
    {
    }
}

public static partial class SyntaxFactory
{
    public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, ExpressionSyntax? expression, SimpleNameSyntax name)
        => new MemberAccessExpressionSyntax(kind, expression, name);

    public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, ExpressionSyntax? expression, SyntaxToken operatorToken, SimpleNameSyntax name)
        => new MemberAccessExpressionSyntax(kind, expression, operatorToken, name);
}


public partial class NameColonSyntax : SyntaxNode
{
    public partial IdentifierNameSyntax Name { get; }

    //public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken ColonTon { get; }

    internal NameColonSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public NameColonSyntax(
        IdentifierNameSyntax name,
        //ExpressionSyntax expression,
        SyntaxToken colonToken)
        : this(new InternalSyntax.NameColonSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, /* (InternalSyntax.ExpressionSyntax)expression.Green)*/ colonToken.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class SimpleLambdaExpressionSyntax : LambdaExpressionSyntax
{
    public override partial SyntaxToken FuncKeyword { get; }
    public partial ParameterSyntax Parameter { get; }
    public partial ArrowTypeClauseSyntax ReturnType { get; }
    public override partial SyntaxToken ArrowToken { get; }
    public override partial ExpressionSyntax ExpressionBody { get; }

    internal SimpleLambdaExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public SimpleLambdaExpressionSyntax(
        SyntaxToken funcKeyword,
        ParameterSyntax parameter,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        : this(new InternalSyntax.SimpleLambdaExpressionSyntax(funcKeyword.Green, (InternalSyntax.ParameterSyntax)parameter.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, arrowToken.Green, (InternalSyntax.ExpressionSyntax?)expressionBody.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(
        SyntaxToken funcKeyword,
        ParameterSyntax parameter,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        => new SimpleLambdaExpressionSyntax(funcKeyword, parameter, returnType, arrowToken, expressionBody);
}

public abstract class StatementSyntax : SyntaxNode
{
    internal StatementSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}


public partial class ImportDirectiveSyntax : SyntaxNode
{
    public partial SyntaxToken ImportKeyword { get; }

    public partial TypeSyntax NamespaceOrType { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal ImportDirectiveSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ImportDirectiveSyntax(SyntaxToken importKeyword, TypeSyntax namespaceOrType, SyntaxToken terminatorToken)
        : base(new InternalSyntax.ImportDirectiveSyntax(importKeyword.Green, (InternalSyntax.NameSyntax)namespaceOrType.Green, terminatorToken.Green), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(NameSyntax @namespace)
        => new ImportDirectiveSyntax(ImportKeyword, @namespace, SemicolonToken);

    public static ImportDirectiveSyntax ImportDirective(SyntaxToken importKeyword, TypeSyntax namespaceOrType, SyntaxToken terminatorToken)
        => new ImportDirectiveSyntax(importKeyword, namespaceOrType, terminatorToken);
}


public partial class ElseClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ElseKeyword { get; }

    public partial ExpressionSyntax Expression { get; }

    internal ElseClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ElseClauseSyntax(SyntaxToken elseKeyword, ExpressionSyntax expression)
      : this(
            new InternalSyntax.ElseClauseSyntax(elseKeyword.Green, (InternalSyntax.ExpressionSyntax)expression.Green))
    {

    }

    public ElseClauseSyntax(ExpressionSyntax expression)
      : this(SyntaxFactory.ElseKeyword, expression)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ElseClauseSyntax ElseClause(ExpressionSyntax expression)
        => new ElseClauseSyntax(expression);
}


public partial class FieldDeclarationSyntax : MemberDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial VariableDeclarationSyntax Declaration { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal FieldDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public FieldDeclarationSyntax(SyntaxTokenList modifiers, VariableDeclarationSyntax declaration, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.FieldDeclarationSyntax(modifiers.Green, (InternalSyntax.VariableDeclarationSyntax)declaration.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static FieldDeclarationSyntax FieldDeclaration(SyntaxTokenList modifiers, VariableDeclarationSyntax declaration, SyntaxToken? terminatorToken)
        => new FieldDeclarationSyntax(modifiers, declaration, terminatorToken);
}


public abstract partial class BasePropertyDeclarationSyntax : MemberDeclarationSyntax
{
    internal BasePropertyDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxToken Identifier { get; }
    public abstract TypeAnnotationClauseSyntax Type { get; }
    public abstract AccessorListSyntax? AccessorList { get; }
}



public partial class VariableDeclarationSyntax : SyntaxNode
{
    public partial SyntaxToken LetOrVarKeyword { get; }
    public partial SeparatedSyntaxList<VariableDeclaratorSyntax> Declarators { get; }

    internal VariableDeclarationSyntax(
        InternalSyntax.VariableDeclarationSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public VariableDeclarationSyntax(SyntaxToken letOrVarKeyword, SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
          : this(
                new InternalSyntax.VariableDeclarationSyntax(letOrVarKeyword.Green, declarators.Green), null)
    {

    }

    public VariableDeclarationSyntax(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        : this(SyntaxFactory.LetKeyword, declarators)
    {

    }
}

public static partial class SyntaxFactory
{
    public static VariableDeclarationSyntax VariableDeclaration(SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        => VariableDeclaration(LetKeyword, declarators);

    public static VariableDeclarationSyntax VariableDeclaration(SyntaxToken letOrVarKeyword, SeparatedSyntaxList<VariableDeclaratorSyntax> declarators)
        => new VariableDeclarationSyntax(letOrVarKeyword, declarators);
}


public abstract partial class LambdaExpressionSyntax : ExpressionSyntax
{
    internal LambdaExpressionSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxToken FuncKeyword { get; }

    public abstract SyntaxToken ArrowToken { get; }

    public abstract ExpressionSyntax ExpressionBody { get; }
}


public partial class ParameterSyntax : SyntaxNode
{
    public partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken Identifier { get; }

    public partial TypeAnnotationClauseSyntax? TypeAnnotation { get; }

    internal ParameterSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ParameterSyntax(
        SyntaxTokenList modifiers,
        SyntaxToken identifier,
        TypeAnnotationClauseSyntax? typeAnnotation)
        : this(new InternalSyntax.ParameterSyntax(modifiers.Green, identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax?)typeAnnotation?.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(
        SyntaxTokenList modifiers,
        SyntaxToken identifier)
        => new ParameterSyntax(modifiers, identifier, null);

    public static ParameterSyntax Parameter(
        SyntaxToken identifier)
        => new ParameterSyntax(SyntaxTokenList.Empty, identifier, null);

    public static ParameterSyntax Parameter(
        SyntaxTokenList modifiers,
        SyntaxToken identifier,
        TypeAnnotationClauseSyntax? typeAnnotation)
        => new ParameterSyntax(modifiers, identifier, typeAnnotation);

    public static ParameterSyntax Parameter(
        SyntaxToken identifier,
        TypeAnnotationClauseSyntax? typeAnnotation)
        => new ParameterSyntax(SyntaxTokenList.Empty, identifier, typeAnnotation);
}


public partial class EnumMemberDeclarationSyntax : MemberDeclarationSyntax
{
    internal EnumMemberDeclarationSyntax(
        GreenNode greenNode,
        SyntaxNode? parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public EnumMemberDeclarationSyntax(SyntaxToken identifier, EqualsValueClauseSyntax? equalsValueClauseSyntax)
        : this(new InternalSyntax.EnumMemberDeclarationSyntax(identifier.Green, (InternalSyntax.EqualsValueClauseSyntax?)equalsValueClauseSyntax?.Green, null))
    {
    }

    public override SyntaxTokenList Modifiers { get; } = SyntaxTokenList.Empty;

    public partial SyntaxToken Identifier { get; }
    public partial EqualsValueClauseSyntax? EqualsValueClauseSyntax { get; }
}

public static partial class SyntaxFactory
{
    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(SyntaxToken identifier)
        => new EnumMemberDeclarationSyntax(identifier, null);

    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(SyntaxToken identifier, EqualsValueClauseSyntax? equalsValueClauseSyntax)
        => new EnumMemberDeclarationSyntax(identifier, equalsValueClauseSyntax);
}


public partial class ArrowExpressionClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ArrowToken { get; }

    public partial ExpressionSyntax Expression { get; }

    internal ArrowExpressionClauseSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ArrowExpressionClauseSyntax(
        SyntaxToken arrowToken,
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArrowExpressionClauseSyntax(arrowToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArrowExpressionClauseSyntax ArrowExpressionClause(SyntaxToken arrowToken, ExpressionSyntax expression)
    => new ArrowExpressionClauseSyntax(arrowToken, expression);
}


public abstract partial class PatternSyntax : ExpressionOrPatternSyntax
{

    internal PatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public partial class UnaryPatternSyntax : PatternSyntax
{
    public override partial SyntaxKind Kind { get; }

    public partial SyntaxToken OperatorToken { get; }

    public partial PatternSyntax Pattern { get; }

    internal UnaryPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public UnaryPatternSyntax(SyntaxKind kind, SyntaxToken operatorToken, PatternSyntax pattern)
        : this(new InternalSyntax.UnaryPatternSyntax(kind, operatorToken.Green, (InternalSyntax.PatternSyntax)pattern.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class BinaryPatternSyntax : PatternSyntax
{
    public override partial SyntaxKind Kind { get; }

    public partial PatternSyntax Left { get; }

    public partial SyntaxToken OperatorToken { get; }

    public partial PatternSyntax Right { get; }

    internal BinaryPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public BinaryPatternSyntax(SyntaxKind kind, PatternSyntax left, SyntaxToken operatorToken, PatternSyntax right)
        : this(new InternalSyntax.BinaryPatternSyntax(kind, (InternalSyntax.PatternSyntax)left.Green, operatorToken.Green, (InternalSyntax.PatternSyntax)right.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class DeclarationPatternSyntax : PatternSyntax
{
    public partial TypeSyntax Type { get; }

    public partial VariableDesignationSyntax Designation { get; }

    internal DeclarationPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public DeclarationPatternSyntax(TypeSyntax type, VariableDesignationSyntax designation)
        : this(new InternalSyntax.DeclarationPatternSyntax((InternalSyntax.TypeSyntax)type.Green, (InternalSyntax.VariableDesignationSyntax)designation.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static UnaryPatternSyntax UnaryPattern(SyntaxKind kind, SyntaxToken operatorToken, PatternSyntax pattern)
        => new UnaryPatternSyntax(kind, operatorToken, pattern);

    public static BinaryPatternSyntax BinaryPattern(SyntaxKind kind, PatternSyntax left, SyntaxToken operatorToken, PatternSyntax right)
        => new BinaryPatternSyntax(kind, left, operatorToken, right);

    public static DeclarationPatternSyntax DeclarationPattern(TypeSyntax type, VariableDesignationSyntax designation)
        => new DeclarationPatternSyntax(type, designation);
}


public partial class EnumDeclarationSyntax : BaseTypeDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken EnumKeyword { get; }
    public override partial SyntaxToken Identifier { get; }
    public override partial SyntaxToken OpenBraceToken { get; }
    public partial SeparatedSyntaxList<EnumMemberDeclarationSyntax> Members { get; }
    public override partial SyntaxToken CloseBraceToken { get; }
    public override partial SyntaxToken? TerminatorToken { get; }

    internal EnumDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public EnumDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.EnumDeclarationSyntax(modifiers.Green, enumKeyword.Green, identifier.Green, openBraceToken.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static EnumDeclarationSyntax EnumDeclaration(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
        => new EnumDeclarationSyntax(SyntaxTokenList.Empty, enumKeyword, identifier, openBraceToken, members, closeBraceToken, terminatorToken);
}


public partial class IndexerDeclarationSyntax : BasePropertyDeclarationSyntax
{
    internal IndexerDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public IndexerDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, BracketedParameterListSyntax parameterList, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.IndexerDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.BracketedParameterListSyntax)parameterList.Green, (InternalSyntax.TypeAnnotationClauseSyntax)type.Green, (InternalSyntax.AccessorListSyntax)accessorList.Green, (InternalSyntax.EqualsValueClauseSyntax?)initializer?.Green, terminatorToken?.Green), null, 0)
    {

    }

    public override partial SyntaxTokenList Modifiers { get; }

    public override partial SyntaxToken Identifier { get; }

    public partial BracketedParameterListSyntax ParameterList { get; }

    public override partial TypeAnnotationClauseSyntax Type { get; }

    public override partial AccessorListSyntax? AccessorList { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    public partial SyntaxToken? TerminatorToken { get; }
}

public static partial class SyntaxFactory
{
    public static IndexerDeclarationSyntax IndexerDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, BracketedParameterListSyntax parameterList, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        => new(modifiers, identifier, parameterList, type, accessorList, initializer, terminatorToken);
}



public sealed partial class WhileExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken WhileKeyword { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial StatementSyntax Statement { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal WhileExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public WhileExpressionSyntax(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement, SyntaxToken? terminatorToken)
      : this(
            new InternalSyntax.WhileExpressionSyntax(whileKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.StatementSyntax)statement.Green))
    {

    }

    public WhileExpressionSyntax(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement)
          : this(
                new InternalSyntax.WhileExpressionSyntax(whileKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.StatementSyntax)statement.Green, null))
    {

    }

    public WhileExpressionSyntax(ExpressionSyntax condition, StatementSyntax statement)
        : this(SyntaxFactory.WhileKeyword, condition, statement)
    {

    }
}

public static partial class SyntaxFactory
{
    public static WhileExpressionSyntax WhileStatement(ExpressionSyntax condition, StatementSyntax statement)
        => new WhileExpressionSyntax(condition, statement);

    public static WhileExpressionSyntax WhileStatement(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement)
        => new WhileExpressionSyntax(whileKeyword, condition, statement);
}


public partial class ArgumentListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ArgumentSyntax> Arguments { get; }
    public partial SyntaxToken CloseParenToken { get; }

    internal ArgumentListSyntax(
        InternalSyntax.ArgumentListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ArgumentListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ArgumentSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.ArgumentListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public ArgumentListSyntax(SeparatedSyntaxList<ArgumentSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static ArgumentListSyntax ArgumentList(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> arguments, SyntaxToken closeBracketToken)
        => new ArgumentListSyntax(openBracketToken, arguments, closeBracketToken);

    public static ArgumentListSyntax ArgumentList(SeparatedSyntaxList<ArgumentSyntax> arguments)
        => ArgumentList(SyntaxFactory.OpenParenToken, arguments, SyntaxFactory.CloseParenToken);
}


public partial class TypeArgumentSyntax : SyntaxNode
{
    public partial TypeSyntax Type { get; }

    internal TypeArgumentSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public TypeArgumentSyntax(
        TypeSyntax type)
        : this(new InternalSyntax.TypeArgumentSyntax((InternalSyntax.TypeSyntax)type.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArgumentSyntax TypeArgumentSyntax(TypeSyntax type)
        => new ArgumentSyntax(type);
}


public partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public override SyntaxTokenList Modifiers { get; }

    public partial StatementSyntax Statement { get; }

    internal GlobalStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public GlobalStatementSyntax(StatementSyntax statement)
        : this(new InternalSyntax.GlobalStatementSyntax((InternalSyntax.StatementSyntax)statement.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static GlobalStatementSyntax GlobalStatement(StatementSyntax statement)
        => new GlobalStatementSyntax(statement);
}


public partial class PropertyDeclarationSyntax : BasePropertyDeclarationSyntax
{
    internal PropertyDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public PropertyDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.PropertyDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax)type.Green, (InternalSyntax.AccessorListSyntax)accessorList.Green, (InternalSyntax.EqualsValueClauseSyntax?)initializer?.Green, terminatorToken?.Green), null, 0)
    {

    }

    public override partial SyntaxTokenList Modifiers { get; }

    public override partial SyntaxToken Identifier { get; }

    public override partial TypeAnnotationClauseSyntax Type { get; }

    public override partial AccessorListSyntax? AccessorList { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    public partial SyntaxToken? TerminatorToken { get; }
}

public static partial class SyntaxFactory
{
    public static PropertyDeclarationSyntax PropertyDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        => new(modifiers, identifier, type, accessorList, initializer, terminatorToken);
}


public partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial ExpressionSyntax LeftHandSide { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial ExpressionSyntax RightHandSide { get; }

    internal BinaryExpressionSyntax(
        InternalSyntax.BinaryExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BinaryExpressionSyntax(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.BinaryExpressionSyntax(kind, (InternalSyntax.ExpressionSyntax)leftHandSide.Green, operatorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static BinaryExpressionSyntax BinaryExpression(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new BinaryExpressionSyntax(kind, leftHandSide, operatorToken, rightHandSize);
}


public partial class TypeAnnotationClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ColonToken { get; }
    public partial TypeSyntax Type { get; }

    internal TypeAnnotationClauseSyntax(
        InternalSyntax.TypeAnnotationClauseSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TypeAnnotationClauseSyntax(SyntaxToken colonToken, TypeSyntax type)
          : this(
                new InternalSyntax.TypeAnnotationClauseSyntax(colonToken.Green, (InternalSyntax.TypeSyntax)type.Green), null)
    {

    }

    public TypeAnnotationClauseSyntax(TypeSyntax type)
        : this(SyntaxFactory.ColonToken, type)
    {

    }
}

public static partial class SyntaxFactory
{
    public static TypeAnnotationClauseSyntax TypeAnnotation(SyntaxToken colonToken, TypeSyntax type)
        => new TypeAnnotationClauseSyntax(colonToken, type);

    public static TypeAnnotationClauseSyntax TypeAnnotation(TypeSyntax type)
         => new TypeAnnotationClauseSyntax(type);
}


public partial class IsPatternExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken IsKeyword { get; }

    public partial PatternSyntax Pattern { get; }

    internal IsPatternExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public IsPatternExpressionSyntax(ExpressionSyntax expression, SyntaxToken isKeyword, PatternSyntax pattern)
        : this(new InternalSyntax.IsPatternExpressionSyntax((InternalSyntax.ExpressionSyntax)expression.Green, isKeyword.Green, (InternalSyntax.PatternSyntax)pattern.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static IsPatternExpressionSyntax IsPatternExpression(ExpressionSyntax expression, SyntaxToken isKeyword, PatternSyntax pattern)
        => new IsPatternExpressionSyntax(expression, isKeyword, pattern);
}


public partial class ParenthesizedLambdaExpressionSyntax : LambdaExpressionSyntax
{
    public override partial SyntaxToken FuncKeyword { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial ArrowTypeClauseSyntax ReturnType { get; }
    public override partial SyntaxToken ArrowToken { get; }
    public override partial ExpressionSyntax ExpressionBody { get; }

    internal ParenthesizedLambdaExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ParenthesizedLambdaExpressionSyntax(
        SyntaxToken funcKeyword,
        ParameterListSyntax parameterList,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        : this(new InternalSyntax.ParenthesizedLambdaExpressionSyntax(funcKeyword.Green, (InternalSyntax.ParameterListSyntax)parameterList.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, arrowToken.Green, (InternalSyntax.ExpressionSyntax?)expressionBody.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(
        SyntaxToken funcKeyword,
        ParameterListSyntax parameterList,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody)
        => new ParenthesizedLambdaExpressionSyntax(funcKeyword, parameterList, returnType, arrowToken, expressionBody);
}


public partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public partial SyntaxToken FuncKeyword { get; }
    public partial SyntaxToken Identifier { get; }
    public partial ParameterListSyntax ParameterList { get; }
    public partial ArrowTypeClauseSyntax ReturnType { get; }
    public partial BlockSyntax? Body { get; }

    internal LocalFunctionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType)
        : this(new InternalSyntax.LocalFunctionStatementSyntax(funcKeyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, null))
    {
    }

    public LocalFunctionStatementSyntax(SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, BlockSyntax? body)
    : this(new InternalSyntax.LocalFunctionStatementSyntax(funcKeyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static LocalFunctionStatementSyntax LocalFunctionStatement(SyntaxToken funcKeyword, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, BlockSyntax? body)
        => new LocalFunctionStatementSyntax(funcKeyword, identifier, parameters, returnType, body);
}


public partial class ObjectCreationExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken NewKeyword { get; }

    public partial TypeSyntax Type { get; }

    public partial ArgumentListSyntax ArgumentList { get; }

    internal ObjectCreationExpressionSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ObjectCreationExpressionSyntax(SyntaxToken newKeyword, TypeSyntax? type, ArgumentListSyntax argumentList)
      : this(
            new InternalSyntax.ObjectCreationExpressionSyntax(newKeyword.Green, (InternalSyntax.TypeSyntax)type!.Green, (InternalSyntax.ArgumentListSyntax)argumentList.Green))
    {

    }

    public ObjectCreationExpressionSyntax(TypeSyntax? type, ArgumentListSyntax argumentList)
      : this(SyntaxFactory.NewKeyword, type, argumentList)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ObjectCreationExpressionSyntax ObjectCreationExpression(SyntaxToken newKeyword, TypeSyntax type, ArgumentListSyntax argumentList)
        => new ObjectCreationExpressionSyntax(newKeyword, type, argumentList);
}


public partial class MethodDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken Identifier { get; }
    public override partial ParameterListSyntax ParameterList { get; }
    public partial ArrowTypeClauseSyntax ReturnType { get; }
    public override partial BlockSyntax? Body { get; }
    public override partial ArrowExpressionClauseSyntax? ExpressionBody { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, BlockSyntax body, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body.Green, terminatorToken?.Green))
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
    : this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody.Green, terminatorToken?.Green, null))
    {
    }

    public MethodDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, BlockSyntax? body, ArrowExpressionClauseSyntax? expressionBody, SyntaxToken? terminatorToken)
: this(new InternalSyntax.MethodDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameters.Green, (InternalSyntax.ArrowTypeClauseSyntax)returnType.Green, (InternalSyntax.BlockSyntax)body?.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody?.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, BlockSyntax body, SyntaxToken? terminatorToken)
        => new MethodDeclarationSyntax(modifiers, identifier, parameters, returnType, body, terminatorToken);

    public static MethodDeclarationSyntax MethodDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, ParameterListSyntax parameters, ArrowTypeClauseSyntax returnType, ArrowExpressionClauseSyntax expressionBody, SyntaxToken? terminatorToken)
        => new MethodDeclarationSyntax(modifiers, identifier, parameters, returnType, expressionBody, terminatorToken);
}


public partial class BracketedParameterListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenBracketToken { get; }
    public partial SeparatedSyntaxList<ParameterSyntax> Parameters { get; }
    public partial SyntaxToken CloseBracketToken { get; }

    internal BracketedParameterListSyntax(
        InternalSyntax.BracketedParameterListSyntax greenNode,
        SyntaxNode bracket = null,
        int position = 0)
        : base(greenNode, bracket, position)
    {
    }

    public BracketedParameterListSyntax(SyntaxToken openBracketToken, SeparatedSyntaxList<ParameterSyntax> parameters, SyntaxToken closeBracketToken)
          : this(
                new InternalSyntax.BracketedParameterListSyntax(openBracketToken.Green, parameters.Green, closeBracketToken.Green), null)
    {

    }

    public BracketedParameterListSyntax(SeparatedSyntaxList<ParameterSyntax> parameters)
        : this(SyntaxFactory.OpenBracketToken, parameters, SyntaxFactory.CloseBracketToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static BracketedParameterListSyntax BracketedParameterList(SeparatedSyntaxList<ParameterSyntax> parameters)
        => new BracketedParameterListSyntax(parameters);
}



public partial class InvocationExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax Expression { get; }
    public partial ArgumentListSyntax ArgumentList { get; }

    internal InvocationExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public InvocationExpressionSyntax(ExpressionSyntax expression, ArgumentListSyntax argumentList)
        : this(new InternalSyntax.InvocationExpressionSyntax((InternalSyntax.ExpressionSyntax)expression.Green, (InternalSyntax.ArgumentListSyntax)argumentList.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static InvocationExpressionSyntax InvocationExpression(ExpressionSyntax expression, ArgumentListSyntax argumentList)
        => new InvocationExpressionSyntax(expression, argumentList);
}


public abstract class ExpressionOrPatternSyntax : SyntaxNode
{
    internal ExpressionOrPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}


public partial class VariableDeclaratorSyntax : SyntaxNode
{
    public partial SyntaxToken Identifier { get; }
    public partial TypeAnnotationClauseSyntax TypeAnnotation { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    internal VariableDeclaratorSyntax(
        InternalSyntax.VariableDeclaratorSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public VariableDeclaratorSyntax(SyntaxToken identifier)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax(identifier.Green), null)
    {

    }

    public VariableDeclaratorSyntax(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotation)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax(identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax?)typeAnnotation.Green), null)
    {

    }

    public VariableDeclaratorSyntax(SyntaxToken identifier, EqualsValueClauseSyntax initializer)
      : this(
            new InternalSyntax.VariableDeclaratorSyntax(identifier.Green, (InternalSyntax.EqualsValueClauseSyntax)initializer?.Green), null)
    {

    }

    public VariableDeclaratorSyntax(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotation, EqualsValueClauseSyntax initializer)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax(identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax?)typeAnnotation?.Green, (InternalSyntax.EqualsValueClauseSyntax)initializer?.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier)
        => new VariableDeclaratorSyntax(identifier);

    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotationSyntax)
        => new VariableDeclaratorSyntax(identifier, typeAnnotationSyntax);

    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier, EqualsValueClauseSyntax initalizer)
        => new VariableDeclaratorSyntax(identifier, initalizer);

    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotationSyntax, EqualsValueClauseSyntax initalizer)
        => new VariableDeclaratorSyntax(identifier, typeAnnotationSyntax, initalizer);
}


public partial class AccessorDeclarationSyntax : SyntaxNode
{
    public override partial SyntaxKind Kind { get; }

    public partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken Keyword { get; }

    public partial BlockSyntax? Body { get; }

    public partial ArrowExpressionClauseSyntax? ExpressionBody { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal AccessorDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0) : base(greenNode, parent, position)
    {
    }

    public AccessorDeclarationSyntax(SyntaxKind kind, SyntaxTokenList modifiers, SyntaxToken keyword, BlockSyntax body, SyntaxToken terminatorToken)
        : this(new InternalSyntax.AccessorDeclarationSyntax(kind, modifiers.Green, keyword.Green, (InternalSyntax.BlockSyntax)body.Green, terminatorToken.Green), null, 0)
    {
    }

    public AccessorDeclarationSyntax(SyntaxKind kind, SyntaxTokenList modifiers, SyntaxToken keyword, ArrowExpressionClauseSyntax expressionBody, SyntaxToken terminatorToken)
    : this(new InternalSyntax.AccessorDeclarationSyntax(kind, modifiers.Green, keyword.Green, (InternalSyntax.ArrowExpressionClauseSyntax)expressionBody.Green, terminatorToken.Green), null, 0)
    {
    }

    internal AccessorDeclarationSyntax(SyntaxKind kind, SyntaxTokenList modifiers, SyntaxToken keyword, BlockSyntax? body, ArrowExpressionClauseSyntax? expressionBody, SyntaxToken terminatorToken)
    : this(new InternalSyntax.AccessorDeclarationSyntax(kind, modifiers.Green, keyword.Green, (InternalSyntax.BlockSyntax?)body?.Green, (InternalSyntax.ArrowExpressionClauseSyntax?)expressionBody?.Green, terminatorToken.Green), null, 0)
    {
    }
}


public abstract partial class BaseMethodDeclarationSyntax : MemberDeclarationSyntax
{
    internal BaseMethodDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract ParameterListSyntax ParameterList { get; }
    public abstract BlockSyntax? Body { get; }
    public abstract ArrowExpressionClauseSyntax? ExpressionBody { get; }
}




public partial class ElementAccessExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax Expression { get; }
    public partial BracketedArgumentListSyntax ArgumentList { get; }

    internal ElementAccessExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ElementAccessExpressionSyntax(ExpressionSyntax expression, BracketedArgumentListSyntax argumentList)
        : this(new InternalSyntax.ElementAccessExpressionSyntax((InternalSyntax.ExpressionSyntax)expression.Green, (InternalSyntax.BracketedArgumentListSyntax)argumentList.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ElementAccessExpressionSyntax ElementAccessExpression(ExpressionSyntax expression, BracketedArgumentListSyntax argumentList)
        => new ElementAccessExpressionSyntax(expression, argumentList);
}

