using System;

namespace Raven.CodeAnalysis.Syntax
{
    public partial class CustomSyntaxNode
    {
        Raven.CodeAnalysis.Syntax.SyntaxToken _identifierToken;
        Raven.CodeAnalysis.Syntax.SyntaxNode _childNode;
        Raven.CodeAnalysis.Syntax.SyntaxToken _identifierToken2;
        public partial Raven.CodeAnalysis.Syntax.SyntaxToken IdentifierToken
        {
            get
            {
                return default;
            }
        }

        public partial Raven.CodeAnalysis.Syntax.SyntaxNode ChildNode
        {
            get
            {
                return default;
            }
        }

        public partial Raven.CodeAnalysis.Syntax.SyntaxToken IdentifierToken2
        {
            get
            {
                return default;
            }
        }

        public override SyntaxNode? GetNodeSlot(int index)
        {
            return (SyntaxNode? )(index switch
            {
                1 => ((SyntaxNode)this).GetRed<SyntaxNode>(ref _childNode, 1),
                _ => null
            });
        }
    }
}