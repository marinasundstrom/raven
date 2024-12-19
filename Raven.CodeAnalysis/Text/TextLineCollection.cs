
using System.Collections;

namespace Raven.CodeAnalysis.Text;

public class TextLineCollection : IEnumerable<TextLine>
{
    public TextLine this[int index]
    {
        get
        {
            throw new NotImplementedException();
        }
    }

    public IEnumerator<TextLine> GetEnumerator()
    {
        throw new NotImplementedException();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}
