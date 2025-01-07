using System.Collections.Immutable;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public class SourceText
{
    private readonly string _text;
    private readonly Encoding _encoding;
    private readonly List<int> _lineStarts;

    public Encoding Encoding => _encoding;

    public int Length => _text.Length;

    private SourceText(string text, Encoding? encoding = default)
    {
        _text = text ?? throw new ArgumentNullException(nameof(text));
        _encoding = encoding ?? Encoding.UTF8;
        _lineStarts = TextUtils.ComputeLineStarts(_text);
    }

    public static SourceText From(string text, Encoding? encoding = default)
    {
        return new SourceText(text, encoding);
    }

    public static SourceText From(Stream stream, Encoding? encoding = default)
    {
        encoding ??= Encoding.UTF8;

        using var textReader = new StreamReader(stream, encoding);

        return new SourceText(textReader.ReadToEnd(), encoding);
    }

    public char this[int index]
    {
        get
        {
            throw new Exception();
        }
    }

    public SourceText WithChange(int position, string newText)
    {
        if (position < 0 || position > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(position));

        // Apply the change
        string updatedText = _text.Substring(0, position) + newText + _text.Substring(position);
        return From(updatedText, Encoding);
    }

    public SourceText WithChange(TextChange change)
    {
        if (change.Span.Start < 0 || change.Span.Start > _text.Length ||
            change.Span.End < 0 || change.Span.End > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(change), "The span is out of bounds.");

        // Apply the change
        string updatedText = _text.Substring(0, change.Span.Start) +
                             change.NewText +
                             _text.Substring(change.Span.End);

        // Return a new SourceText instance with the updated text
        return SourceText.From(updatedText, Encoding);
    }

    public (int line, int column) GetLineAndColumn(TextSpan span) => GetLineAndColumn(span.Start);

    private (int line, int column) GetLineAndColumn(int position)
    {
        if (position < 0 || position > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(position));

        return TextUtils.GetLineAndColumn(_lineStarts, position);
    }

    public TextReader GetTextReader()
    {
        return new StringReader(_text);
    }

    public TextReader GetTextReader(int position)
    {
        MemoryStream stream = new MemoryStream(Encoding.UTF8.GetBytes(_text));
        StreamReader reader = new StreamReader(stream);
        stream.Seek(position, SeekOrigin.Begin);
        return reader;
    }

    public IReadOnlyList<TextChange> GetTextChanges(SourceText oldText)
    {
        if (oldText == null)
            throw new ArgumentNullException(nameof(oldText));

        var oldTextString = oldText._text;
        var newTextString = this._text;

        int oldLength = oldTextString.Length;
        int newLength = newTextString.Length;

        int start = 0;
        while (start < oldLength && start < newLength && oldTextString[start] == newTextString[start])
        {
            start++;
        }

        if (start == oldLength && start == newLength)
        {
            // No changes
            return Array.Empty<TextChange>();
        }

        int oldEnd = oldLength - 1;
        int newEnd = newLength - 1;

        while (oldEnd >= start && newEnd >= start && oldTextString[oldEnd] == newTextString[newEnd])
        {
            oldEnd--;
            newEnd--;
        }

        // Calculate the spans and the new text
        int oldSpanStart = start;
        int oldSpanLength = oldEnd - start + 1;
        int newSpanStart = start;
        int newSpanLength = newEnd - start + 1;

        string changedText = newTextString.Substring(newSpanStart, newSpanLength);

        return new List<TextChange>
        {
            new TextChange(new TextSpan(oldSpanStart, oldSpanLength), changedText)
        };
    }

    public IReadOnlyList<TextChangeRange> GetChangeRanges(SourceText oldText)
    {
        throw new NotImplementedException();
    }

    public bool ContentEquals(SourceText other)
    {
        return _text.Equals(other._text);
    }

    public void CopyTo(int sourceIndex, char[] destination, int destinationIndex, int count)
    {
        throw new NotImplementedException();
    }

    public ImmutableArray<byte> GetChecksum()
    {
        throw new NotImplementedException();
    }

    public ImmutableArray<byte> GetContentHash()
    {
        throw new NotImplementedException();
    }

    public string GetSubText(TextSpan span) => _text.Substring(span.Start, span.Length);

    public string GetSubText(int start, int length) => _text.Substring(start, length);

    public SourceText Replace(TextSpan span, string newText)
    {
        throw new NotImplementedException();
    }

    public SourceText Replace(int start, int length, string newText)
    {
        throw new NotImplementedException();
    }

    public TextLineCollection GetLines()
    {
        throw new NotImplementedException();
    }

    public void Write(TextWriter textWriter, CancellationToken cancellationToken)
    {
        throw new NotImplementedException();
    }

    public void Write(TextWriter textWriter, TextSpan span, CancellationToken cancellationToken)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        return _text.ToString();
    }

    public string ToString(TextSpan span)
    {
        return GetSubText(span);
    }
}