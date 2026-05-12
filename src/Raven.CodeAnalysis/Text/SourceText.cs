using System;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public class SourceText
{
    private const int ChangeRangeChainScanLimit = 32;

    private readonly string _text;
    private readonly Encoding _encoding;
    private readonly List<int> _lineStarts;
    private readonly WeakReference<SourceText>? _previousText;
    private readonly IReadOnlyList<TextChangeRange>? _changeRanges;

    public Encoding Encoding => _encoding;

    public int Length => _text.Length;

    private SourceText(
        string text,
        Encoding? encoding = default,
        SourceText? previousText = null,
        IReadOnlyList<TextChangeRange>? changeRanges = null)
    {
        _text = text ?? throw new ArgumentNullException(nameof(text));
        _encoding = encoding ?? Encoding.UTF8;
        _lineStarts = TextUtils.ComputeLineStarts(_text);
        _previousText = previousText is not null ? new WeakReference<SourceText>(previousText) : null;
        _changeRanges = changeRanges;
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

        return WithChange(new TextChange(new TextSpan(position, 0), newText));
    }

    public SourceText WithChange(TextChange change)
    {
        if (change.Span.Start < 0 || change.Span.Start > _text.Length ||
            change.Span.End < 0 || change.Span.End > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(change), "The span is out of bounds.");

        string updatedText = _text.Substring(0, change.Span.Start) +
                             change.NewText +
                             _text.Substring(change.Span.End);

        return new SourceText(
            updatedText,
            Encoding,
            previousText: this,
            changeRanges: [new TextChangeRange(change.NewText.Length, change.Span)]);
    }

    public (int line, int column) GetLineAndColumn(TextSpan span) => GetLineAndColumn(span.Start);

    public (int line, int column) GetLineAndColumn(int position)
    {
        // Clamp out-of-range positions instead of throwing. Some recovery paths
        // can request spans slightly beyond the end of the source text.
        if (position < 0)
            position = 0;
        else if (position > _text.Length)
            position = _text.Length;

        return TextUtils.GetLineAndColumn(_lineStarts, position);
    }

    public int GetLineCount() => _lineStarts.Count;

    public int GetLineLength(int zeroBasedLine)
    {
        if (zeroBasedLine < 0)
            return 0;

        if (zeroBasedLine >= _lineStarts.Count)
            return 0;

        var start = _lineStarts[zeroBasedLine];
        var end = zeroBasedLine + 1 < _lineStarts.Count ? _lineStarts[zeroBasedLine + 1] : _text.Length;

        while (end > start && (_text[end - 1] == '\n' || _text[end - 1] == '\r'))
            end--;

        return Math.Max(0, end - start);
    }

    public TextReader GetTextReader()
    {
        return new StringReader(_text);
    }

    public TextReader GetTextReader(int position)
    {
        if (position <= 0)
        {
            return new StringReader(_text);
        }

        if (position >= _text.Length)
        {
            return new StringReader(string.Empty);
        }

        var current = _text[position];

        // Fast path for the common case where the current character is ASCII. In that
        // case we can slice directly without any additional checks.
        if (current <= 0x7F)
        {
            return new StringReader(_text[position..]);
        }

        // For non-ASCII content make sure we don't start reading in the middle of a
        // surrogate pair. This still keeps slicing fast for Latin text while keeping
        // Unicode content intact.
        if (char.IsLowSurrogate(current) && position > 0 && char.IsHighSurrogate(_text[position - 1]))
        {
            position--;
        }

        return new StringReader(_text[position..]);
    }

    public IReadOnlyList<TextChange> GetTextChanges(SourceText oldText)
    {
        if (oldText == null)
            throw new ArgumentNullException(nameof(oldText));

        var ranges = GetChangeRanges(oldText);
        if (ranges.Count == 0)
            return Array.Empty<TextChange>();

        if (!IsFullTextChangeRange(oldText, ranges))
            return CreateTextChangesFromRanges(ranges);

        return ComputeTextChanges(oldText);
    }

    public IReadOnlyList<TextChangeRange> GetChangeRanges(SourceText oldText)
    {
        if (oldText == null)
            throw new ArgumentNullException(nameof(oldText));

        if (ReferenceEquals(this, oldText) || ContentEquals(oldText))
            return Array.Empty<TextChangeRange>();

        if (_previousText is not null &&
            _previousText.TryGetTarget(out var directPrevious) &&
            ReferenceEquals(directPrevious, oldText) &&
            _changeRanges is not null)
        {
            return _changeRanges;
        }

        if (IsChangedFrom(oldText))
            return ComputeTextChanges(oldText)
                .Select(change => new TextChangeRange(change.NewText.Length, change.Span))
                .ToArray();

        return ComputeTextChanges(oldText)
            .Select(change => new TextChangeRange(change.NewText.Length, change.Span))
            .ToArray();
    }

    private IReadOnlyList<TextChange> CreateTextChangesFromRanges(IReadOnlyList<TextChangeRange> ranges)
    {
        var changes = new List<TextChange>(ranges.Count);
        var delta = 0;

        foreach (var range in ranges)
        {
            var newStart = range.Span.Start + delta;
            changes.Add(new TextChange(
                range.Span,
                _text.Substring(newStart, range.NewLength)));
            delta += range.NewLength - range.Span.Length;
        }

        return changes;
    }

    private static bool IsFullTextChangeRange(SourceText oldText, IReadOnlyList<TextChangeRange> ranges)
        => ranges.Count == 1 &&
           ranges[0].Span.Start == 0 &&
           ranges[0].Span.Length == oldText.Length;

    private bool IsChangedFrom(SourceText oldText)
    {
        var current = this;
        for (var i = 0; i < ChangeRangeChainScanLimit; i++)
        {
            if (current._previousText is null ||
                !current._previousText.TryGetTarget(out var previous))
            {
                return false;
            }

            if (ReferenceEquals(previous, oldText))
                return true;

            current = previous;
        }

        return false;
    }

    private IReadOnlyList<TextChange> ComputeTextChanges(SourceText oldText)
    {
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
        using var sha256 = SHA256.Create();
        var bytes = Encoding.GetBytes(_text);
        return ImmutableArray.CreateRange(sha256.ComputeHash(bytes));
    }

    public ImmutableArray<byte> GetContentHash()
    {
        return GetChecksum();
    }

    public string GetSubText(TextSpan span) => _text.Substring(span.Start, span.Length);

    public string GetSubText(int start, int length) => _text.Substring(start, length);

    public SourceText Replace(TextSpan span, string newText)
    {
        return WithChange(new TextChange(span, newText));
    }

    public SourceText Replace(int start, int length, string newText)
    {
        return Replace(new TextSpan(start, length), newText);
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
