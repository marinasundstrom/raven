namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal sealed class SeekableTextSource
{
    private readonly LinkedList<char> _buffer = new();
    private readonly TextReader _reader;
    private int _position;
    private int _absoluteStart; // The absolute index of the first character in the buffer
    private readonly int _maxBufferSize;

    private readonly Stack<int> _savedPositions = new(); // Optional: to help with pruning

    public SeekableTextSource(TextReader reader, int initialPosition = 0, int maxBufferSize = int.MaxValue)
    {
        _reader = reader;
        _maxBufferSize = maxBufferSize;
        _position = initialPosition;
        _absoluteStart = initialPosition;
    }

    public int Position => _position;

    /// <summary>
    /// Reset the position to the given index
    /// </summary>
    public void ResetPosition(int position)
    {
        if (position < _absoluteStart)
        {
            // If the requested position predates the buffered range, clamp it to
            // the earliest available character instead of throwing. The parser
            // occasionally rewinds past the start when recovering from errors.
            position = _absoluteStart;
        }

        // Ensure we have buffered characters up to the requested position
        // before validating the upper bound. The parser may attempt to rewind
        // to positions beyond the currently buffered range (e.g., after peeking
        // ahead for newline handling). Buffering first avoids spurious
        // out-of-range errors when the position is still within the source text.
        EnsureBuffered(position);

        if (position > _absoluteStart + _buffer.Count)
            position = _absoluteStart + _buffer.Count;

        _position = position;
    }

    /// <summary>
    /// Push position to stack
    /// </summary>
    public int PushPosition()
    {
        _savedPositions.Push(_position);
        return _position;
    }

    /// <summary>
    /// Pop and restore position from stack
    /// </summary>
    public int PopAndRestorePosition()
    {
        var position = _savedPositions.Pop();
        ResetPosition(position);
        return position;
    }


    /// <summary>
    /// Pop position from stack, without restoring
    /// </summary>
    public int PopPosition()
    {
        return _savedPositions.Pop();
    }

    public int Peek()
    {
        EnsureBuffered(_position);
        return InBufferRange(_position) ? GetFromBuffer(_position) : -1;
    }

    public int Read()
    {
        EnsureBuffered(_position);

        if (!InBufferRange(_position))
        {
            return -1;
        }

        var value = GetFromBuffer(_position);
        _position++;
        return value;
    }

    private void EnsureBuffered(int position)
    {
        int neededOffset = position - _absoluteStart;
        while (_buffer.Count <= neededOffset)
        {
            int next = _reader.Read();
            if (next == -1)
                break;

            _buffer.AddLast((char)next);

            // Trim buffer if it exceeds max size
            if (_buffer.Count > _maxBufferSize)
            {
                _absoluteStart++;
                _buffer.RemoveFirst();
            }
        }
    }

    private int GetFromBuffer(int position)
    {
        var node = _buffer.First;
        int offset = position - _absoluteStart;
        for (int i = 0; i < offset && node != null; i++)
            node = node.Next;

        return node?.Value ?? -1;
    }

    private bool InBufferRange(int position) =>
        position >= _absoluteStart && position < _absoluteStart + _buffer.Count;
}
