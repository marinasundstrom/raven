namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal sealed class SeekableTextSource
{
    private readonly List<char> _buffer = new();
    private readonly TextReader _reader;
    private int _position;
    private int _absoluteStart; // The absolute index of the first character in the buffer
    private readonly int _maxBufferSize;

    private readonly Stack<int> _savedPositions = new();

    public SeekableTextSource(TextReader reader, int initialPosition = 0, int maxBufferSize = int.MaxValue)
    {
        ArgumentNullException.ThrowIfNull(reader);
        if (initialPosition < 0)
            throw new ArgumentOutOfRangeException(nameof(initialPosition));
        if (maxBufferSize <= 0)
            throw new ArgumentOutOfRangeException(nameof(maxBufferSize));

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
        TrimBuffer();
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
        var position = _savedPositions.Pop();
        TrimBuffer();
        return position;
    }

    public int Peek()
    {
        EnsureBuffered(_position);
        return InBufferRange(_position) ? GetFromBuffer(_position) : -1;
    }

    public bool PeekChar(int offset, out char ch)
    {
        EnsureBuffered(_position + offset);
        var targetPosition = _position + offset;
        if (!InBufferRange(targetPosition))
        {
            ch = '\0';
            return false;
        }
        var no = GetFromBuffer(targetPosition);
        if (no == -1)
        {
            ch = '\0';
            return false;
        }
        ch = (char)no;
        return true;
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
        TrimBuffer();
        return value;
    }

    private void EnsureBuffered(int position)
    {
        while (_buffer.Count <= position - _absoluteStart)
        {
            int next = _reader.Read();
            if (next == -1)
                break;

            _buffer.Add((char)next);

            TrimBuffer();
        }
    }

    private void TrimBuffer()
    {
        if (_buffer.Count <= _maxBufferSize)
            return;

        // Lookahead and checkpoints may temporarily exceed the retention limit.
        // Only consumed characters older than every saved position can be removed.
        var retainFrom = _position;
        foreach (var saved in _savedPositions)
            retainFrom = Math.Min(retainFrom, saved);
        var removeCount = Math.Min(_buffer.Count - _maxBufferSize, retainFrom - _absoluteStart);
        if (removeCount <= 0)
            return;

        _buffer.RemoveRange(0, removeCount);
        _absoluteStart += removeCount;
    }

    private int GetFromBuffer(int position)
    {
        int offset = position - _absoluteStart;
        return (uint)offset < (uint)_buffer.Count ? _buffer[offset] : -1;
    }

    private bool InBufferRange(int position) =>
        position >= _absoluteStart && position < _absoluteStart + _buffer.Count;
}
