namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

public sealed class SeekableTextSource
{
    private readonly LinkedList<char> _buffer = new();
    private readonly TextReader _reader;
    private int _position;
    private int _absoluteStart; // The absolute index of the first character in the buffer
    private readonly int _maxBufferSize;

    private readonly Stack<int> _savedPositions = new(); // Optional: to help with pruning

    public SeekableTextSource(TextReader reader, int maxBufferSize = 1024)
    {
        _reader = reader;
        _maxBufferSize = maxBufferSize;
    }

    public int Position => _position;

    public void Restore(int position)
    {
        if (position < _absoluteStart || position > _absoluteStart + _buffer.Count)
            throw new InvalidOperationException("Position outside of buffer bounds.");
        _position = position;
    }

    public int Save()
    {
        _savedPositions.Push(_position);
        return _position;
    }

    public char Peek()
    {
        EnsureBuffered(_position);
        return InBufferRange(_position) ? GetFromBuffer(_position) : '\0';
    }

    public char Read()
    {
        EnsureBuffered(_position);
        return InBufferRange(_position) ? GetFromBuffer(_position++) : '\0';
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

    private char GetFromBuffer(int position)
    {
        var node = _buffer.First;
        int offset = position - _absoluteStart;
        for (int i = 0; i < offset && node != null; i++)
            node = node.Next;

        return node?.Value ?? '\0';
    }

    private bool InBufferRange(int position) =>
        position >= _absoluteStart && position < _absoluteStart + _buffer.Count;
}