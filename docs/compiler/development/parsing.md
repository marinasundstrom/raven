# Parsing

The parser is divided into sub-parsers that create sub-contexts with shared access to the lexer. There is backtracking support.

## Peeking tokens

Sometimes you want to look-ahead in the token stream in order to know what actions to take next.

The `BaseParseContext` class provides this method: `PeekToken(int index = 0)` 

You can peek any number of tokens ahead of you by their zero-based index.

### Example

```csharp
foo()
```

Peeking the tokens:

 ```csharp
var t1 = PeekToken(0); // Identifier 'foo'
var t1 = PeekToken(1); // '('
var t1 = PeekToken(2); // ')'
 ```

### In the `Lexer`

 Similarly, the `Lexer` has a `PeekToken(int index = 0)` method, but that is used when identifying compound operator tokens, or trivia such as comments.

 You won't touch this code unless you are adding new tokens and trivia.

 ## Backtracking

 For contextual parsing, you can create a checkpoint, by calling `CreateCheckpoint` in the parser. And then restore to that checkpoint by either calling the method `Rewind` or disposing the object.

 The lexer does support resetting to a specified position. This is ultimately used by the aforementioned functionality.

## Seekable reader retention

`SeekableTextSource` is the internal input boundary used by the lexer. Its position
is an absolute UTF-16 offset, and the supplied reader starts at `initialPosition`.
The reader must be non-null, the initial position nonnegative, and the buffer limit
positive. Peek operations do not consume input; repeated reads at EOF leave the
position unchanged. Reset clamps to the available input range.

The buffer limit bounds retained history, not active lookahead or live checkpoints.
Those can temporarily require more space. Trimming removes only consumed characters
older than the current position and all saved positions. Clearing a checkpoint
makes its history eligible for trimming; restoring one rewinds before trimming.
A reset into already-pruned history clamps to the earliest retained character.
The default limit remains `int.MaxValue`; the lexer does not select a smaller limit.
Shared C# tests exercise both the C# and Raven implementations, including a small
limit that forces pruning. Adjacent lexer tests verify tokenization behavior.
