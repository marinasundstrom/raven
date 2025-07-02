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

 For contextual parsing, you can create a checkpoint, by calling `Checkpoint` in the parser. And then restore it by calling `Backtrack`.

 The lexer does support resetting to a specified position. This is ultimately used by the aforementioned functionality.