# Parsing

## Peeking tokens

Sometimes you want to look-ahead in the token stream in order to know what actions to take next.

The `Tokenizer` class provides this method: `PeekToken(int offset = 0)` 

You can peek any number of tokens ahead of you by their zero-based offset.

### Example

````
foo()
````

Peeking the tokens:

 ```csharp
var t1 = PeekToken(0); // Identifier 'foo'
var t1 = PeekToken(1); // '('
var t1 = PeekToken(2); // ')'
 ```

### In the `Lexer`

 Similarly, the `Lexer` has a `PeekToken(int offset = 0)` method, but that is used when identifying compound operator tokens, or trivia such as comments.

 You won't touch this code unless you are adding new tokens and trivia.