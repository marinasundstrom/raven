# Proposal: Expression-first

> ℹ️ This proposal has been partly implemented

We should focus on expressions over statements.

Every statement is an expression that may return a value.

## Blocks

Here is wild proposal of a block as an expression allowing you do scope code:

```c#
let x = {
    42; 
};

Console.WriteLine(x); // 42
```

### Automatic return

Also implying that we will make ``return`` optional.

## Conclusion

What traditionally is seen as statements in most languages should also be valid expressions.