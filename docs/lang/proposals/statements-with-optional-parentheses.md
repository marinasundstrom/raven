# Proposal: Statements with optional parentheses

> ℹ️ This proposal has been partly implemented

For most C-like languages, parentheses are important in many cases and not optional, like in the `if` statement.

Raven should make them optional:

## Proposal

Here is the proposed ``if`` statement:

```c#
if file.IsOpen {

} else {

}
```

Parentheses around conditions are optional, and part of the expression that form the condition:

```c#
if (file.IsOpen) {

}
```