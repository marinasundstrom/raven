# Syntax (Proposal)

## Expression-first

We should focus on expressions over statements.

Every statement is also an expression that may return a value.

Also implying that we will make ``return`` optional.

Here is wild proposal of a block as an expression allowing you do scope code:

```c#
let x = {
    42; 
};

Console.WriteLine(x); // 42
```

## ``If`` statement

Here is the proposed ``if`` statement:

```c#
if file.IsOpen {

} else {

}
```

With ``else`` and ``else if`` statements:

```c#
if a {

} else if b {
    
} else {
    
}
```

Parentheses around conditions are optional and part of the expression:

```c#
if (file.IsOpen) {

}
```

### ``let`` vs ``var`` declarations

Explanation:

* ``let`` is a name whose value can't be changed, but overridden.

* ``var`` is a variable in its true sense. It's value may change.