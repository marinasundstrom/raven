# Compiler diagnostics

Raven emits diagnostics during compilation to report syntax and semantic issues.
Each diagnostic has an identifier of the form `RAV####`.
The examples below demonstrate common situations that trigger each diagnostic.

## RAV0021: Cannot apply indexing with [] to an expression of type '{0}'
Attempted to index a value whose type does not support indexing.

```raven
let number = 0
let x = number[0] // RAV0021
```

## RAV0023: Operator cannot be applied to operand of type
Applying a unary operator to an unsupported operand type.

```raven
let text = "hi"
let value = -text // RAV0023
```

## RAV0024: Operator cannot be applied to operands of types
Using a binary operator with incompatible operand types.

```raven
let result = 1 + "a" // RAV0024
```

## RAV0027: This value is not mutable
Attempted to assign to an immutable value.

```raven
let x = 1
x = 2 // RAV0027
```

## RAV0030: Invalid invocation
Tried to invoke a value that is not callable.

```raven
let n = 5
n() // RAV0030
```

## RAV0103: The name does not exist in the current context
Referenced an identifier that is not defined.

```raven
value // RAV0103
```

## RAV0111: Member already defined
Type declares two members with the same signature.

```raven
type C {
    func f() {}
    func f() {} // RAV0111
}
```

## RAV0112: Function already defined
A free function with the same name and parameters already exists.

```raven
func main() {}
func main() {} // RAV0112
```

## RAV0117: Member does not contain definition
Member access targets an unknown member.

```raven
let x = ()
let y = x.missing // RAV0117
```

## RAV0118: Variable used like a type
Using a variable where a type is expected.

```raven
let value = 0
let y: value // RAV0118
```

## RAV0121: Call is ambiguous
Call matches multiple overloads equally well.

```raven
func f(x: int) {}
func f(x: string) {}

f(_) // RAV0121
```

## RAV0131: The left-hand side of an assignment must be a variable, property or indexer
Assignment target is not assignable.

```raven
1 = 2 // RAV0131
```

## RAV0149: Method name expected
Missing method name in declaration.

```raven
func (x: int) {} // RAV0149
```

## RAV0165: Use of unassigned local variable
A variable is read before being assigned a value.

```raven
let x: int
let y = x // RAV0165
```

## RAV0166: Local variable must be initialized
Variable declaration requires an initializer.

```raven
let x: int // RAV0166
```

## RAV0167: Variable already defined
Redeclared a variable with the same name in the same scope.

```raven
let x = 1
let x = 2 // RAV0167
```

## RAV0168: Variable shadows outer scope variable
Declared a variable with same name as one in an outer scope.

```raven
let x = 1
if true {
    let x = 2 // RAV0168 (warning)
}
```

## RAV0191: A readonly field cannot be assigned to
Attempted to assign to a readonly field outside of its initializer or constructor.

```raven
type C {
    let x: int
}

let c = C()
c.x = 1 // RAV0191
```

## RAV0200: Property or indexer cannot be assigned to -- it is read only
Assignment to a read-only property or indexer.

```raven
type C {
    let x: int
}
let c = C()
c.x = 0 // RAV0200
```

## RAV0234: Type or namespace does not exist in namespace
Referenced a nested type or namespace that does not exist.

```raven
use System.Missing // RAV0234
```

## RAV0235: Type expected without wildcard
Used a wildcard where a concrete type is required.

```raven
let x: _ // RAV0235
```

## RAV0269: Unassigned out parameter
Out parameter is not assigned before returning.

```raven
func set(out x: int) {
    // missing assignment to x
} // RAV0269
```

## RAV0305: Type requires type arguments
Generic type used without specifying required type arguments.

```raven
let list: List // RAV0305
```

## RAV0306: Cannot inherit from sealed type
Attempted to derive from a sealed type.

```raven
type MyString : string {} // RAV0306
```

## RAV0400: Nullable type not allowed in union types
Union includes a nullable type.

```raven
type U = int | string? // RAV0400
```

## RAV0426: Type name does not exist in type
Referenced a nested type that does not exist.

```raven
type A {}
let x: A.B // RAV0426
```

## RAV0815: Cannot assign void to an implicitly-typed variable
Implicitly-typed variable assigned a `void` (unit) value.

```raven
func log() {}
let x = log() // RAV0815
```

## RAV1001: Identifier expected
Parser expected an identifier but none was provided.

```raven
let = 42 // RAV1001
```

## RAV1002: Semicolon expected
Missing semicolon between statements.

```raven
let x = 1
let y = 2 // RAV1002
```

## RAV1003: Character expected
Missing required character such as a parenthesis or brace.

```raven
func main( { } // RAV1003
```

## RAV1004: Duplicate modifier
Repeated the same modifier.

```raven
pub pub func main() {} // RAV1004
```

## RAV1005: Import directive out of order
Import directives must precede alias directives and declarations.

```raven
type C {}
use System // RAV1005
```

## RAV1006: Alias directive out of order
Alias directives must appear before member declarations.

```raven
type C {}
alias x = int // RAV1006
```

## RAV1009: Unrecognized escape sequence
String or character literal contains an invalid escape.

```raven
let s = "\q" // RAV1009
```

## RAV1010: Newline in constant
Constant literal spans multiple lines without proper escaping.

```raven
let s = "unfinished
" // RAV1010
```

## RAV1011: File-scope code out of order
File-scoped statements appear after declarations.

```raven
type C {}
1 + 2 // RAV1011
```

## RAV1012: File-scope code requires console application
Top-level code used in a project that is not a console application.

```raven
// In a library project
doStuff() // RAV1012
```

## RAV1013: File-scope code may only appear in one file
Multiple files contain top-level statements.

```raven
// second file with top-level code
print("hi") // RAV1013
```

## RAV1501: No overload for method takes argument
Called a method with a wrong number of arguments.

```raven
func f(x: int) {}

f(1, 2) // RAV1501
```

## RAV1503: Cannot convert from type to type
Implicit conversion between types is not allowed.

```raven
let x: int = "hi" // RAV1503
```

## RAV1504: Cannot assign to type
Assignment from incompatible type.

```raven
let x: int
x = true // RAV1504
```

## RAV1525: Invalid expression term
Encountered an unexpected token in an expression.

```raven
let x = ) // RAV1525
```

## RAV1526: Member access on void value
Accessed a member on a value of type `void`.

```raven
func log() {}
log().ToString() // RAV1526
```

## RAV1900: Return statement not allowed here
`return` used inside an expression producing a value.

```raven
let x = if flag { return 1 } else { 2 } // RAV1900
```

## RAV1955: Non-invocable member cannot be used like a method
Attempted to invoke a member that is not a method.

```raven
let s = "text"
let len = s.Length() // RAV1955
```

## RAV2001: Numeric literal out of range
Numeric literal is outside the range of its target type.

```raven
let b: byte = 1000 // RAV2001
```

## RAV2002: Unterminated character literal
Character literal missing closing quote.

```raven
let ch = 'a // RAV2002
```

## RAV2003: Invalid escape sequence
Character literal uses an invalid escape.

```raven
let ch = '\x' // RAV2003
```

## RAV2010: Target-typed member access requires a known type
Target-typed member access without a known type.

```raven
let x = .ToString() // RAV2010
```

## RAV2020: Invalid alias target
Alias directive targets an unsupported symbol.

```raven
alias five = 5 // RAV2020
```
