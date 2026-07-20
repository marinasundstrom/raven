# Raven for absolute beginners

This guide is for people who are new to programming. You do not need to know
C#, .NET, functional programming, or object-oriented programming.

A program is a set of instructions for a computer. Programming means describing
those instructions precisely enough for the computer to follow and clearly
enough for another person to understand.

Raven lets a small program stay small. You can begin with direct instructions,
then introduce functions and types when they become useful.

## Before you begin

Raven is experimental and currently runs from a source checkout. Follow the
build steps in [Getting Started](getting-started.md) first.

Save each example as `beginner.rav`. From the Raven repository root, compile and
run it with:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  beginner.rav -o /tmp/raven-beginner.dll
dotnet /tmp/raven-beginner.dll
```

If the compiler reports an error, start with the first message. It normally
includes the line and character where Raven became confused.

The examples build on ideas introduced earlier. When a later fragment calls
`WriteLine`, keep `import System.Console.*` at the top of your file.

## Your first program

```raven
import System.Console.*

WriteLine("Hello, Raven!")
```

`WriteLine` displays text in the terminal. Text surrounded by quotation marks is
called a string.

The `import` line lets the program use the short name `WriteLine` for the .NET
operation `System.Console.WriteLine`.

The call is written directly in the file. This is a top-level statement. Raven
does not require you to create a class before your program can do something.

## Remembering values

A program can give a name to a value and use it later:

```raven
import System.Console.*

let name = "Mira"
let age = 12

WriteLine("$name is $age years old")
```

`let` gives a name to a value that will not be replaced later. The `$name` and
`$age` parts insert those values into the surrounding string.

When a value really needs to change, use `var`:

```raven
var score = 0
score = score + 10
```

Prefer `let` until you have a reason to use `var`.

## Values have types

A type describes what kind of value something is and which operations make
sense for it.

```raven
let name: string = "Mira"
let age: int = 12
let height: decimal = 1.52
let isLearning: bool = true
```

- `string` represents text.
- `int` represents whole numbers.
- `decimal` represents decimal numbers.
- `bool` represents either `true` or `false`.

Raven can infer these types, so the annotations are often optional. Types help
the compiler find mistakes before the program runs.

## Making decisions

Use `if` when a program should choose between two paths:

```raven
import System.Console.*

let temperature = 28

if temperature > 25 {
    WriteLine("It is warm")
} else {
    WriteLine("It is cool")
}
```

The condition after `if` is either `true` or `false`. Raven runs the first block
when it is true and the `else` block when it is false.

An `if` can also produce a value:

```raven
let description = if temperature > 25 {
    "warm"
} else {
    "cool"
}
```

## Repeating work

A collection holds several values. This array contains three names:

```raven
import System.Console.*

let names = ["Mira", "Noah", "Ari"]

for name in names {
    WriteLine("Hello, $name!")
}
```

The `for` loop runs the same instructions once for each name.

## Understanding braces and scopes

The `if` and `for` examples both use braces: `{` begins a block of code and `}`
ends it. An ordinary block creates a new **scope**. A scope is the part of the
program where a name can be used.

```raven
import System.Console.*

let outside = "I belong to the outer scope"

if true {
    let inside = "I belong to the if block"
    WriteLine(outside)
    WriteLine(inside)
}

WriteLine(outside)
// WriteLine(inside) // Error: inside is not in scope here.
```

Code inside a block can use names declared in an enclosing, outer scope. Code
outside the block cannot use locals declared inside it. At the closing `}`, the
name `inside` goes out of scope.

The same rule applies to loop bodies. A name declared inside the loop cannot be
used after the loop ends:

```raven
for name in names {
    let greeting = "Hello, $name"
    WriteLine(greeting)
}

// name and greeting are not in scope here.
```

You will encounter the same idea again in function bodies and `match` arms.
Each block keeps its local names contained, so different parts of the program
can use simple names without interfering with one another.

Scope describes where a **name** is available. It is not exactly the same as how
long an object remains alive. Later, you may return an object from a function or
store it somewhere outside the block. That object can continue to exist even
though the local name used inside the block is gone.

## Naming behavior with functions

A function gives a name to an operation. It can receive input and produce a
result.

```raven
import System.Console.*

func Greet(name: string) -> string {
    return "Hello, $name!"
}

let message = Greet("Mira")
WriteLine(message)
```

Read the declaration from left to right:

- `func` declares a function.
- `Greet` is its name.
- `name: string` is a text input named `name`.
- `-> string` says the result is text.
- `return` supplies that result.

A short function can be written as one expression:

```raven
func Add(a: int, b: int) -> int => a + b
```

Functions do not need to be wrapped in a class. Use them directly for
calculations, validation, formatting, and workflows.

## Creating your own data types

Programs become easier to understand when important concepts have meaningful
types.

```raven
record class Person(val Name: string, val Age: int)

let person = Person("Mira", 12)
```

`Person` groups a name and age into one value. The `val` parameters become
properties that can be read as `person.Name` and `person.Age`.

## Modeling alternatives

Some values can be one of several meaningful cases. A union names those cases:

```raven
union Weather {
    case Sunny
    case Rainy(amount: int)
    case Snowy(depth: int)
}
```

Use `match` to handle every alternative:

```raven
func Describe(weather: Weather) -> string {
    return weather match {
        .Sunny => "It is sunny"
        .Rainy(let amount) => "Rain: $amount mm"
        .Snowy(let depth) => "Snow: $depth cm"
    }
}
```

The data and the decisions use the same case names, which makes the program
easier to follow.

## Values that may be absent

Sometimes not finding a value is normal. `Option<T>` represents either a value
with `Some(...)` or no value with `None`.

```raven
func FindEven(numbers: int[]) -> Option<int> {
    for number in numbers {
        if number % 2 == 0 {
            return Some(number)
        }
    }

    return None
}
```

The return type tells the caller that absence is possible. A `match` handles
both possibilities:

```raven
match FindEven([1, 3, 4, 7]) {
    Some(let number) => WriteLine("Found $number")
    None => WriteLine("No even number found")
}
```

## Operations that may fail

`Result<T, E>` represents either success with `Ok(...)` or an expected problem
with `Error(...)`.

```raven
func Divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        return Error("Cannot divide by zero")
    }

    return Ok(a / b)
}
```

```raven
match Divide(10, 0) {
    Ok(let answer) => WriteLine("Answer: $answer")
    Error(let message) => WriteLine("Problem: $message")
}
```

The function's type makes the possible problem visible to its caller.

## Passing behavior to functions

Functions are also values. One function can receive another function:

```raven
func Transform(value: int, operation: (int) -> int) -> int {
    return operation(value)
}

let doubled = Transform(5, number => number * 2)
let squared = Transform(5, number => number * number)
```

`(int) -> int` means a function that receives an integer and returns an integer.
This lets programs customize behavior without creating a class containing one
method.

## When to use a class

Classes are an important part of Raven. Use one when something has identity,
owns changing state, controls a resource lifecycle, protects internal details,
or participates in object-oriented polymorphism.

```raven
class ScoreBoard {
    var Score: int = 0

    func Add(points: int) -> () {
        Score = Score + points
    }
}

let board = ScoreBoard()
board.Add(10)
```

The score board owns changing state, so a class is a natural model. A standalone
calculation such as `Add(a, b)` does not need an object merely to contain it.

Functional and object-oriented programming are not rival modes in Raven. They
are tools that can be combined in one application.

## A small complete program

```raven
import System.Console.*

record class Student(val Name: string, val Score: int)

union Grade {
    case Passed(score: int)
    case Failed(score: int)
}

func GradeStudent(student: Student) -> Grade {
    if student.Score >= 60 {
        return .Passed(student.Score)
    }

    return .Failed(student.Score)
}

func Describe(student: Student) -> string {
    return GradeStudent(student) match {
        .Passed(let score) => "${student.Name} passed with $score"
        .Failed(let score) => "${student.Name} needs another try ($score)"
    }
}

let students = [
    Student("Mira", 84),
    Student("Noah", 52),
    Student("Ari", 71)
]

for student in students {
    WriteLine(Describe(student))
}
```

The record models student data, the union models possible grades, functions hold
the grading rules, and the loop processes the collection. Each construct has a
clear job.

## How to continue learning

Do not try to memorize the whole language. Change the examples and observe what
happens:

1. Change names and numbers.
2. Add another student.
3. Change the passing score.
4. Add an `Excellent` grade case and update the match.
5. Write a function that calculates an average.

Compiler errors are part of learning. Read them, return to the smallest example
that demonstrates the problem, and make one change at a time.

Continue with:

- [Getting Started](getting-started.md) for compiler and project commands.
- [Language Introduction](introduction.md) for a broader tour.
- [Domain Modeling](lang/domain-modeling.md) for choosing among data and
  behavior shapes.
