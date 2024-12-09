# Syntax

## Top-level statements

The program starts by writing some statements in a file that becomes its de-facto entrypoint.

```c#
import System;

Console.WriteLine("Hello, World!");
```

## Namespace declaration

Each file may define a namespace like so:

```c#
namespace Foo;

import System;
import System.IO;

// Members here
```

### Scoped namespaces

You may define multiple namespaces, even nested ones, in one single file by defining block scopes:

```c#

// Members here

namespace A1
{
    import System;
    import System.IO;

    // Members here

    namespace B1
    {
        // Members here
    }
}

namespace A.B
{
    // Members here
}
```

The outermost undeclared namespace is the global namespace. Meaning whatever is declared there has no prefixed namespace

## Method declaration

```c#
int Foo(int a, int b) 
{
    // Body here
}
```

## Local declaration

```c#
let x = "Foo";
```

```c#
let x : int = 2;
```

```c#
let a : int = 2, b : string = "";
```

## Block

```c#
{
    // Body here
}
```