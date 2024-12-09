# Syntax

## Top-level statements

```c#
import System;

Console.WriteLine("");
```

## Namespace declaration

```c#
namespace Foo;

import System;
import System.IO;

// Members here
```

### Scoped namespaces

```c#
namespace Foo 
{
    import System;
    import System.IO;

    // Members here
}
```

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