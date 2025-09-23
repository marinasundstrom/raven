# Proposal: Import directive

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines the import directive.

## Syntax

The `import` directive appears at the top of a file, either outside or inside a namespace declaration.
Importing all types from a namespace requires a trailing `.*` wildcard.

### Import a specific type

```raven
import System.StringBuilder
```

### Import the types of an entire namespace

```raven
import System.*
```

### Import members of a type

Applying the wildcard to a type name brings its nested types and static
members into scope:

```raven
import System.Math.*

let pi = PI
```

### Resolution

Resolution works like this:

```raven
namespace System.Net

import Http.HttpClient // System.Net.Http.HttpClient
import Http.* // System.Net.Http.*

alias SB = System.StringBuilder // Aliases use the separate `alias` directive
```

## Questions

* Should we scope import directive to a scoped namespace?
