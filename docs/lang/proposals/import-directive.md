# Proposal: Import directive

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines the import directive.

## Syntax

The `import` directive appears at the top of a file, either outside or inside a namespace declaration.

### Import a specific type

```raven
import System.StringBuilder
```

### Import the types of an entire namespace

```raven
import System.*
```

### Aliasing

```raven
import SB = System.StringBuilder
```

The name `SB` will be an alias for `System.StringBuilder`.

The type aliased is required to be specified in fully qualified form (`System.StringBuilder`). This is to resolve ambiguities.

### Resolution

Resolution works like this:

```raven
namespace System.Net

import Http.HttpClient // System.Net.Http.HttpClient
import Http.* // System.Net.Http.*

import SB = System.StringBuilder // System.StringBuilder
```

## Questions

* Should we scope import directive to a scoped namespace?