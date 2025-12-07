# Proposal: Type syntax



## Summary

### Aliasing

```raven
type Year = int
```

Unlike `alias` this will only work with type definitions and the symbol will have its own identity and not be implicitly convertible to its aliased type.

### Complex types

```raven
type Year = int

type Person = {
    Name: string
    Nickname?: string
    YearOfBirth: Year
}
```

### Unions

Unified union syntax

#### Type unions

```raven
type NonNull = int | null
```

Also possible:

```raven
type Switch = "on" | "off"
```

#### Discriminate unions

```raven
type Option<T> = Some(value: T) 
                | None
```

Discriminated unions are backed by struct containing the cases. The value is decided by the payload and it can be anything.

Mixing custom cases with existing types:

```raven
type MyCases = Some(value: T) 
        | string
```

The compiler would emit a shim case and additional metadata to indicate that the `string` case is part of the union. And we would emit the appropriate `TryGet<Case>` method.