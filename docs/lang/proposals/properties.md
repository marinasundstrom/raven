# Proposal: Properties

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines the feature properties

## Purpose

Properties are outwards field-like constructs on objects - constructs that actually are accessor methods that are responsible for retrieving and setting the value. Due to being methods you can set access modifiers on each accessor individually. That is why some call them "smart fields".

## Syntax

### Full property declaration

```raven
public class Person {
    var name: string

    init(name: string) {
        self.name = name
    }

    public Name: string {
        get {
            return name // or self.name
        }
        set {
            self.name = value
        }
    }
}
```

And you access properties like this:

```raven
var person = Person("John Doe")

// Get a property value
let name = person.Name

// Set a property value
person.Name = "Jane Doe"
```


### Expression-bodied accessors

```raven
public class Person {
    var name: string = "N/A"

    public Name: string {
        get => name // or self.name
        set => self.name = value
    }
}
```

### Expression-bodied property declaration

You can define a property with just an expression body which will be equivalent to the `get` accessor.

```raven
public class Dog {
    let name: string = ""

    init(name: string) {
        self.name = name
    }

    public Name: string => name
}
```

Functionally equivalent to this expression-bodied `get` accessor:

```raven
/* Class declaration omitted */

public Name: string  { 
    get => name
}
```

Or this with a full `get` accessor:


```raven
/* Class declaration omitted */

public Name: string  { 
    get {
        return name // "return" is optional in blocks
    }
}
```

### Auto-properties

Auto-properties are mainly intended to make it easier to define "pure" properties that guard access to data, without the need for the ceremony.

They are properties with an auto-generated backing-field and auto-generated accessor methods.

```raven
public class Person {
    init(name: string) {
        self.Name = name
    }

    public Name: string {
        get
        private set
    }
}
```

An auto-property is recognised when every accessor omits an explicit body. The
compiler generates a hidden backing field and accessor bodies that simply read
and write that storage. Accessor modifiers (such as `private set`) continue to
govern accessibility, and marking the property `static` creates a shared backing
field on the type itself. Accessors remain optional—a get-only auto-property is
read-only and will expose the backing field's default value until it is assigned
internally (for example from a constructor).

> ℹ️ Auto-properties are available only on classes and structs; interface
> accessors remain abstract requirements.

### Static properties

Properties can be marked `static` to associate them with the type rather than an instance.

```raven
public class Logger {
    static var level: int = 0

    public static Level: int {
        get => level
        set => level = value
    }
}

Logger.Level = 3
```

Static properties are accessed through the type name and cannot be used on an instance.

### Element access operators (a.k.a. Indexers)

Indexers are a special kind of properties that give element access semantics to objects:

```raven
import System.Collections.Generic.List<>

public class Item {
    let roles: List<string>  = ["admin"]

    // Indexer: e.g., item[0]
    public self[index: int]: string {
        get => roles[index];
        set => roles[index] = value
    }
}
```

The indexer is accessed like so:

```raven
var item = Item()
let role1 = item[0]
item[0] = "cook"
```