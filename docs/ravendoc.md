# "RavenDoc" - Documentation generator

This is the document generator.

## For whom is RavenDoc intended?

RavenDoc is intended for those whose need for documentation is sufficed by placing markdown within the source code.

## Documentation comments in Raven

Raven supports two kinds of content: XML and Markdown. 

* XML is structural and keeps data, which then is used by a third-party tool (such as DocFx) to generate something presentable.

* Markdown is presentation-focused in itself. The comment is representing what is presented.

RavenDoc focuses on the markdown.

### Defined in syntax

Documentation comments are attached to declaration syntaxes as leading trivia:

```
/// ## Hello
/// 
/// ** Test **
public func Foo() { }
```

### Loaded from symbols

You can retrieve documentation comments via symbols - both those defined in source and those next to metadata:

```c#
    var comment = symbol.GetDocumentationComment();

    var content = comment?.Content; // Without "///"
    var rawContent = comment?.RawContent; // With "///"
```

## Current state

RavenDoc is in an early stage. It's not by itself a re-usable tool, not without re-compiling. The layout is fixed. You can't add additional content.