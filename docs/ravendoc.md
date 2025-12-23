# "RavenDoc" - Documentation generator

This is the document generator.

Raven supports two kinds of content: XML and Markdown. 

XML is structural, and used to generate presentation.
Markdown is focused on presentation.

RavenDoc focuses on the markdown.

## Documentation comments in Raven

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