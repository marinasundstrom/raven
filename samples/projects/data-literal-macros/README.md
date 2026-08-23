# JSON and XML Literal Macros

This project demonstrates Raven's standard `json!` and `xml!` macros. They
embed small data-language DSLs in ordinary Raven code and expand to the .NET
platform object models:

- `json!` returns `System.Text.Json.Nodes.JsonObject`;
- `xml!` returns `System.Xml.Linq.XElement`.

The braces in `json! { ... }` are the JSON object's braces, so an additional
pair is unnecessary. The body of `xml! { ... }` contains one XML root element.

Both macros use Raven's normal interpolation spellings. `$name` inserts a
single identifier and `${expression}` inserts a larger expression. In a JSON
value position, the inserted value is serialized as JSON. In XML content,
LINQ to XML applies its normal value, escaping, and node semantics. The sample
therefore safely renders `Ada & Bob` as XML text containing `&amp;` while its
in-memory value remains `Ada & Bob`. Formatting whitespace used only to indent
the XML literal is not added as text content.

```raven
let payload = json! {
    "name": "$name",
    "age": $age,
    "nextAge": ${age + 1}
}

let element = xml! {
    <person age="$age">
        <name>$name</name>
        $status
    </person>
}
```

The macro bodies are also projected to JSON and XML editor services. Raven
expression splices remain Raven-owned fragments, so hover, completion, and
diagnostics inside them use the caller's semantic scope. The surrounding
literal is validated with the platform parser during compilation, preventing
malformed JSON or XML from becoming a runtime string-construction problem.

Run the sample after installing an SDK build that contains these macros:

```bash
dotnet run --project samples/projects/data-literal-macros/DataLiteralMacros.rvnproj \
  --property WarningLevel=0
```
