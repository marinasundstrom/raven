# Node generator

`NodeGenerator` builds most of the syntax tree infrastructure from XML descriptions.
It reads the `Model.xml`, `Tokens.xml`, and `NodeKinds.xml` files in
`src/Raven.CodeAnalysis/Syntax` and emits code for both the public
"red" nodes and the internal "green" nodes along with visitors,
rewriters, and factory helpers.

See the [generator specification](../architecture/generator-specification.md)
for a detailed description of the produced members and algorithms. The
structure of the XML files is described in the
[XML format specification](../../../tools/NodeGenerator/README.md).

## Running the generator

Regenerate syntax nodes whenever the XML model or generator code changes:

```bash
cd src/Raven.CodeAnalysis/Syntax
# add `-f` to force regeneration
dotnet run --project ../../../tools/NodeGenerator -- -f
```

