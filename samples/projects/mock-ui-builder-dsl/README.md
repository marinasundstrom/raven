# Mock UI Builder DSL (.rvnproj)

This sample demonstrates builder-backed trailing blocks with a tiny mock UI model.

The important shape is `[Builder<UiBuilder>]`: blocks collect child `UiNode` expressions, `if` expressions, and `for` loops into one node through `BuildExpression`, `BuildBlock`, `BuildOptional`, and `BuildArray`.

```raven
val view = Window(title: "Tasks") {
    VStack {
        Text("Inbox")

        Button(title: "Add") {
            "create-task"
        }
    }
}
```

The button handler is an ordinary trailing closure. The window and stack bodies are builder blocks.

## Build and run

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --framework net10.0 --property WarningLevel=0 -- MockUiBuilderDsl.rvnproj --run
```
