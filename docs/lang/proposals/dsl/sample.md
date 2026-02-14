## Sample

Here’s a concrete “small MAUI-ish app” example that uses the **framework-structure DSL** (named args only, trailing blocks for children, `Key:` metadata), plus a couple of re-renders worth of state.

```raven
import System.*
import System.Collections.Generic.*
import Microsoft.Maui.Controls.*

record Todo(Id: int, Title: string, Done: bool)

class TodoApp {

    // App state (in a real app this would live in a view-model / store)
    private var _showDone: bool = true
    private var _todos: List<Todo> = [
        Todo(1, "Buy milk", false),
        Todo(2, "Ship Raven DSL", true),
        Todo(3, "Walk outside", false),
    ]

    func ToggleShowDone() {
        _showDone = !_showDone
        Invalidate()
    }

    func ToggleTodo(id: int) {
        _todos = _todos
            .Select(t => t.Id == id ? Todo(t.Id, t.Title, !t.Done) : t)
            .ToList()
        Invalidate()
    }

    func AddTodo(title: string) {
        val nextId = _todos.Count == 0 ? 1 : _todos.Max(t => t.Id) + 1
        _todos.Add(Todo(nextId, title, false))
        Invalidate()
    }

    // Framework hook: tells runtime to re-render (your DSL runtime provides this)
    func Invalidate() {
        MauiRuntime.RequestRender(this)
    }

    // The root “render function” (builder block)
    func Render([Builder<MauiBuilder>] view: () -> ViewNode) -> View {
        return MauiRuntime.Render(view)
    }

    func Build() -> View {
        return Render {
            // Root window/page container (exact type can be whatever your adapter treats as root)
            ContentPage(Title: "Todos") {

                VerticalStackLayout(Spacing: 12, Padding: Thickness(16)) {

                    // Header row
                    HorizontalStackLayout(Spacing: 10) {
                        Label(Text: "Raven Todos", FontSize: 24)
                        Spacer() // if you have an adapter alias; otherwise remove
                        Button(
                            Text: _showDone ? "Hide done" : "Show done",
                            Clicked: () => ToggleShowDone()
                        )
                    }

                    // Input row
                    HorizontalStackLayout(Spacing: 8) {
                        Entry(Placeholder: "New todo...", Key: "newTodoEntry")
                        Button(
                            Text: "Add",
                            Clicked: () => {
                                // In a real app you’d read Entry.Text via a Ref/Binding;
                                // leaving as a placeholder for now.
                                AddTodo("New item")
                            }
                        )
                    }

                    // List
                    VerticalStackLayout(Spacing: 6) {
                        for todo in _todos {
                            if _showDone || !todo.Done {
                                TodoRow(todo: todo)
                            }
                        }
                    }

                    // Footer
                    Label(
                        Text: $"Total: {_todos.Count}",
                        Opacity: 0.6
                    )
                }
            }
        }
    }

    // “Component function” returning a node (still pure description)
    func TodoRow(todo: Todo) -> ViewNode {
        return HorizontalStackLayout(
            Spacing: 10,
            Key: todo.Id,                 // <-- key for stable reconciliation
            Opacity: todo.Done ? 0.5 : 1.0
        ) {
            CheckBox(
                IsChecked: todo.Done,
                CheckedChanged: () => ToggleTodo(todo.Id)
            )
            Label(Text: todo.Title, LineBreakMode: LineBreakMode.TailTruncation)
        }
    }
}
```

A few notes baked into the example (matching your constraints):

* **Named args only** everywhere (`Text:`, `Spacing:`, `Clicked:` etc.).
* Uses **framework types directly** (`ContentPage`, `VerticalStackLayout`, `Label`, `Button`, `Entry`, `CheckBox`).
* Uses **trailing blocks** to express child hierarchies.
* Uses **`Key:` metadata** for stable node identity (`todo.Id`, `"newTodoEntry"`).
* `TodoRow` is just a normal Raven function returning a `ViewNode` description, which fits builder rewriting via `BuildExpression` (or by already being `ViewNode`).