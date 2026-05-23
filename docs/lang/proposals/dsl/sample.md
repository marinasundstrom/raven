## Sample

Here’s a concrete “small UI app” example that uses normal Raven calls with
defaulted parameters, trailing blocks for children, and `key:` metadata, plus a
couple of re-renders worth of state.

```raven
import System.*
import System.Collections.Generic.*

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
            Window(title: "Todos") {

                StackPanel(spacing: 12.0) {

                    // Header row
                    StackPanel(orientation: .Horizontal, spacing: 10.0) {
                        Label("Raven Todos")
                        Button(_showDone ? "Hide done" : "Show done") {
                            ToggleShowDone()
                        }
                    }

                    // Input row
                    StackPanel(orientation: .Horizontal, spacing: 8.0) {
                        TextBox(placeholder: "New todo...", key: "newTodoEntry")
                        Button("Add") {
                            // In a real app you’d read TextBox.Text via a Ref/Binding;
                            // leaving as a placeholder for now.
                            AddTodo("New item")
                        }
                    }

                    // List
                    StackPanel(spacing: 6.0) {
                        for todo in _todos {
                            if _showDone || !todo.Done {
                                TodoRow(todo: todo)
                            }
                        }
                    }

                    // Footer
                    Label($"Total: {_todos.Count}")
                }
            }
        }
    }

    // “Component function” returning a node (still pure description)
    func TodoRow(todo: Todo) -> ViewNode {
        return StackPanel(orientation: .Horizontal, spacing: 10.0, key: todo.Id) {
            CheckBox(isChecked: todo.Done) {
                ToggleTodo(todo.Id)
            }
            Label(todo.Title)
        }
    }
}
```

A few notes baked into the example (matching your constraints):

* Uses normal Raven arguments and defaults (`StackPanel(spacing: 12.0)`).
* Uses wrapper functions over framework controls (`Window`, `StackPanel`, `Label`, `Button`, `TextBox`, `CheckBox`).
* Uses **trailing blocks** to express child hierarchies.
* Uses **`key:` metadata** for stable node identity (`todo.Id`, `"newTodoEntry"`).
* `TodoRow` is just a normal Raven function returning a `ViewNode` description, which fits builder rewriting via `BuildExpression` (or by already being `ViewNode`).
