# Project history

Raven began as an experimental compiler exploring a Roslyn-style architecture on the .NET runtime. Early development focused on building a structured syntax tree and adopting immutable nodes similar to C#'s Roslyn, enabling integration with existing .NET tooling.

## Key decisions and milestones

- **Roslyn-inspired design** – The project adopted Roslyn's compiler-as-a-service model. This decision likely leveraged proven patterns, making the compiler approachable for contributors familiar with C# tooling.
- **Code generation for syntax nodes** – Early on, a source generator parsed user-written partial classes and patched in implementations, requiring cross-file parsing to add members and visitor hooks. This became unwieldy and was replaced by a single XML definition that is loaded once and drives all generation in one pass, still using Roslyn to structure the output code. Automation keeps syntax definitions consistent as new constructs are added.
- **Emphasis on semantics and diagnostics** – The binder and semantic model were prototyped early to mirror Roslyn's flow-sensitive typing. Later, diagnostics were moved to an XML specification, reflecting a desire for maintainable, data-driven error reporting.
- **Type system experimentation** – Union and literal types were implemented along with flow-sensitive typing. These features show a focus on modern language ideas inspired by TypeScript and F#, exploring expressive type interactions on .NET.
- **Tooling and developer experience** – Recent work added a console editor prototype and Spectre-based diagnostic highlighting, suggesting an interest in making the compiler pleasant to use even at an experimental stage.
- **Exploration of language styles** – The language was pushed toward a purely expression-based style inspired by functional languages, though statements were later reintroduced to balance familiarity with expressiveness.
- **Limited AI involvement** – AI tooling assisted occasionally with brainstorming and minor snippets, but architecture and implementation choices were authored and reviewed manually.
- **Lessons from Checked Exceptions Analyzer** – Work on a parallel C# Checked Exceptions Analyzer informed an understanding of Roslyn APIs and behaviors, which in turn shaped Raven's approach to syntax and semantics.

Raven remains a learning project, evolving as new ideas in language design and compiler construction are explored.
