# Workspace API

This API is under development.

---

The purpose of the Workspace API is to provide an abstraction of a project system on-top of the compiler APIs. Instead of dealing with syntax trees and compilations, you deal with workspaces, solutions, projects, and documents.

A `Document` maps to an instance of `SourceText` (and a `SyntaxTree`), and  `Project` maps to a `Compilation`.

## Design goals
* Immutable API
* Internal state objects (similar to Green tree for syntax)
* Lazy-loading

