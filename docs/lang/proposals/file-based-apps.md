# Proposal: File-based applications

> ℹ️ This proposal is under consideration

## Summary

Allow a Raven source file to be built, run, and eventually published without a
`.rvnproj` project file. The source file is the application root and may name
additional source files and build inputs through directives.

```raven
#!/usr/bin/env rvn
#:include "lib/**/*.rvn"
#:package Spectre.Console@1.0.0

Console.WriteLine("Hello from Raven")
```

```bash
rvn hello.rvn
rvn run hello.rvn -- Alice
rvn build hello.rvn
./hello.rvn
```

The proposed user-facing name is **file-based application**. The corresponding
compiler/tool mode may be called **source-file mode**. Raven should avoid
calling this a separate scripting language or script compilation mode: the
source uses ordinary Raven syntax, binding, diagnostics, lowering, and code
generation.

This proposal covers the intended experience and semantic boundaries. It does
not require every directive or command to ship in the first implementation.

## Motivation

Small programs, examples, command-line utilities, build helpers, and learning
material should not require a project file before they can be compiled and
executed. A source file should be able to grow gradually:

1. begin as one file with top-level statements;
2. include supporting source files;
3. add pinned package or project dependencies;
4. convert to a normal Raven project when project-level structure is useful.

Raven already has relevant building blocks:

* top-level statements and explicit `Main` functions;
* one-shot compilation of one or more source files in `rvnc`;
* framework and reference selection in the compiler driver;
* application emission and execution support in the compiler host;
* project-oriented `rvn build` and `rvn run` commands.

The missing surface is a stable `rvn` workflow that treats a source file as the
root of an ephemeral application, resolves its declared inputs, and invokes the
same compiler pipeline used by project builds.

## Goals

* Build and run one Raven source file without creating a `.rvnproj` file.
* Pass application arguments without confusing them with tool arguments.
* Allow a root file to include a deterministic tree of Raven source files.
* Preserve ordinary Raven compilation semantics across project-based and
  file-based applications.
* Support a Unix shebang so an executable Raven file can invoke `rvn` directly.
* Leave room for packages, project references, framework selection, publishing,
  restore, and project conversion.
* Give editors and the language server the same resolved source tree and
  compilation options as the command-line tool.

## Non-goals

* No interpreter, REPL, or persistent interactive session is introduced.
* No separate type system, name lookup rules, or relaxed diagnostics are
  introduced for file-based applications.
* File-based applications do not replace `.rvnproj` projects for larger builds.
* The first implementation does not need to support every MSBuild property or
  item type.
* A shebang is not a general command-execution directive inside Raven source.

## Command-line experience

### Run

`rvn run` accepts either a project file or a Raven source file:

```bash
rvn run app.rvn
rvn run app.rvn -- first second
rvn run App.rvnproj -- first second
```

Arguments after `--` belong to the application. Tool options occur before the
separator.

When the first argument to `rvn` is a Raven source-file path, it is shorthand
for `rvn run`:

```bash
rvn app.rvn -- first second
```

This shorthand is important for shebang support because a portable
`#!/usr/bin/env rvn` invocation supplies the script path directly to `rvn`.

### Build, publish, restore, and clean

The project-oriented commands should grow source-file overloads:

```bash
rvn build app.rvn
rvn publish app.rvn
rvn restore app.rvn
rvn clean app.rvn
```

`build` emits a normal managed application. `run` builds when necessary and
executes it. `publish` produces independently runnable artifacts according to
the file's configuration. `restore` resolves declared external dependencies.
`clean` removes cached build state for that root application.

`rvnc` remains the compiler driver. Dependency restore, application caching,
project conversion, and the user-facing run workflow belong to `rvn` and its
shared compiler-host services rather than to the compiler API.

### Project conversion

A later command should turn a file-based application into an ordinary project
without changing the original file:

```bash
rvn project convert app.rvn
```

The generated project should preserve source includes, pinned dependencies,
framework selection, and supported build properties.

## Application root

The source file passed to `rvn` is the **application root**. It determines:

* application identity and default assembly name;
* the directory from which root-relative build configuration is resolved;
* the implicit top-level entry point, when the file contains global statements;
* the initial set of file-based directives;
* the cache identity for restore and compilation.

Only the root file may contain global statements. Included files contribute
imports, aliases, namespace members, types, and other declarations, but a
global statement in an included file is diagnosed. This prevents include order
from changing the executable body.

An explicit `Main` declaration follows the normal compilation-wide entry-point
rules. It may be declared in an included file, although applications should
normally keep their entry point in the root file. Existing diagnostics continue
to handle multiple or conflicting entry points.

## File-based directives

File-based build directives use a `#:` prefix so they are visually and
syntactically distinct from compiler directives such as `#pragma` and from
macro syntax.

Directives must appear in a contiguous header before imports, aliases,
declarations, or global statements. A shebang, when present, precedes them.
Blank lines and comments may appear within the header.

The initial directive set should be deliberately small. The complete intended
namespace is:

```raven
#:include "lib/**/*.rvn"
#:package Spectre.Console@1.0.0
#:project "../Shared/Shared.rvnproj"
#:framework net10.0
#:property PublishAot=false
```

### `#:include`

`#:include` adds source files to the same compilation:

```raven
#:include "helpers.rvn"
#:include "models/**/*.rvn"
```

Rules:

* Literal paths and glob patterns are supported.
* Paths are resolved relative to the file containing the directive.
* Included files may contain further `#:include` directives.
* Matches are canonicalized, deduplicated, and ordered deterministically.
* An include cycle is diagnosed with the include chain.
* A literal include that does not exist is an error.
* A glob that matches no files should produce a warning by default.
* The root file is never added a second time through an include.
* Included sources are ordinary, separate syntax trees; inclusion is not
  textual preprocessing.

Because included files remain separate syntax trees, file-scoped semantics such
as `fileprivate`, source locations, diagnostics, and incremental document
identity remain meaningful.

### `#:package`

`#:package` adds a package dependency:

```raven
#:package Spectre.Console@1.0.0
```

Package directives are allowed only in the application root. Versions should
be explicit and reproducible. Floating versions, if supported, should require
an intentional syntax and should be discouraged by diagnostics or tooling.

Package restore is an `rvn` responsibility. The resolved assemblies are passed
to the existing compiler reference pipeline.

### `#:project`

`#:project` adds a reference to another Raven or compatible .NET project:

```raven
#:project "../Shared/Shared.rvnproj"
```

Project directives are allowed only in the application root. Project build and
reference resolution should use the .NET SDK/MSBuild boundary rather than
reimplementing project evaluation in the language compiler.

### `#:framework`

`#:framework` selects the target framework when the host default is not
appropriate:

```raven
#:framework net10.0
```

It is allowed only in the application root. An explicit command-line framework
option overrides the directive.

### `#:property`

`#:property` supplies a curated build property:

```raven
#:property PublishAot=false
```

The supported property set should be allowlisted. File-based directives should
not become an unrestricted mechanism for executing arbitrary MSBuild behavior.
Command-line options override corresponding source properties.

## Shebang

A file-based application may begin with a Unix shebang:

```raven
#!/usr/bin/env rvn

Console.WriteLine("Hello")
```

After the file is marked executable, a Unix-like system can launch it directly:

```bash
chmod +x hello.rvn
./hello.rvn
```

Lexical rules:

* `#!` is recognized as a shebang only on the first physical line of the root
  source file.
* The complete line is ignored by Raven compilation and preserved as trivia so
  source round-tripping retains it.
* `#!` elsewhere is not a shebang and is diagnosed through normal syntax
  recovery.
* Included files must not contain a shebang.
* Editors should classify the line as a shebang/comment rather than Raven code.

For direct Unix execution, `#` and `!` must be the first two bytes of the file.
A UTF-8 byte-order mark before the shebang prevents the operating system from
recognizing it. Raven tooling should avoid writing a BOM to executable source
files and should warn when a BOM precedes a shebang.

The portable form is `#!/usr/bin/env rvn`. Forms that depend on passing multiple
arguments through `env`, such as `#!/usr/bin/env -S rvn run`, should not be
required for the core experience.

On platforms that do not use shebangs, the line remains harmless compiler
trivia and the file can be launched with `rvn app.rvn`.

## Compilation and execution semantics

A file-based application produces an ordinary Raven `Compilation` containing
the root and all resolved included syntax trees. The compiler does not need a
file-based semantic mode.

The `rvn` frontend constructs an ephemeral project snapshot with:

* the resolved source-file set;
* the selected target framework and reference assemblies;
* restored package and project references;
* ordinary Raven parse and compilation options;
* the same generated prelude policy used by comparable Raven projects.

The compiler remains authoritative for syntax, binding, entry-point selection,
diagnostics, lowering, and emission. `rvn` is responsible for input discovery,
restore, caching, artifact layout, and process execution.

Build results should be cached using all inputs that can affect output,
including:

* canonical root path and source contents;
* the transitive include graph and contents;
* directive values and relevant command-line overrides;
* resolved package/project dependency state;
* target framework, compiler version, and compilation options.

Glob membership changes must invalidate the source graph even if existing file
contents are unchanged.

## Diagnostics

The feature should prefer stable Raven diagnostics over frontend exceptions.
At minimum, diagnostics are needed for:

* misplaced or malformed file-based directives;
* a shebang outside the root's first line;
* a shebang in an included file;
* global statements in an included file;
* missing literal includes and empty include globs;
* include cycles;
* invalid or duplicate package specifications;
* root-only directives used in included files;
* incompatible framework, package, or project references;
* unsupported or unsafe `#:property` names;
* restore and project-evaluation failures.

Diagnostics should identify both the directive location and, for graph errors,
the relevant include or dependency chain.

## Language service and editor behavior

Opening an application-root file should let the language server resolve its
directives and construct the same ephemeral project shape as `rvn`. Included
files must participate in diagnostics, navigation, completion, rename, and
other semantic features as normal documents.

The workspace owns source-graph refresh and document snapshots. The compiler
owns semantic caching and binding correctness. The language server should not
create a separate include-aware lookup path.

Changes to a directive, included file, glob membership, framework, or restored
dependency produce a new project snapshot. Unchanged documents and compiler
state may be reused according to the normal incremental compilation rules.

Editor support should include:

* highlighting and completion for file-based directives;
* path completion and go-to-definition for includes and project references;
* package identifier/version completion when a package source is available;
* clear indication of the application root for an included file;
* a command to run or debug the active file-based application.

## Artifact and security policy

Ordinary `run` and `build` operations should not scatter generated project files
or intermediate artifacts beside the source. Artifacts belong in a stable
cache or an explicit output directory. `publish` is the operation that creates
user-facing distributable output.

The frontend must treat directives as untrusted input:

* includes may read only paths permitted by the host environment;
* restore uses configured package sources and existing credential boundaries;
* `#:property` is allowlisted;
* merely opening a source file in an editor must not execute builds, targets,
  package scripts, or the program;
* project references that require evaluation should be resolved only through
  an explicit build/restore action or a suitably constrained design-time host.

## Implementation stages

### Stage 1: Single-file run and build

* Add `rvn run <file.rvn>` and `rvn build <file.rvn>`.
* Add `rvn <file.rvn>` as run shorthand.
* Pass application arguments after `--`.
* Use existing compiler-host compilation and execution facilities.
* Do not add source directives yet.

### Stage 2: Shebang

* Recognize and preserve a root-file shebang.
* Add syntax, formatting, and editor coverage.
* Support `#!/usr/bin/env rvn` end to end.

### Stage 3: Source trees

* Add `#:include` and deterministic transitive source-graph resolution.
* Enforce root/global-statement rules.
* Integrate the graph with workspace and language-server snapshots.

### Stage 4: Dependencies and configuration

* Add pinned `#:package`, `#:project`, `#:framework`, and curated
  `#:property` directives.
* Add restore, cache invalidation, and design-time reference resolution.

### Stage 5: Distribution and growth

* Add publish, clean, and project conversion workflows.
* Add run/debug support in editor integrations.

## Testing strategy

Each stage should add focused coverage at its owning layer:

* `rvn` command tests for dispatch, argument separation, exit codes, caching,
  and execution;
* syntax tests for shebang/directive recognition, trivia preservation,
  formatting, malformed input, and recovery;
* source-graph tests for relative paths, globs, deterministic ordering,
  deduplication, cycles, and graph invalidation;
* compiler tests for root versus included global statements and ordinary
  multi-file semantic behavior;
* integration tests for restore, framework selection, project references,
  emission, and observable runtime behavior;
* language-server tests for project discovery, graph refresh, diagnostics, and
  navigation across included files;
* platform tests for direct Unix shebang execution where supported.

Stable tests should assert diagnostics, symbols, metadata shape, and observable
behavior rather than emitted instruction sequences.

## Open questions

* Should `#:include` allow paths outside the root directory by default, or
  require an explicit opt-in?
* Should empty include globs be warnings or errors?
* Which `#:property` names belong in the initial allowlist?
* Should floating package versions be rejected, warned about, or recorded in a
  generated lock file?
* Should a directory containing exactly one file-based application be runnable
  as `rvn run`, or should source-file mode always require an explicit root?
* What artifact/cache location provides good cleanup behavior across platforms?
* Should project conversion preserve transitive includes as explicit project
  items or retain the root file's include directives?

## Alternatives considered

### Call the feature scripting mode

Rejected as the primary name. Raven source is compiled with normal language
semantics, and the same file can become part of a project. “Scripting” suggests
an interpreter or a distinct semantic mode that this proposal does not add.

### Infer all Raven files in the root directory

Rejected as the default. Implicit directory inclusion makes unrelated examples
interfere with one another and makes the application boundary unclear.
Explicit includes provide a reviewable and cacheable source graph.

### Concatenate included files before parsing

Rejected. Textual inclusion would damage source locations, file-scoped
semantics, editor identity, diagnostics, and incremental compilation. Included
files should remain separate syntax trees in one compilation.

### Put run and restore behavior in `rvnc`

Rejected. `rvnc` is the compiler driver, while `rvn` is the user-facing tool.
Restore, caching, project evaluation, publishing, conversion, and process
execution are frontend/application-host responsibilities.
