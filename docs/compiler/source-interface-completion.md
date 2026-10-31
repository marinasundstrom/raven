# Source interface declaration completion

Source interface relationships and members can be queried while declarations in other
files are still being bound. Source interface closures and constructed source views must
not retain those incomplete results. They now use the existing
`Compilation.SourceDeclarationsComplete` publication boundary before caching; the
existing mutable source-union behavior is retained. Completed declarations and metadata
symbols keep their normal caches. No performance improvement is claimed.

Bodyless interface indexers infer abstract accessors, as ordinary interface properties
already do. These are shared semantic fixes, independent of target-specific emission.
`SourceInterfaceCompletionTests` compiles a three-level generic interface hierarchy and
executes inherited property/indexer calls on .NET in both source orders (result 42).
The focused interface declaration/completion and interface-symbol tests pass (29 cases).
