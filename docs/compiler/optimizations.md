# Compiler optimizations

This document describes Raven's first optimization tier. It is an implementation
guide for compiler contributors and is intentionally outside the published
user-documentation site.

## Goals

Release optimization should produce smaller, more canonical, verifiable IL
without changing Raven semantics. The initial tier favors local rewrites whose
correctness is apparent from one expression or lowered block. This is especially
useful before ahead-of-time compilation, while remaining useful to the CLR JIT
and other .NET tooling.

The optimization priorities are:

1. preserve runtime behavior, evaluation order, exceptions, and disposal;
2. preserve valid metadata and verifiable IL;
3. retain meaningful portable-PDB sequence points;
4. remove obviously redundant expressions, statements, and branches;
5. leave global data-flow and target-specific optimization to later work.

Debug is the compatibility path. It remains the default and retains the
pre-optimization bound-tree and emission behavior.

## Selecting Release optimization

`CompilationOptions.OptimizationLevel` controls compiler optimization. New
compilations default to `OptimizationLevel.Debug`.

The compiler driver selects Release optimization with:

```text
rvnc input.rvn -o output.dll --configuration Release
```

For `.rvnproj` builds, the evaluated MSBuild `Optimize` property is authoritative
when it is explicitly present. Otherwise, the `Release` configuration selects
Release optimization and other configurations select Debug.

Both modes can emit portable PDBs. Release omits debug-only IL padding but keeps
sequence points on meaningful instructions.

## Pipeline

The optimizer runs after semantic lowering and before IL generation. The
Release-only coordinator applies specialized rewriters in this order:

1. `PatternOptimizer`
2. `BooleanExpressionOptimizer`
3. `ControlFlowOptimizer`
4. `UnreachableCodeOptimizer`
5. `BranchOptimizer`

Ordering is intentional. Expression and pattern simplification can expose
literal conditions; literal conditions can expose unreachable statements;
reachability cleanup can make branches adjacent; branch cleanup then canonicalizes
the remaining lowered control flow.

### Pattern algebra

`PatternOptimizer` implements mechanically equivalent identities:

- `_ and pattern` becomes `pattern`;
- `pattern and _` becomes `pattern`;
- `_ or pattern` becomes `_`;
- `not not pattern` becomes `pattern`.

The pass does not replace `pattern or _` because evaluating the left pattern can
bind designators or otherwise affect observable pattern evaluation.

### Boolean expressions

`BooleanExpressionOptimizer` handles built-in Boolean operators only. It folds
literal negation, short-circuit identities, literal equality and inequality,
and comparisons between a Boolean expression and a literal. Constants are
recognized through parentheses and identity conversions.

The pass preserves evaluation order. For example, `value && false` is not
replaced with `false`, because evaluating `value` may have side effects. User-defined
operators and lifted operators are not treated as built-in Boolean identities.

### Literal control flow

`ControlFlowOptimizer` selects `if` branches and conditional gotos when their
conditions have become Boolean constants. This is a structural bound-tree
rewrite; Debug continues to lower and emit the original shape.

### Reachability

`UnreachableCodeOptimizer` performs conservative reachability within each
lowered block. It follows fallthrough, direct gotos, conditional gotos, and
lowered break/continue targets. Statements after terminal transfers are removed
when no branch can reach them.

The analysis does not claim whole-method or exception-region knowledge. Authored
source labels are seeded as reachable, and transfers found in nested statements
are retained conservatively.

### Branch cleanup

`BranchOptimizer` removes an unconditional goto whose target label immediately
follows it. It also rewrites the local shape:

```text
conditional-goto fallthrough
goto target
fallthrough:
```

to an inverted conditional goto targeting `target`, followed by normal
fallthrough. The condition is still evaluated exactly once.

## Release emission adjustments

Release emission also:

- omits debug-only `nop` padding;
- avoids sequence-point-only entry-point padding while retaining portable-PDB
  mappings on meaningful instructions;
- avoids an unreachable match fallback when a final unguarded catch-all arm
  already covers all remaining inputs.

These adjustments are selected by the same `OptimizationLevel.Release` policy
as the bound-tree pipeline.

## Validation policy

Stable tests should prove observable behavior, diagnostics, metadata, or bound
rewriter behavior. They should not lock Raven to exact opcode sequences.

Changes to the optimization pipeline should normally validate:

- direct tests for the specialized rewriter, including cases that must remain
  unchanged;
- Debug and Release runtime behavior for side-effect-sensitive rewrites;
- representative Release sample compilation with IL verification;
- the native-AOT smoke test when lowered control flow or emission changes;
- portable-PDB tests when sequence points or debug padding are affected.

## Deferred work

The MVP intentionally does not include arithmetic constant folding, dead-store
elimination, copy propagation, common-subexpression elimination, inlining,
loop optimization, or general cross-block data-flow analysis. Those features
need stronger overflow, exception, aliasing, lifetime, and side-effect models,
and should be justified by emitted-shape measurements or profiles.
