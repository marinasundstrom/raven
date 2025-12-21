# Operator overloading support: implementation plan

## Goals
* Allow declaring user-defined operator overloads with syntax such as `public static operator + (a: int, b: int) -> int { }` where the declaration node is represented by `OperatorDeclarationSyntax` deriving from `BaseMethodDeclarationSyntax`.
* Enable consuming those overloads from expressions (binary, unary, equality, etc.) with proper overload resolution, diagnostics, and code generation.
* Keep the language specification and compiler implementation aligned so future spec updates can draw from the work items here.

## Non-goals
* Implementing implicit/explicit conversion operators (tracked separately in existing drafts).
* Designing new operators; this plan focuses on overloadability for operators already in the language.

## Step-by-step plan
1. **Syntax surface and tokens**
   * Add an `operator` contextual keyword/token plus per-operator tokens as needed (e.g., keyword plus following `+`, `-`, `*`, `/`, `==`, `!=`, unary tokens).
   * Introduce `OperatorDeclarationSyntax : BaseMethodDeclarationSyntax` in the syntax model (update `Model.xml`, `NodeKinds.xml`, and generators) with slots for the operator token, parameter list, return type clause, body, and optional expression body.
   * Extend parsing in `Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser` (and the extension declaration parser) to recognize operator members, consume modifiers (require `static`), parse the operator token, parameters, arrow/type, and body/terminator. Add recovery for malformed operator tokens or incorrect arity.
   * Update `SyntaxFacts`, normalizer, and quoter/printer to round-trip operator declarations and ensure trivia/formatting is stable.

2. **Symbols and method metadata**
   * Extend symbol creation to produce `SourceMethodSymbol` instances with `MethodKind.UserDefinedOperator`, ensuring they are always static and non-generic and track the operator token/metadata name (`op_Addition`, etc.).
   * Add a helper mapping from syntax tokens to metadata names (replacing or expanding `GetOperatorMethodName` in `BlockBinder`) that can be reused by declaration binding and invocation.
   * Validate constraints during symbol creation (exact parameter counts for unary vs. binary, ref/out restrictions, no `params`/type parameters, required accessibility rules) and surface diagnostics for invalid operator signatures.

3. **Declaration binding and lookup**
   * Update `TypeMemberBinder`/`DeclarationTable` to include operator members when walking type syntax, producing operator symbols alongside methods/constructors.
   * Ensure operators participate in member lookup via their metadata names (e.g., `op_Addition`) and that overload sets are disambiguated by parameter types.
   * For extensions, allow (or explicitly reject) operator declarations according to the language design and wire extension operator lookup so consuming sites can see them when the receiver type matches.

4. **Overload resolution for consumption**
   * Expand binary/unary binding in `BlockBinder` to gather operator candidates from both operand types and applicable extensions, using the operator metadata name and enforcing static binding rules.
   * Replace the current early-exit user-defined operator resolution with a proper overload-resolution pass: build candidate sets, classify implicit conversions for each operand, apply tie-breakers consistent with method overload rules, and produce ambiguity diagnostics when needed.
   * Integrate nullable/literal unwrapping and numeric promotions so operators declared on underlying types can be selected even when operands are literals or nullable wrappers.
   * Ensure equality/inequality and logical operators follow any required lifted behavior or short-circuit semantics once an operator is chosen.

5. **Binding results and lowering**
   * Represent successful operator bindings as `BoundInvocationExpression` nodes targeting the resolved operator symbol, and ensure bound tree rewrites (constant folding, nullability flow, async/iterator analysis) respect these nodes.
   * Adjust any lowering or rewriting that currently assumes operators map directly to `BoundBinaryOperator`/`BoundUnaryOperator` so user-defined operators remain intact through code generation.

6. **Code generation**
   * Teach `MethodGenerator`/`MethodBodyGenerator` to emit methods with operator metadata names and attributes (e.g., `SpecialName`/`RTSpecialName` where required by CLI).
   * Update invocation emission so calls to operator symbols (especially lifted from extensions) produce the same IL as normal method calls with the synthesized name.
   * Add coverage for debug information and sequence points around operator bodies and call sites.

7. **Diagnostics and IDE surface**
   * Add diagnostics for duplicate operator definitions, missing `static`, invalid parameter counts, use on unsupported types, or ambiguous resolution at call sites.
   * Update semantic model query paths (symbol lookup, `GetDeclaredSymbol`, `GetSymbolInfo`) so operator declarations and usages surface meaningful symbols/tooling experiences.

8. **Documentation and tests**
   * Update `docs/lang/spec/language-specification.md` and related grammar sections to describe operator declaration/overload rules once implementation details settle.
   * Add compiler unit tests covering parsing, binding (valid/invalid signatures), overload resolution precedence, extension operator visibility, nullable/literal interactions, and codegen IL verification.
   * Include end-to-end samples demonstrating declaration and consumption of operators in classes and extensions.
