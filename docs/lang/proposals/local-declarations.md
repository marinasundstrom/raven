# Proposal: Local declarations

> ⚠️ 🧩 This proposal has been partly implemented

Locals are names that values are bound to. Commonly referred to as "variables", there are many types of bindings, and the behavior of a variable is just one of them. The names might be 

## Anatomy

There is the identifier and the bound expression.

## ``let`` vs ``var`` declarations

Explanation:

* ``let`` is a name whose value can't be changed, but overridden.

* ``var`` is a variable in its true sense. It's value may change.

## Type inferences

If a local doesn't have an annotation then its type will be inferred from the bound expression.

This is part of a bigger feature.