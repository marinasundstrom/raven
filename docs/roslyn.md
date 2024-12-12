# Roslyn

In this document we talk about "Roslyn" and this project's (Raven's) relationship to that project.

## Relationship with Roslyn

This project doesn't have any code dependency on Roslyn, but it is built on knowledge obtained from both using the product and reading it's source code, which is open-source.

We are a project enabled by the work of Microsoft and contributors in the community. So we should thank them.

## What is Project "Roslyn"?

### History

In the early days, the first versions of the C# compiler were crafted in C++ using tried-and-true methods of compiler construction.

However, a compiler written in C++ proved to be a bottleneck. It was restrictive due to its architecture, the accumulation of technical debt, and the limitations of the language itself. The C# team found it increasingly difficult to implement the features they envisioned for the language.

In response, Microsoft took a bold step by launching a project to completely rewrite its compilers using modern language features and design principles. Both the C# and Visual Basic .NET compilers were rewritten in C#, resulting in a suite of compilers with a fresh architecture designed to grow alongside the language.

This ambitious project was named "Roslyn," and the name stuck.

Rewriting a compiler in the very language it compiles—a process known as "bootstrapping"—is considered a significant milestone in the field of compiler development.

## Concepts

### Compiler as a service

Instead of the classical black box, Roslyn provides APIs for leveraging the compiler.

What used to be separate tooling is now integrated and exposed via APIs.

### Syntax tree

The syntax tree is exposed as an API, which is the same one used by the compiler. It allows the consumer to modify the tree, which is immutable, in a non-destructive manner.

A key implementation detail for Roslyn is the internal "Green" tree. This tree stores information about nodes and their children. While the external "Red" tree (the consumer API) keeps track of node-parent relationships. This allows for the Green tree nodes to be re-used as you transform the syntax tree. That way the syntax tree is memory efficient.

We are explaining the concepts in further details in other documents.

For more on the syntax tree, read [this](syntax-tree.md).

## Notable differences in this project

### Syntax tree fully in C#

Our Red syntax tree is fully defined as C# classes. There is no separate file defining any nodes in an XML format, or so.

### Use of source generator

We use a custom C# source generator instead of T4 templates to generate partial classes with properties and methods for the Red tree. This code that otherwise would be repetitive, time-consuming, and risky to implement and maintain.