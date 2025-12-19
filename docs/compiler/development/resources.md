# Resources

Here is a list of useful resources for when developing the compiler.

---

## Tools

Some helpful tools:

* [ILSpy](https://github.com/icsharpcode/ILSpy) - .NET de-compiler that can be used to inspect assemblies and turn instructions back into C#. Useful when trying to understand generated IL code, as well as when understanding the C# compiler. Since a lot of Roslyn's source code is generated and not available in the repository. There is also a VS Code plugin.

* [SharpLab](https://sharplab.io/) - Online C#/VB/F# compiler playground. You can view lowered code, IL, and syntax trees. Test the latest experimental C# features.

* [.NET Lab](https://lab.razor.fyi/) - Online compiler playground for .NET. Similar to SharpLab, but with additional support for Razor components and Pages.

* [Roslyn Quoter](https://roslynquoter.azurewebsites.net/) - Turns C# code into Roslyn syntax node definitions in C#. Useful as a reference when designing syntax nodes. Also for when working on our "Generator" project.

## Reference compilers

Here are some compilers whose source codes are worth having a look at:

* [Good for nothing compiler](https://github.com/johandanforth/good-for-nothing-compiler) - a simple compiler project in C#, originally by Joel Pobar and Joe Duffy, published in an article for MSDN Magazine.
* [Roslyn C# compiler](https://github.com/dotnet/roslyn) - the C# compiler and related services.
* [SmallBasic](https://github.com/sb/smallbasic-editor) - A Basic-like programming language and IDE meant to teach kids about programming. Built on C#/.NET.
* [IronPython3](https://github.com/IronLanguages/ironpython3) - An implementation of Python 3 for .NET. Source code C#.
* [Mono C# compiler](https://github.com/mono/mono/tree/main/mcs/mcs) - C# compiler from the Mono project - written in C# itself. 

## My old compiler projects

_In chronological order:_

* [C-Micro](https://github.com/marinasundstrom/cmicro-compiler) (2008-2009) - My first ever attempt at building a compiler. C-like language.
* [VB Lite compiler](https://github.com/marinasundstrom/vb-lite-compiler) (2011) - Compiler for a Visual Basic.NET-like language. Loosely-based on Mono C# compiler architecture.
* [EBNF Grammar parser](https://github.com/marinasundstrom/ebnf-parser-gen) (2014) - Incomplete parser generator for EBNF
* [ExpressionEvaluator](https://github.com/marinasundstrom/ExpressionEvaluator) (2016) - Expression parser, evaluator, and compiler. Using the operator-precendence parser algorithm and Reflection.Emit for code generation.
* [Compiler projects](https://github.com/marinasundstrom/compiler-projects) (2022 - 2023) - A couple of compiler projects for prototyping using a modern compiler architecture.
* Raven (2024)

## Other tools

_Other resources for compiler developers_

* [Compiler Explorer](https://godbolt.org/) - Compiles C/C+ code into target assembly. To explore other possible compile targets.
* [CPUlator ARMv7 System Simulator](https://cpulator.01xz.net/) - Online ARMv7 emulator

## Books

* [X86 Disassembly - Wikibooks](https://en.wikibooks.org/wiki/X86_Disassembly)