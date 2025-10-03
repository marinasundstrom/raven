# Common Intermediate Language

CIL, for short, is the intermediate bytecode that C# is compiled into, a format that is platform-independent and portable. It is Just-in-Time (JIT) compiled by the Common Language Runtime (CLR).

## Accessing instance members

There is a difference in how you access members of reference types and value types.

Reference types are always treated as references, so they are passed as addresses by default. Loading a local variable with `ldloc` will load the address since that is what is stored in the local slot.

But with value types, the actual value is stored in the locals, so you need to explicitly load its address using the `ldloca` op code when for instance calling an instance method.

## Inspecting emitted IL

When you need to verify which instructions the Raven compiler produces you can inspect the generated assemblies with the same tools used for C#.

1. **Compile a Raven source file**
   ```bash
   dotnet run --project src/Raven.Compiler -- path/to/file.rvn -o ./out/MyProgram.dll
   ```
   The `-o` switch lets you control where the compiled assembly is written so you can disassemble it afterwards.

2. **Use ILSpy (recommended)** – Install the command-line decompiler once:
   ```bash
   dotnet tool install --global ilspycmd
   ```
   Then dump the IL for a specific method:
   ```bash
   ilspycmd --il --type Namespace.TypeName --member MethodName ./out/MyProgram.dll
   ```
   You can omit the `--type`/`--member` switches to print every method, or use the graphical ILSpy application for interactive browsing.

3. **Alternative disassemblers** – `ildasm` (ships with the .NET SDK) and `dotnet ildasm` provide similar views of the IL stream. Run `dotnet ildasm ./out/MyProgram.dll` to open the GUI, or `ildasm /text ./out/MyProgram.dll > MyProgram.il` to write the textual representation for diffing.

4. **Online experiments** – For small snippets you can compare Raven’s output with C# by pasting equivalent code into [SharpLab](https://sharplab.io) and switching the right-hand pane to “IL”. This is useful when you want to confirm the general shape of the instructions without compiling locally.

5. **Automated checks in tests** – Unit tests under `test/Raven.CodeAnalysis.Tests` can assert on emitted IL by compiling snippets with the testing helpers and inspecting the resulting method bodies. This is especially handy when you need repeatable verification for regressions.

These techniques make it straightforward to capture and reason about the exact instructions Raven emits, whether you are debugging code generation or validating a new lowering.