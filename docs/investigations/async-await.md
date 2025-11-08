# Async/await investigation – test8 reboot

The previous async/await notes drifted away from the current goal, so this
investigation restarts with a single objective: **make `samples/test8.rav`
compile and run successfully**. The script exercises a generic async function
that awaits `Task.Delay` and returns the awaited value:

```swift
import System.Console.*
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

let x = await Test(42)

WriteLine(x)
```

## Failing behaviour today

* Compilation succeeds, but running the emitted assembly crashes while the async
  state machine reflects over its generic `Program` container before the backing
  `TypeBuilder` has been finalized. `ConstructedMethodSymbol.GetMethodInfo`
  escalates to `TypeBuilderImpl.ThrowIfNotCreated`, tearing down the runtime
  before the awaited value is printed.

## Investigation goals

1. **Keep constructed async builders on the Reflection.Emit surface.** Reuse the
   `MethodBuilder` handles recorded during emission instead of asking
   `TypeBuilder.GetMethods()` for instantiated async members.
2. **Prevent constructed async state machines from materializing early.** Delay
   `GetMethodInfo` requests until after the owning `TypeBuilder` is created or
   redirect them through cached builders when the type is still open.
3. **Exercise the generic async entry point end to end.** Add regression tests
   that compile `test8.rav`, execute it, and confirm the awaited value flows back
   to the top-level script without null builders or reflection crashes.

## Immediate plan of record

### Step 1 – Cache async state-machine builders for generic substitutions
* Extend `CodeGenerator.AddMemberBuilder`/`TryGetMemberBuilder` so the async
  state-machine creation path records `MoveNext`, hoisted-field, and constructor
  builders keyed by their owning definition plus substitution map.
* Teach `ConstructedMethodSymbol` and `SubstitutedMethodSymbol` to consult this
  cache before invoking `GetMethods()`, returning the cached `MethodBuilder`
  whenever the constructed receiver still lives on an open `TypeBuilder`.
* Verify the emitter reuses cached builders by logging a diagnostic (or unit
  test assertion) that the constructed async generic no longer touches
  `TypeBuilder.GetMethods`.

### Step 2 – Guard builder lookup against premature type creation
* Audit every async lowering and emission site that calls
  `GetMethodInfo(invokeConstructed, substitution)` while the state machine or its
  containing type is still under construction.
* Route these call sites through the cached builder path, or defer the lookup
  until `TypeGenerator.GetCompletedType` finalizes the type, so no constructed
  async member triggers `ThrowIfNotCreated`.
* Add unit tests around `ConstructedMethodSymbol.GetMethodInfo` to prove the
  fallback never executes when a cached builder exists.

### Step 3 – Runtime regression for `samples/test8.rav`
* Promote the script into the regression suite, compile it with the async
  investigation flags enabled, and execute the resulting assembly within the
  runtime harness.
* Assert that the program prints `42`, the awaited value survives the state
  machine, and no exceptions or pointer-instability logs appear.
* Capture the emitted IL (and, if helpful, pointer traces) as golden files so
  future changes can diff against the working baseline.

## Done when

* `samples/test8.rav` builds and runs via the CLI without hitting
  `TypeBuilderImpl.ThrowIfNotCreated` or dropping the awaited integer.
* Regression coverage locks in the cached-builder lookup and end-to-end runtime
  behaviour, preventing the crash from resurfacing.
