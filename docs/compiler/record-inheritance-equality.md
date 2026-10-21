# Record inheritance equality

Generated record-class typed equality and operators dispatch through virtual
Object.Equals after identity/null guards. The generated Object override checks exact
runtime type before component comparison. This prevents base-typed comparisons from
ignoring derived state or comparing sibling/root instances as equal. Inherited
positional properties participate in generated display/hash/equality, even when
not repeated in the derived primary parameter list. Value-record paths are unchanged.

This follows .NET record value equality across inheritance; Raven retains its text
quoting. It uses ordinary virtual calls rather than a second equality hierarchy.
No metadata ABI or Runtime Contract change. Native emission has its own selected-core
implementation and requires separate validation, not a whole-branch merge.

The failing emitted .NET baseline now passes through base-typed, interface and
Object dispatch, sibling/root rejection and inherited display. All 37 record
semantic tests also pass. No performance improvement is claimed.

Generated copy constructors mark required members initialized. An explicit derived
copy now compiles and retains both base and derived values; the 38-test focused
cohort includes this execution check.
