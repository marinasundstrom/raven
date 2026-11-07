# Partial sealed hierarchy declaration completion

Closed bound-node families need generated `permits` declarations separate from
handwritten roots. Partial declarations aggregate the presence of explicit
permits instead of letting a later part without a clause erase that contract.

The semantic model queues direct-subtype validation with its declaration work.
Compilation declaration completion drains those checks after every source model
has established its inheritance shapes. Local declarations discovered afterward
validate immediately. Public symbol queries and diagnostics use the same
completion path; consumers need no cache-specific APIs.

Validation: 72 regression cases cover class, record class and interface roots,
every ordering of three files, cold symbol queries and diagnostics-first queries,
on .NET and the neoCLR CLI configuration. Together with existing hierarchy,
constraint, diagnostic and metadata tests, 140 tests passed on .NET 11 using an
isolated compiler build. The neoCLR cases validate shared declaration semantics;
they do not demonstrate native neoCLR execution. No grammar change is required.
