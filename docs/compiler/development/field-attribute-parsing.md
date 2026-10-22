# Field attributes and array suffixes

A newline followed by `[Attribute(...)]` starts the next declaration rather than
extending the preceding field type. The type parser checks both leading and
trailing line-break trivia and retains array rank lookahead, so multiline `[]`,
`[,]` and numeric fixed ranks keep their existing meaning. This general parser
repair was developed on main; neoCLR native layout tests exposed it. No target
mapping or metadata format changes. The focused parser group passes 49 tests.
