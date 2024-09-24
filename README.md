# view-patterns-snippet

Starting with the [simplest][simplest-example] aeson example, this snippet adds
echoing functions to compare the use of `-XViewPatterns` at the top-level and at
the case pattern level with using `-XPatternGuards` and using only case
expressions.

When upgrading to `aeson-2` and from `ghc-8.10.7` to `ghc-9.2.8` and
`ghc-9.4.5`, I found `-XViewPatterns` useful to introduce a shim and avoid
touching much of the implementation of functions broken by the `aeson` upgrade.

[simplest-example]: https://github.com/haskell/aeson/blob/master/examples/src/Simplest.hs