{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
