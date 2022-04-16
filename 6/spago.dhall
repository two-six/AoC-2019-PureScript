{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "free"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "strings"
  , "transformers"
  , "tree-rose"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
