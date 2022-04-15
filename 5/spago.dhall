{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
