{ name = "my-project"
, dependencies =
  [ "arrays", "console", "control", "effect", "integers", "prelude", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
