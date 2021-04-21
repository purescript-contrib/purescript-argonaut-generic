{ name = "argonaut-generic"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign-object"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
