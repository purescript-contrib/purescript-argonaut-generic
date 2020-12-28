{ name = "argonaut-generic"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "assert"
  , "console"
  , "effect"
  , "exceptions"
  , "prelude"
  , "psci-support"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
