{ name = "argonaut-generic"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "assert"
  , "console"
  , "effect"
  , "exceptions"
  , "generics-rep"
  , "psci-support"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
