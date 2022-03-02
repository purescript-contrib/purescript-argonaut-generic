# Argonaut Generic

[![CI](https://github.com/purescript-contrib/purescript-argonaut-generic/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-argonaut-generic/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut-generic.svg)](https://github.com/purescript-contrib/purescript-argonaut-generic/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut-generic/badge)](http://pursuit.purescript.org/packages/purescript-argonaut-generic)

This package provides `genericEncodeJson` and `genericDecodeJson` functions for any data types that have a `Generic.Rep` instance, which can be used to encode or decode `Json` values or to implement `EncodeJson` and `DecodeJson` instances for [`argonaut-codecs`](https://github.com/purescript-contrib/purescript-argonaut-codecs).

## Installation

Install `argonaut-generic` with [Spago](https://github.com/purescript/spago):

```sh
spago install argonaut-generic
```

## Quick start

We can use functions from the `Data.Argonaut.Decode.Generic.Rep` and `Data.Argonaut.Encode.Generic.Rep` modules to automatically write instances of `DecodeJson` and `EncodeJson` for our data types. The below example defines a recursive sum type, derives `Generic`, and then uses this library to write its decoding and encoding instances:

```purs
import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)

data Example
  = Either (Either String Example)
  | Record { foo :: Int, bar :: String }
  | Nested { foo :: { nested :: Int }, bar :: String }
  | Product Int Int Example

-- We first need to derive `Generic` for our type
derive instance genericExample :: Generic Example _

-- Necessary to eta-expand because the generic data type is recursive.
instance encodeJsonExample :: EncodeJson Example where
  encodeJson a = genericEncodeJson a

instance decodeJson :: DecodeJson Example where
  decodeJson a = genericDecodeJson a
```

You may also be interested in other libraries in the Argonaut ecosystem:

- [purescript-argonaut-core](https://github.com/purescript-contrib/purescript-argonaut-core) defines the `Json` type, along with basic parsing, printing, and folding functions
- [purescript-argonaut-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs) provides codecs based on `EncodeJson` and `DecodeJson` type classes, along with instances for common data types and combinators for encoding and decoding `Json` values.
- [purescript-argonaut-traversals](https://github.com/purescript-contrib/purescript-argonaut-traversals) provides prisms, traversals, and a zipper for working with nested `Json` structures.

## Documentation

`argonaut-generic` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-argonaut-generic).
2. Written documentation is kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-argonaut-generic/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `argonaut-generic` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-argonaut-generic/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
