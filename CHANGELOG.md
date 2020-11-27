# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

- Removed vestigial `EncodeRepFields` class and its remaining instance for  `Data.Generic.Rep.Product`.

New features:

Bugfixes:

Other improvements:

## [v6.0.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v6.0.0) - 2020-06-20

Updates to use [`argonaut-codecs` v7.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v7.0.0), which introduces typed decoder errors. These errors provide richer information for processing and printing error messages in JSON libraries.

## [v5.0.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v5.0.0) - 2019-05-10

Update major dependencies: `-argonaut-codecs`, `-argonaut-core`, and `-record` (@LiamGoodacre)

## [v4.0.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v4.0.0) - 2019-01-05

Support unwrapping single arg constructors & ignore values for nullary constructors (@LiamGoodacre)

## [v3.0.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v3.0.0) - 2018-11-12

Support ability to configure field names in the encoding used by `decodeRep`, instead of always using `"tag"` and `"values"` (@LiamGoodacre). Changes type class to use `decodeRepWith`.

## [v2.1.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v2.1.0) - 2018-11-09

Update dependencies (@LiamGoodacre) for codecs.

## [v2.0.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v2.0.0) - 2018-08-07

Library update for PureScript 0.12 compatibility (@CarstenKoenig)

## [v1.2.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v1.2.0) - 2017-07-19

- Added functions for encoding and decoding sums types with nullary constructors as string literals (@justinwoo)

## [v1.1.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v1.1.0) - 2017-07-13

- Added encoding and decoding for `purescript-generics-rep` (@rightfold)

## [v1.0.0](https://github.com/purescript-contrib/purescript-argonaut-generic/releases/tag/v1.0.0) - 2017-04-22

- Initial release
