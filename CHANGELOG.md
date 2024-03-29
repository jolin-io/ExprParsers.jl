# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.2.3] - 2023-07-13
### Fixed
- `EP.Indexed` failed if index was not nested

## [1.2.2] - 2023-07-12
### Fixed
- `to_expr(::EP.TypeRange)` now recursively calls `to_expr`

## [1.2.1] - 2023-07-01
### Changed
- removed Compat dependency

## [1.2.0] - 2022-04-09

### Added
- all Parsers now also have `Base.hash` defined in addition to `Base.:(==)`
- defined `to_expr` for `TypeVar` for convenience

### Changed
- updated dependency `StructEquality` to version 2.0
- supported Julia versions is now 1.6 or higher because 1.6 is the new long term release

## [1.1.0] - 2021-07-21
### Fixed
- Julia 1.0 seems to be failing, Compat entry is now correctly set to julia 1.1. (The community guidelines says that Compat entry updates are breaking. As it hasn't worked before, this is not breaking, but for auto-merging we increase the version nevertheless.)
- now using `keys(keywords)` instead of `keywords.itr` because of deprecation warning
- updated TagBot and CompatHelper

## [1.0.0] - 2020-08-05
### Added
- extensive docstring documentation
- extensive example manual on how to use ExprParsers in practice
- GithubActions for CI, Codecov
- Changelog
- License
- added broadcasting for ExprParsers
- added `Base.getindex` for Named in order to enable `named[]` as a shorter and maybe more intuitive alternative to `named.value`.

### Changed
- minimized exports to only export `EP`, `@passert`, `parse_expr`, `to_expr`
- switched dependency to ProxyInterfaces (plural) instead of deprecated ProxyInterface (singular)
- `EP.Call` now collects all keyword arguments into field `kwargs`, including those given on the args-site like `f(a, b, c=1)`

### Fixed
- `EP.Signature` no longer skips the first argument in anonymous function signatures
- `EP.Args` now also applies `default`-sub-parser to `nodefault` value.

## [0.1.0] - 2020-05-01
initial release
