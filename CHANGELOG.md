# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.1] - 2021-07-21
### Changed
- now using `keys(keywords)` instead of `keywords.itr` because of deprecation warning

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
