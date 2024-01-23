# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2023-05-11

Add user level unit tests and fix bugs found along the way.

NOTE:
This changelog entry was written some time after this version. Thus it is incomplete

### Added

- Cask files for dependency handling
- Buttercup test framework dependency
- Run script for buttercup unit tests
- User level unit tests
- Script to compile mode
- Package header & footer
- Internal message function that does not print when in unit tests

### Changed

- Use lexical scoping
- Move buffer local variables to top of file
- Centering is no longer a global setting
- Toggling centering triggers a centering of the current text on the active row

### Fixed

- Countless bugs found when implementing unit tests
- Fix compiler warnings

## [0.1.0] - 2023-01-30

All major features needed for release 1 are in place.

[0.0.2]: https://github.com/danielthoren/Block-Comment-Mode/compare/v0.1.0...v0.2.0
[0.2.0]: https://github.com/danielthoren/Block-Comment-Mode/releases/tag/v0.1.0
