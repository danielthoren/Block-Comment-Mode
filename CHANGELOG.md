# Changelog

All notable changes to this project will be documented in this file.
The headings should be interpreted as follows:
- Added  : Features or other things that have been added and may affect the end user.
- Changed: Changes to features/interfaces that may affect the end user.
- Fixed  : Fixes within the project that should not affect the end user directly.
- Removed: Things taht have been removed that may affect the end user.

If a change affect the end user, said user may need to change their
integration of this package, or adapt their usage of it. An example is
if a external API is changed, then the user must change the way their
code calls upon this package (key bindings or the like). Another
example is if a feature within the package change, then the end user
may need to change how they use the package.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

### Changed

### Fixed

### Removed

## [1.0.0] - 2023-06-21

Documentation has been added to the whole project and adhere to the
GNU style convention. All pre-existing documentation has bee
re-written and all internal keywords have been overhauled and
documented. The external API has been simplified and documented and
internal functions & variables have been renamed to increase
readability.

NOTE: This changelog entry was written some time after this
version. Changes might have been left out or described incorrectly.

### Added

- Add doc string to all global/buffer-local variables
- Add doc string to all functions
- Add test case to 'user-new-line-tests'
- Add new function 'get-text-boundries'

### Changed

- Remove 'autoload' from all non-interactive functions
- Moved key map to its own variable with documentation
- Remove default style init on startup, instead resetting style if incomplete
- Merge similar functions:
  'block-comment--is-current-line-empty' | -> 'block-comment--is-empty-line'
  'block-comment--is-blank-line'         |
- Rename variables:
  * 'block-comment-centering--start-pos' -> 'block-comment-body-start-boundry'
  * 'block-comment-centering--end-pos' -> 'block-comment-body-end-boundry'
- Rename functions:
  * 'block-comment-centering--cursor-moved' -> 'block-comment--cursor-moved'
  * 'block-comment--init-comment-style' -> 'block-comment--set-comment-style'
  * 'block-comment-centering--edit' -> 'block-comment--user-edit'
  * 'block-comment-centering--removed-chars' -> 'block-comment--user-removed-chars'
  * 'block-comment-centering--inserted-chars' -> 'block-comment--user-inserted-chars'

### Fixed

- Optimize function 'block-comment--insert-newline' by removing redundant checks
- Use stored start/end boundaries instead of looking it up each time
- Made most global variables buffer local instead
- Improve header documentation
- Replace the word 'row' with the word 'line' in all documentation
- Format some doc strings to adhere to GNU coding standard

## [0.2.0] - 2023-05-11

First stable (unit tested) feature complete version. Documentation and
code structure is lacking, but all functionality work as expected.
This version adds user level unit tests and a large amount of bug
fixes.

NOTE: This changelog entry was written some time after this
version. Changes might have been left out or described incorrectly.

### Added

- Cask files for dependency handling
- Buttercup test framework dependency
- Run script for buttercup unit tests
- User level unit tests
- Script to compile mode
- Package header & footer
- Internal message function that does not print when in unit tests

### Changed

- Centering is no longer a global setting
- Toggling centering triggers a centering of the current text on the active row

### Fixed

- Move buffer local variables to top of file
- Use lexical scoping
- Countless bugs found when implementing unit tests
- Fix compiler warnings

## [0.1.0] - 2023-01-30

All major features needed for release 1 are in place. No unit tests
exist and the documentation is lacking.

[1.0.0]: https://github.com/danielthoren/Block-Comment-Mode/compare/v0.2.0...v1.0.0
[0.2.0]: https://github.com/danielthoren/Block-Comment-Mode/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/danielthoren/Block-Comment-Mode/releases/tag/v0.1.0
