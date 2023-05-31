# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added `Deleted` char in flags column.
- Added `expunge` command via keybind `e`.

### Fixed

- Fixed `Answered` flag not set when sending an email from another folder than `INBOX`.

## [0.3] - 2023-02-09

The [0.2] has been reverted due to unintentional early merge, so the [0.3] is just a bump to avoid conflicts.

## [0.2] - 2023-01-09

### Added

- Included code from a [fork](https://git.sr.ht/~soywod/himalaya-emacs) [#15].
- Added possibility to mark multiple emails. Compatible actions: attachments, copy, move and delete. The implementation was inspired by the [tablist](https://github.com/politza/tablist) package) [#17].

### Changed

- Adjusted code for Himalaya CLI [v0.7.0](https://github.com/soywod/himalaya/pull/433).

### Fixed

- Fixed `nil` subjects in read buffers.
- Fixed `Answered` flag not set after replying to an email.

## [0.1] - 2022-10-09

First release added to the [MELPA](https://github.com/melpa/melpa/pull/7952) repository.

[unreleased]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.3...HEAD
[0.3]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.2...v0.3
[0.2]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.1...v0.2
[0.1]: https://github.com/dantecatalfamo/himalaya-emacs/compare/init...v0.1

[#15]: https://github.com/dantecatalfamo/himalaya-emacs/pull/15
[#17]: https://github.com/dantecatalfamo/himalaya-emacs/pull/17
