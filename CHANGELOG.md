# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0] - 2026-06-03

### Added

- Added support for himalaya CLI 2.0.
- Added `M-x himalaya-search-envelopes` (filter + sort using the v2 search DSL), bound to `C-c C-s` in the envelope listing.
- Added `himalaya-from` defcustom to pre-fill the `From:` header in compose buffers.
- Added MML composition in the write buffer: `<#part ...>` directives are compiled to MIME via `mml-to-mime` before the message is piped to `himalaya message send`.

### Changed

- Targeted himalaya CLI 2.0 wire contract: global flag is `--json` (replaces `-o json`); subcommands use `mailbox` (replaces `folder`) and the `--mailbox` / `-m` flag (replaces `--folder`); flag CLI is `flag add -m <NAME> -f <FLAG> <ids>` (split flag and ids); message `copy` / `move` use `--from` / `--to`.
- Renamed `Folder` to `Mailbox` everywhere: `himalaya-folder` defvar, `himalaya-switch-folder`, mode-line label, internal helpers and prompts.
- Renamed `himalaya-folder.el` to `himalaya-mailbox.el`.
- Rewrote envelope JSON parsing for the v2 shape: top-level object wrappers (`{"envelopes":[...]}`, `{"mailboxes":[...]}`, `{"accounts":[...]}`), `Address.email` (replaces `Address.addr`), `Flag` objects (`{raw, iana}`) replacing bare flag strings, and the kebab-cased `has-attachment` field.
- Rewrote message reading: fetch raw RFC 5322 bytes via `himalaya message read --raw`, then decode headers and the text/plain body in Elisp (via Gnus `mm-decode`) for the plain view.
- Rewrote compose, reply and forward to build the buffer directly in Emacs using `message-mode` instead of fetching a template from the CLI. Sending now compiles the buffer through `mml-to-mime` and pipes the result to `himalaya message send` over stdin.

### Removed

- Removed the `e` (expunge mailbox) keybind: v2 dropped shared `folder expunge`.
- Removed the `D` (delete messages) keybind: v2 dropped shared `message delete`.
- Removed the `h` (HTML view) keybind: v2 dropped `message export --destination`.
- Removed the `--preview` argument on read: on the IMAP backend, v2 always uses `BODY.PEEK` so reading never auto-sets `\Seen`.
- Removed `himalaya-template.el` and the `himalaya template …` integration: v2 dropped the `template` subcommand family.

## [1.0] - 2023-02-09

### Added

- Added `Deleted` char in flags column.
- Added `himalaya-config-path` option to override the default configuration file path.
- Added `e` envelope listing keybind to expunge the current folder.
- Added `C-c +` envelope listing keybind to add a flag to marked envelopes (or envelope at point) [#22]. 
- Added `C-c -` envelope listing keybind to add a flag to marked envelopes (or envelope at point) [#22].
- Added preview support for `RET` envelope listing keybind using the universal argument, which allow you to preview a message without applying the "Seen" flag.

### Changed

- Adjusted API to match Himalaya CLI `v1.0.0`.
- Renamed option `himalaya-email-order` to `himalaya-list-envelopes-order`.
- Renamed option `himalaya-page-size` to `himalaya-list-envelopes-page-size`.
- Reduced default envelopes listing page size from `50` to `25`.
- Made all CLI calls async (except for envelopes listing due to table issue).
- Moved code into domain-specific files (account, folder, envelope, message, flag, template, attachment).
- Renamed functions containing `email` by either `envelope` or `message`, depending on the domain they belong to.

### Fixed

- Fixed `Answered` flag not set when sending an email from another folder than `INBOX`.

### Removed

- Removed option `himalaya-default-account`.
- Removed option `himalaya-default-folder`.

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

[Unreleased]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v2.0...HEAD
[2.0]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v1.0...v2.0
[1.0]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.3...v1.0
[0.3]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.2...v0.3
[0.2]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.1...v0.2
[0.1]: https://github.com/dantecatalfamo/himalaya-emacs/compare/init...v0.1

[#15]: https://github.com/dantecatalfamo/himalaya-emacs/pull/15
[#17]: https://github.com/dantecatalfamo/himalaya-emacs/pull/17
[#22]: https://github.com/dantecatalfamo/himalaya-emacs/issues/22
