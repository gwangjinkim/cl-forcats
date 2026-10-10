# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Breaking
- `parity-FC1`: With explicit maintainer approval, FCT-COUNT returns a tibble
  with f/n/optional p columns and FCT-UNIQUE returns a factor containing all
  levels and observed implicit missingness. Replace plist/list consumption
  with the shared column protocol; migration examples are in README.

### Tests
- `parity-FC1`: All 109 package checks and 135 pinned R cases pass (77 and
  six before). Tests cover empty/NA values, factor owners, weights, numeric
  ordering, names, combination errors and approved inspection migrations.
- `parity-X2`: 33 new checks cover factors, orderedness and every factor
  cast boundary; all 77 checks pass (44 before).

### Added
- `parity-FC1`: Add nine creation, conversion, combination, level-union,
  ordering and matching APIs from forcats 1.0.1. AS-FACTOR supports CLOS
  extension; weighted frequency ordering, names, unused levels and missing
  values follow pinned reference cases.
- `parity-X2`: implement the shared column/prototype protocol for factors.
  Factor casts re-encode labels into target levels, preserve missing codes,
  and union levels on concatenation; incompatible ordered casts error.

### Changed
- AGENTS.md: "Parity work" section listing this repository's parity
  milestones (FC1, FC2, FC3) and where the plan and backlog live
  (topic: parity plan).
- Removed emojis from README.md (maintainer preference: no emojis in
  documentation). Meaning is kept in words where an emoji carried it
  (topic: docs style).
- Stopped tracking `build/` (22 files, 796K): the ASDF/Roswell compile
  cache that `make test` writes via `XDG_CACHE_HOME=$(PWD)/build`. It held
  compiled fasls, including Quicklisp internals and absolute local paths.
  `build/` is now in `.gitignore`; local files are untouched. Old copies
  remain in history (topic: repo hygiene).
