# AGENTS.md: cl-forcats Implementation Plan

This document outlines the milestones for the `cl-forcats` project. Each milestone should be implemented test-first using `fiveAM`.

## Milestone 0: Factor Data Structure
- Define the `factor` structure/class.
- Implement a basic constructor `make-factor`.
- Implement `print-object` for factors to show levels and data.
- **Verification**: Tests to ensure factor creation and basic properties.

## Milestone 1: Inspection API
- Implement `fct-count`: Count occurrences of levels.
- Implement `fct-unique`: Unique values in logical order.
- Implement `fct-levels` (and `(setf fct-levels)`): Get/set for levels.
- **Verification**: Tests with various data inputs including `NA`.

## Milestone 2: Reordering API
- Implement `fct-relevel`: Manual reordering.
- Implement `fct-reorder`: Reorder by another variable.
- Implement `fct-infreq`: Reorder by frequency.
- Implement `fct-rev`, `fct-shift`.
- **Verification**: Tests ensuring the `data` indices are correctly updated to match new level positions.

## Milestone 3: Modifying API
- Implement `fct-recode`: Rename levels.
- Implement `fct-collapse`: Combine levels.
- Implement `fct-lump`: Group into "Other".
- Implement `fct-other`.
- **Verification**: Tests ensuring levels are correctly merged or renamed.

## Milestone 4: Utility functions
- Implement `fct-drop`, `fct-expand`, `fct-explicit-na`.
- **Verification**: Tests for dropping unused levels and handling `NA`.

## Milestone 5: Integration & DSL
- Integrate with `cl-tibble` (printing factor columns).
- Ensure `fct-reorder` works seamlessly in `dplyr:mutate` (might require helper macros in `cl-dplyr` or generic dispatch).
- Create a user-friendly `factor` function/macro.
- **Verification**: Integration tests with `cl-tibble` and `cl-dplyr`.

## Milestone 6: Final Polish
- Comprehensive docstrings.
- Final README.
- Ensure all tests pass.

## Change Discipline (git + documentation)

Every change to this repository must be reconstructible later: what changed,
why, and what it means for users. Agents and humans follow these rules.

1. **Branch, never commit straight to `main`.** Use a topic branch such as
   `fix/<topic>` or `feat/<topic>` (cross-package work in the tidystat effort
   uses `fix/tidystat-phase0`, `feat/tidystat-phase1`, ...).
2. **One logical change per commit.** Test-suite repairs, bug fixes, new
   features and documentation go in separate commits, so each can be
   reviewed, bisected or reverted on its own.
3. **Commit messages explain the why.** Format:
   ```
   <area>: <imperative summary, <= 72 chars>

   Problem:  what was wrong / missing, with the observable symptom.
   Cause:    the root cause (file:function), if it is a fix.
   Change:   what this commit does.
   Impact:   behaviour change for users; mark BREAKING if any.
   Tests:    which tests were added/changed, and the suite result.
   ```
4. **Every commit updates `CHANGELOG.md`** under `## [Unreleased]`, in the
   sections *Fixed*, *Added*, *Changed*, *Breaking*, *Tests*. Each entry
   says what users will notice and names the commit topic.
   Add new entries to the **existing** section of that heading (newest
   first); never start a second `### Fixed` (etc.) under the same release.
   Before committing, run
   `python3 -I ../cl-tidystat/scripts/normalize-changelog.py --check CHANGELOG.md`
   (without `--check` it merges duplicated sections, losing nothing).
5. **Run the full test suite before committing** (`make test`, or from the
   umbrella repo `../cl-tidystat/scripts/run-tests.sh <this-package>`) and put
   the result in the commit message. Never commit with a red suite unless
   the commit message says so and why.
6. **Cross-package changes** (e.g. an API in `cl-vctrs-lite` used by
   `cl-dplyr`) are recorded in both repos' changelogs and in
   `../cl-tidystat/docs/CHANGES-phase*.md`, which links the commits.
7. **Push only after review**; do not rewrite history that has been pushed.

## Parity work (R tidyverse equivalence)

This repository is part of the plan to reach full parity with the current
R tidyverse. Milestones owned by this repository: **FC1, FC2, FC3**.

- Plan, conventions (R -> Lisp names, arguments, types, 0-based indices)
  and the step-by-step workflow: `docs/PARITY-PLAN.md` in
  `../cl-tidystat` (GitHub: gwangjinkim/cl-tidystat, private; ask the
  maintainer for access).
- Function lists with status and target names:
  `../cl-tidystat/parity/backlog/<ID>.md`; overview in `INDEX.md`.
- Work on a branch `feat/parity-<ID>`, write conformance cases first,
  and regenerate the parity status in cl-tidystat after each step.
