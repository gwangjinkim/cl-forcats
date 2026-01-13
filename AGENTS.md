# AGENTS.md: cl-forecats Implementation Plan

This document outlines the milestones for the `cl-forecats` project. Each milestone should be implemented test-first using `fiveAM`.

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
