# SPEC.md: cl-forcats

`cl-forcats` is a Common Lisp library for handling categorical variables (factors), inspired by the R package `forcats`. It is part of the `cl-tidyverse` ecosystem.

## Philosophy

The goal of `cl-forcats` is to provide a clean, functional API for manipulating factors. Factors are used to store categorical data where the order of levels matters (e.g., for plotting, tables, or statistical modeling), or where it's useful to have a fixed set of possible values.

## Data Structure: Factor

A `factor` in `cl-forcats` is a structure that stores:
- `data`: An integer vector (usually `(unsigned-byte 32)` or similar) representing the indices of levels.
- `levels`: A sequence (usually a list or vector of strings) representing the categorical labels.
- `ordered`: A boolean indicating if the levels have a meaningful order (optional, defaults to `nil`).

Index `0` in `data` usually represents `NA` or "Missing", while indices `1` to `N` map to the `N` elements in `levels`.

## Core API

### Inspection
- `fct-count (factor &key sort prop)`: Count occurrences of each level.
- `fct-unique (factor)`: Return unique values in order of appearance.
- `fct-levels (factor)`: Access or change the levels of a factor.

### Reordering Levels (Change order, keep values)
- `fct-relevel (factor &rest levels)`: Manually move levels to the front.
- `fct-reorder (factor v &key (fun #'mean) desc)`: Reorder levels by another numeric vector `v`.
- `fct-infreq (factor &key ordered)`: Reorder levels by frequency.
- `fct-rev (factor)`: Reverse the order of levels.
- `fct-shift (factor &key (n 1))`: Shift levels left or right.

### Modifying Levels (Change values, potentially collapse)
- `fct-recode (factor &rest new-levels)`: Rename levels.
- `fct-collapse (factor &rest group-definitions)`: Collapse multiple levels into one.
- `fct-lump (factor &key n prop other-level)`: Group rare levels into "Other".
- `fct-other (factor &key keep drop other-level)`: Specifically keep or drop certain levels into "Other".

### Utility
- `fct-drop (factor)`: Remove unused levels.
- `fct-expand (factor &rest additional-levels)`: Add new levels to a factor.
- `fct-explicit-na (factor &key (na-level "(Missing)"))`: Convert `NA` to a named level.

## Integration

### cl-vctrs-lite
`cl-forcats` factors should be recognized as a valid vector type in the `cl-vctrs-lite` ecosystem.

### cl-tibble & cl-dplyr
- `fct-reorder` should be easily usable inside `dplyr:mutate`.
- Tibble printing should display factor columns with their levels or as `<fct>`.

## Lisp DSL (Lispy Sugar)
- A macro or function `factor (data &key levels ordered)` for easy creation.
- Generic functions where appropriate to work with other sequence types.
- Reader macros or shorthand for common operations if they fit the ecosystem.

## Regex Support
- Use `cl-ppcre` for any level renaming or filtering that involves regular expressions.
