# cl-forcats: Categorical Variables for Common Lisp

`cl-forcats` is a Common Lisp port of the famous R package **forcats**. It provides a robust, efficient, and user-friendly way to handle **categorical data** (factors).

Categorical variables (or factors) are variables that have a fixed and known set of possible values (levels). They are essential for:
-   **Plotting**: Controlling the order of bars or lines.
-   **Modeling**: Ensuring consistent encoding of categories.
-   **Data Analysis**: Moving away from fragile string comparisons to robust, indexed representations.

`cl-forcats` is part of the `cl-tidyverse` ecosystem.

---

## Quick Start for Tidyverse Fans

If you know `forcats` in R, you are already at home. Most functions follow the mapping `fct_xxx` -> `fct-xxx`.

| R `forcats` | Common Lisp `cl-forcats` | Description |
| :--- | :--- | :--- |
| `factor(x)` | `(factor x)` | Create a factor from a sequence |
| `fct_reorder()` | `(fct-reorder f v :fun #'mean)` | Reorder levels by another variable |
| `fct_infreq()` | `(fct-infreq f)` | Reorder levels by frequency |
| `fct_relevel()` | `(fct-relevel f "B" "A")` | Manually move levels to the front |
| `fct_recode()` | `(fct-recode f "New" "Old")` | Rename levels |
| `fct_collapse()` | `(fct-collapse f "Group" '("A" "B"))` | Combine multiple levels into one |
| `fct_lump()` | `(fct-lump f :n 3)` | Group rare levels into "Other" |
| `fct_drop()` | `(fct-drop f)` | Remove unused levels |
| `fct_explicit_na()` | `(fct-explicit-na f)` | Convert `NA` to a named level |

---

## For Common Lisp Developers

### What is a Factor?

In Lisp, we often use symbols or strings for categories. While symbols are great, they don't have an inherent **order** beyond alphabetization. 

A **Factor** in `cl-forcats` is a specialized data structure that separates the **data** from the **labels**.
-   **Data**: An integer vector (indices).
-   **Levels**: A vector of unique labels (strings).

This makes operations like "reversing the order of levels" extremely fast because we only update the level vector or the mapping, not the raw data processing strings.

### The `factor` Structure

```lisp
(defstruct factor
  data    ; Vector of integers (1..N, 0 for NA)
  levels  ; Vector of strings
  ordered ; Boolean
)
```

### Creating Factors

Use the `factor` sugar function. It's smart enough to coerce symbols, keywords, and numbers to strings automatically.

```lisp
(use-package :cl-forcats)

;; From a list of strings
(factor '("apple" "banana" "apple"))

;; From symbols (automatically coerced)
(factor '(apple banana apple))

;; With explicit levels
(factor '(1 2 1) :levels '("Low" "High"))
```

---

## Main Operations

### 1. Inspection: `fct-count`

Get a quick overview of your categories. Returns a tibble with factor `f` and integer `n` columns; `:prop t` adds double `p`. Unused levels and observed missing values have rows. This replaces the old plist result with the maintainer-approved R contract.

```lisp
(let ((counts (fct-count (fct #("a" "b" "a" "a" "c")))))
  (cl-tibble:tbl-col counts "n")) ; #(3 1 1)
```

### 2. Reordering: `fct-reorder`

Crucial for data visualization. Reorder the categories based on values in another vector.

```lisp
(let ((f (factor '(a b c)))
      (v #(10 50 20)))
  ;; Reorder the factor 'f' by the values in 'v'
  (fct-reorder f v :fun #'max))
```

### 3. Modifying: `fct-recode`

Rename categories without doing complex `mapcar` or `ppcre` replaces on the whole dataset.

```lisp
(fct-recode (factor '(low med high)) 
            "Small" "low" 
            "Large" "high")
```

### 4. Collapsing: `fct-lump`

Tired of having 50 categories where 45 of them only appear once? Collapse them into "Other".

```lisp
(fct-lump (factor '(a a a b b c d e f)) :n 2)
;; Keeps top 2 levels ('a' and 'b'), lumps 'c', 'd', 'e', 'f' into "Other".
```

---

## Tidyverse Integration

`cl-forcats` is built to work seamlessly with `cl-tibble` and `cl-dplyr`.

### Using Factors in a Tibble

When you create a tibble, you can include factor columns. `cl-tibble` will recognize them and display the `<fct>` tag.

```lisp
(defparameter *df* 
  (cl-tibble:tibble 
    :name '("Alice" "Bob" "Charlie" "David")
    :group (factor '("A" "B" "A" "B"))))

;; Output in REPL:
;; #<TIBBLE 4x2>
;;   name    group
;;   <chr>   <fct>
;; 1 Alice   A    
;; 2 Bob     B    
;; 3 Charlie A    
;; 4 David   B    
```

### Mutating Factors with `cl-dplyr`

The real power comes when using `cl-dplyr:mutate` to transform factors on the fly.

```lisp
(cl-dplyr:mutate *df*
  :group (fct-recode :group "Alpha" "A" "Beta" "B"))
```

### Advanced: Reordering for Plots

If you are using a plotting library (like a future `cl-ggplot2`), you can reorder your data before plotting:

```lisp
(cl-dplyr:mutate *df*
  :name (fct-reorder :name :some-value-column))
```

---

## Installation

```lisp
;; Not on Quicklisp yet, so clone to your local-projects
(asdf:load-system :cl-forcats)
```

## Testing

We use `FiveAM` for testing. You can run the tests via ASDF:

```lisp
(asdf:test-system :cl-forcats)
```

Or via the provided Roswell script:

```bash
./scripts/test.ros
```

---

## License

MIT

## Shared column protocol (parity X2)

Factors implement cl-vctrs-lite's column access/prototype generics.
`col-ref` exposes labels and the shared NA singleton; the existing internal
codes remain 1-based with 0 for missing. `vec-cast` re-encodes values into
target levels and rejects unknown labels or incompatible ordered types.
An empty factor prototype infers levels from character input, in encounter
order, as R vctrs does. `vec-c` unions unordered levels in first-encounter
order. Typed initialization, subsetting and recycling preserve the factor
object and its levels. With cl-tibble loaded, factors can be stored,
printed, sliced and row-bound as columns.

## Creating and combining factors (FC1)

| R | Common Lisp |
|---|---|
| `as_factor` | `as-factor` (CLOS generic) |
| `fct` | `fct` |
| `fct_c` | `fct-c` |
| `fct_cross` | `fct-cross` |
| `fct_unify` | `fct-unify` |
| `lvls_union` | `lvls-union` |
| `fct_inorder` | `fct-inorder` |
| `fct_inseq` | `fct-inseq` |
| `fct_infreq` | `fct-infreq` |
| `fct_match` | `fct-match` |
| `fct_count` | `fct-count` |
| `fct_unique` | `fct-unique` |

`fct` accepts character input, infers levels in first-appearance order and
rejects unknown values with explicit levels. `as-factor` preserves factors,
uses appearance order for characters, numeric order for numbers and
FALSE/TRUE levels for logical input. Scalar NIL is FALSE; use a typed empty
column for an empty atomic input. Factor names and orderedness survive where
R retains them. `fct-infreq` accepts optional nonnegative observation weights.
The order functions accept `:ordered` T/NIL or the shared NA to preserve it.

`fct-c` takes factors as rest arguments; APPLY splices a factor list.
`fct-unify` takes a list/alist/typed :LIST, retaining its names and applying
a complete union of levels (or explicit `:levels`). `fct-cross` accepts
`:sep` and `:keep-empty`, recycles scalar inputs and rejects incompatible
sizes. `fct-match` returns a logical column and rejects unknown levels.

**Approved return migrations:** `fct-count` now returns a tibble; read
counts with `(col-ref (cl-tibble:tbl-col (fct-count f) "n") 0)` instead of
GETF on plist rows. `fct-unique` now returns a factor with every level,
including unused levels, plus an implicit missing observation if present;
read labels through `col-ref` rather than treating it as a list. Existing
`fct-lump` keeps its private legacy count adapter and unchanged return shape.

Explicit NA factor levels work in the local FC1 operations, including
count/unique. The shared X2 prototype still requires unique string levels,
so generic cast/prototype pipelines with NA levels retain that restriction.
Empty proportions use actual IEEE NaN on SBCL; other implementations require
equivalent IEEE support. No full-platform or locale parity is claimed.

Validation: 109 package checks and 135 pinned R 4.6.1 / forcats 1.0.1
reference cases pass (129 creation cases plus six baseline cases). Run
`../cl-tidystat/scripts/run-tests.sh cl-forcats` and the umbrella conformance
runner. Reference contracts: [creation](https://forcats.tidyverse.org/reference/fct.html),
[conversion](https://forcats.tidyverse.org/reference/as_factor.html),
[count](https://forcats.tidyverse.org/reference/fct_count.html), and
[unique](https://forcats.tidyverse.org/reference/fct_unique.html).

## Reordering and relabelling (FC2)

| R | Common Lisp |
|---|---|
| `fct_reorder2` | `fct-reorder2` |
| `first2`, `last2` | `first2`, `last2` |
| `fct_relabel` | `fct-relabel` |
| `fct_anon`, `fct_shuffle` | `fct-anon`, `fct-shuffle` |
| `lvls_reorder`, `lvls_revalue`, `lvls_expand` | `lvls-reorder`, `lvls-revalue`, `lvls-expand` |

`lvls-reorder` takes zero-based indices and optional `:ordered`; revalue
merges duplicate labels, and expand requires all existing levels. Factor
codes, names, orderedness and explicit NA levels survive local operations.
`fct-relabel` calls its function once on the complete typed level vector,
then passes its rest arguments. The function must return character labels.
`fct-relevel` accepts a sole level-vector callback or named levels, with
`:after` zero, a nonnegative whole number or positive infinity.

`first2`/`last2` return a size-one owner column from Y after stable ordering
by X, dropping rows missing in either input; an empty selection returns
owner-typed NA. `fct-reorder2` calls `:fun` with two owner columns in
observation order, followed by `:args`. Its defaults are `last2`, decreasing
order and negative infinity for unused levels. Omitted `:na-rm` removes
missing X/Y rows with a warning; `:na-rm t` is silent, NIL keeps them.
The returned factor follows the measured R row removal.

`fct-shuffle` and `fct-anon` accept `:seed` (an integer from -2147483647 to 2147483647) for
exact local compatibility with the pinned R default Mersenne-Twister /
rejection sampler, or `:random-state` for a Common Lisp stream. Calls never
launch R or change an R global stream. `fct-anon` also takes a string
`:prefix` and errors on an empty factor, as R does. Alternative R RNG kinds
are outside this explicit default-stream comparison.

Three existing functions remain **partial** pending explicit compatibility
approval. `fct-reorder` retains mean and legacy empty/missing defaults; its
callback still receives a list. Additive `:default`, `:na-rm` and `:args`
options work, and callback values now follow observation order.
`fct-recode` retains numeric old-label coercion, where R errors; string
specifications merge/remove levels, and unknown levels warn.
`fct-relevel` also retains numeric scalar label coercion, where R errors.
See the umbrella
`docs/FC2-REORDER-PROPOSAL.md` and its preserved pending R evidence.

FC2 validation: 150 package checks and 263 active pinned R reference cases
pass (128 reordering cases plus the existing 135). Six unchanged mismatches
are retained separately as evidence for the three precise partial functions.
