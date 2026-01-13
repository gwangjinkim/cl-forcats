# cl-forcats: Categorical Variables for Common Lisp

`cl-forcats` is a Common Lisp port of the famous R package **forcats**. It provides a robust, efficient, and user-friendly way to handle **categorical data** (factors).

Categorical variables (or factors) are variables that have a fixed and known set of possible values (levels). They are essential for:
-   **Plotting**: Controlling the order of bars or lines.
-   **Modeling**: Ensuring consistent encoding of categories.
-   **Data Analysis**: Moving away from fragile string comparisons to robust, indexed representations.

`cl-forcats` is part of the `cl-tidyverse` ecosystem.

---

## 🚀 Quick Start for Tidyverse Fans

If you know `forcats` in R, you are already at home. Most functions follow the mapping `fct_xxx` ⮕ `fct-xxx`.

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

## 🧠 For Common Lisp Developers

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

## 🛠️ Main Operations

### 1. Inspection: `fct-count`

Get a quick overview of your categories. Returns a list of plists with `:level` and `:n`.

```lisp
(fct-count (factor '(a b a a c)))
;; => ((:LEVEL "A" :N 3) (:LEVEL "B" :N 1) (:LEVEL "C" :N 1))
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

## 📦 Installation

```lisp
;; Not on Quicklisp yet, so clone to your local-projects
(asdf:load-system :cl-forcats)
```

## 🧪 Testing

We use `FiveAM` for testing. You can run the tests via ASDF:

```lisp
(asdf:test-system :cl-forcats)
```

Or via the provided Roswell script:

```bash
./scripts/test.ros
```

---

## 📜 License

MIT
