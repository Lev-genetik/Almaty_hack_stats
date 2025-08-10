# lAIzy.stats

Helpers to **build publication-ready `gtsummary` tables** and to **auto-draft interpretations with an LLM**.

This package (repo) currently exposes three user-facing helpers:

* `gts_build()` – build a grouped `gtsummary::tbl_summary()` with consistent formatting and return handy metadata + a plain-text version for prompts.
* `interpret_table()` – take an existing `tbl_summary` object, choose an output **style** (publication/clinical/report/popular), and ask an LLM for the narrative.

---

## Installation

```r
# from GitHub (replace with your repo if different)
# install.packages("remotes")
remotes::install_github("Lev-genetik/Almaty_hack_stats")
```

### Requirements (key)

* R ≥ 4.1
* `gtsummary`, `tibble`, `dplyr`, `magrittr`, `assertthat`, `readr`
* For LLM features: `openai`
* (Optional for reporting): `flextable`, `officer`

---

## Quick start

```r
library(gtsummary)
library(dplyr)

# Example data
df <- iris |>
  mutate(Species = as.character(Species))  # gts_build will coerce grouping to factor if needed

# 1) Build a summary table and collect metadata
gt <- gts_build(
  data    = df,
  by      = "Species",
  include = c("Sepal.Length", "Sepal.Width", "Species")
)

gt$tbl       # gtsummary table
cat(gt$text) # plain-text table (useful for LLM prompts)
gt$vars_used # variables included
gt$vars_info # "name:class" pairs

# 2) Ask the LLM to interpret a new grouped table built on the fly
txt <- interpret_table_v2(
  table_input         = df,
  by                  = "Species",
  varriabes_for_stats = names(df),
  language            = "en",
  context             = "For a research paper",
  ai_key              = Sys.getenv("OPENAI_API_KEY")  # or path to a file you keep out of git
)
cat(txt)

# 3) Or interpret an existing tbl_summary with a chosen 'style'
tbl_obj <- tbl_summary(df, by = Species)
txt2 <- interpret_table(
  tbl_summary_in = tbl_obj,
  style          = "publication",       # "publication","clin_report","report","popular"
  language       = "en",
  verbosity      = "moderate",
  model          = "gpt-4o",
  ai_key         = Sys.getenv("OPENAI_API_KEY")
)
cat(txt2)
```

---

## Function reference

### `gts_build(data, by = NULL, include = NULL, stat_cont = "{mean} ± {sd}", stat_cat = "{n} ({p}%)", digits_cont = 1, missing_policy = "no", add_p = TRUE, bold = TRUE)`

**What it does**

* Optionally **narrows columns** to `include`.
* Validates and **adds/normalizes `by`** (factor) if provided.
* Builds a consistent `tbl_summary()` with custom statistics and digits.
* Adds p-values (`add_p`) and bold labels (`bold`) if requested.
* Returns the table **plus**:

  * `text`: a plain-text rendering (great for LLM prompts),
  * `vars_used`: names of variables included,
  * `vars_info`: `"name:class"` string,
  * `by`: the grouping variable (or `NULL`).

**Returns**

```r
list(
  tbl,        # gtsummary object
  text,       # plain-text table
  vars_used,  # character vector
  vars_info,  # "name:class, ..." string
  by          # character or NULL
)
```

**Notes**

* If `include` omits `by`, `gts_build()` automatically adds it.
* Character/logical `by` is coerced to factor.

---

### `interpret_table(tbl_summary_in, style = c("publication","clin_report","report","popular"), language = "en", sure = TRUE, verbosity = "moderate", instructions = "", varriabes_for_stats = c(), verbose_function_mode = FALSE, model = "gpt-4o", formality = 0.2, ai_key = "Evgeny")`

**What it does**

* Accepts an existing **`gtsummary` table** (`tbl_summary_in`).
* Chooses a narrative **style**:

  * `publication` → “For a research paper”
  * `clin_report` → “For a clinical report”
  * `report` → “For an activity report”
  * `popular` → “For a popular science article”
* `sure = TRUE` encourages confident wording; `FALSE` asks the model to hedge.
* Builds a prompt and calls the OpenAI API; returns the **narrative text**.

**API key**

* Supports either a direct key or a file path (recommended to keep keys out of version control).

**Returns**

* A character string (the model’s interpretation).

**Example**

```r
tbl_obj <- gtsummary::tbl_summary(iris, by = Species)
txt <- interpret_table(
  tbl_summary_in = tbl_obj,
  style          = "clin_report",
  language       = "en",
  sure           = FALSE,
  ai_key         = Sys.getenv("OPENAI_API_KEY")
)
```

---

## Working with API keys

* **Best practice:** set once per session or shell.

```bash
export OPENAI_API_KEY="sk-...your-key..."
```

* Or pass a **filepath** to a local key stored outside git (e.g., `ai_key = "ai_key.txt"`). Add to `.gitignore`:

```
ai_key.txt
```

---

## Tips & gotchas

* Your **grouping column (`by`) must exist** in the data (and will be coerced to factor when needed).
* For **percent displays**, ensure categorical variables are factors or characters.
* If you see “not in data” errors, check `include` names.
* LLM output is **non-deterministic**; set `formality` (temperature) lower for more stable results.

---

## Contributing

Issues and PRs are welcome! Please include a minimal reproducible example and session info when reporting bugs.

---
