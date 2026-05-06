---
description: Audit consistency between R code, model outputs, and the paper narrative
allowed-tools: Read, Bash(ls:*), Bash(find:*), Bash(grep:*), Bash(wc:*)
---

Read @stm_code.R and @CLAUDE.md before proceeding.

Perform a consistency audit across the three components of this project: **R code → model outputs → paper narrative.**

## Checklist

### 1. Parameter consistency

Check that every number cited in the paper matches what is actually in `stm_code.R`:

- N (number of documents): what does `length(out$documents)` produce? Does it match any N claim in the paper?
- K (number of topics): what is `K_chosen` set to in the code? Does it match the paper?
- Vocabulary size: does `length(out$vocab)` match any preprocessing description?
- `lower.thresh` (line ~131): should be 10 — does any paper description of the threshold match?
- `upper.thresh` (line ~132): should be 104 — verify
- Reference region for `estimateEffect` (line ~250): should be "North America" — verify relevel call
- Seed: `set.seed(42)` should appear before lines 153 and 225 — verify both

### 2. Figure consistency

For each named figure, check if the file exists:
- `stm_model_selection.png`
- `stm_regional_effects_heatmap.png`
- `stm_topic_proportions.png`

Also verify:
- Do all `ggsave()` calls use `dpi = 300`?
- Are figure filenames consistent with how they are referenced in the paper?

### 3. Topic label consistency

- Are topic labels in `topic_labels` (lines 341–353 of `stm_code.R`) filled in (not "LABEL" placeholders)?
- If labels are set, do they match labels used in the paper?
- Does `stm_frex_words.csv` exist in the project directory?

### 4. Regional claims

- Does the paper's regional analysis section align with what `estimateEffect` estimates (`1:K_chosen ~ region + year`)?
- Are the regions listed in `regions_of_interest` (lines 266–272) consistent with regions discussed in the paper?
- Is the reference category in the paper consistent with `ref = "North America"` in the code?

### 5. Export file consistency

- Does `stm_theta_by_document.csv` exist?
- Does `stm_frex_words.csv` exist?
- Are there K columns in the theta CSV matching `K_chosen`?

## Output format

Produce a structured audit report with three sections:

**Consistent** — items that check out

**Inconsistent** — specific mismatches with exact location (code line number and paper section if known)

**Cannot verify** — items that require running the model or reading the DOCX interactively (flag the DOCX as unreadable if it cannot be parsed)

Always flag any remaining "LABEL" placeholders in `topic_labels` (lines 341–353) as items that must be resolved before submission.
