---
description: Plan and execute a stage of the STM analysis pipeline
argument-hint: "[stage: preprocess | k-select | effects | label | diagnostics | export | all]"
allowed-tools: Read, Edit, Write, Bash(Rscript:*), Bash(R:*), Bash(ls:*), Bash(find:*), Bash(grep:*)
---

Read @stm_code.R and @CLAUDE.md before proceeding.

The user is requesting work on this STM analysis stage: **$ARGUMENTS**

## Pipeline Reference

| Stage argument | Lines in stm_code.R | Description |
|---|---|---|
| `preprocess` | 1–145 | Stopwords, textProcessor, prepDocuments thresholds |
| `k-select` | 146–237 | searchK, exclusivity/coherence plots, choosing K |
| `effects` | 240–387 | estimateEffect, regional heatmap, coefficient extraction |
| `label` | 388–403 | labelTopics, findThoughts, assigning substantive labels |
| `diagnostics` | 404–430 | ELBO convergence, topic proportion distribution |
| `export` | 431–455 | CSV and PNG outputs for paper |
| `all` | 1–455 | Full pipeline run |

## Required behavior

1. **Plan first.** Before writing or modifying any code, produce a numbered plan that specifies:
   - What will change in `stm_code.R` and why
   - What parameter values will be used (with justification)
   - What the expected output will be
   - Any decision points that require Thales's input before proceeding

2. **Surface decisions explicitly.** If `$ARGUMENTS` is `k-select` or `label`, these stages require human judgment. Present the diagnostic output or FREX words, then ask Thales to decide before writing anything to the paper or updating topic labels.

3. **After plan approval,** implement autonomously. Run the code with `Rscript stm_code.R` or a targeted excerpt. Return only when done or when an unexpected result changes the plan.

4. **Reproducibility check:** Confirm `set.seed(42)` is present before every stochastic call.

5. **Output check:** Any figure saved must use `dpi = 300` and `theme_minimal()` (or `theme_linedraw()`). Verify before saving.

If `$ARGUMENTS` is empty or unrecognized, list the available stages and ask which one to work on.
