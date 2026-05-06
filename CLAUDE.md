# NCR Global Diffusion — STM Analysis

## Project Overview

**Research question:** How has Neoclassical Realism (NCR) diffused globally since its inception, and do regional scholarly communities adapt NCR differently from the Global North template?

**Core argument:** STM topic modeling on the NCR bibliometric corpus (N ≈ 348 abstracts) reveals latent thematic variation that maps onto regional scholarly traditions — bibliometric evidence of theoretical localization.

**Current status (May 2026):**
- `stm_code.R` — legacy draft; three structural problems identified (see below)
- `stm2.R` — revised analysis; production-ready, K=10 finalized
- Topic labels are provisional (set in `stm2.R` lines ~220–231); Thales must confirm after running `labelTopics()` and `findThoughts()`
- Figures are draft-quality; none are publication-ready yet
- Paper draft exists (`ncr_paper_v1 (1).docx`) but numbers/figures are not yet synchronized with final model

**Three structural problems fixed in stm2.R (vs stm_code.R):**
1. **Region variable**: was entirely NA in stm_code.R — now built from `C1` affiliation field; 257 of 349 articles matched to a WB region
2. **Country names in vocabulary**: stm_code.R added 386 country-name stems as stopwords, destroying regional signal — removed in stm2.R
3. **K and thresholds**: stm_code.R used K=5 (placeholder) and lower.thresh=10 — stm2.R uses K=10 (optimal) and lower.thresh=35 (as stated in paper methods)

---

## Data Files

| File | Contents | Key columns |
|---|---|---|
| `NCRbiblio.RData` | Main bibliometric dataset, loads as `df` | `AU` (authors), `SO` (source journal), `TI` (title), `DT` (doc type), `PY` (pub year), `LA` (language), `AB` (abstract), `region`, `year` |
| `ctr.RData` | Country name object, loads as `countries` | `countries$countries` — used to build country-name stopwords via `ctrad`/`ctrad1` |

**Critical:** The corpus is small (~348 documents after filtering). This constrains K; the `lower.thresh = 10` in `prepDocuments` is deliberate to retain regionally specific vocabulary that would otherwise be pruned.

---

## Analysis Pipeline

All analysis lives in `stm_code.R`. Stages map to line ranges:

| Stage | Lines | Description |
|---|---|---|
| 0 — Initial cleaning | 1–43 | Library loads, `df → dfs`, British→American spelling, Taylor & Francis boilerplate removal, stemming |
| 1 — Preprocessing | 41–145 | Custom stopwords (`stopwords_custom`), `textProcessor()`, `prepDocuments()` with `lower.thresh=10, upper.thresh=104` |
| 2 — K selection | 146–237 | `searchK(K=5:15)`, diagnostic plots (exclusivity × semantic coherence), `ggsave("stm_model_selection.png")` |
| 3 — Regional effects | 240–387 | `estimateEffect(1:K ~ region + year)`, `region` releveled to "North America" as reference, heatmap export |
| 4 — Topic labeling | 388–403 | `labelTopics()`, `findThoughts()` — human interpretation required here |
| 5 — Diagnostics | 404–430 | ELBO convergence plot, average topic proportions bar chart |
| 6 — Export | 431–455 | `stm_frex_words.csv`, `stm_theta_by_document.csv`, three PNGs |

**Regions in data:** North America (reference), Latin America & Caribbean, East Asia & Pacific, Middle East & North Africa, South Asia, Europe & Central Asia

**Seed:** `set.seed(42)` used at lines 153 and 225 — always preserve for reproducibility.

---

## Tech Stack

- **R** (≥ 4.3) via RStudio project `ncr_stm.Rproj`
- **stm** — core modeling: `textProcessor`, `prepDocuments`, `searchK`, `stm`, `estimateEffect`, `labelTopics`, `findThoughts`, `topicQuality`
- **tidyverse** — data wrangling
- **tidytext** — `stop_words` dataset
- **ggplot2 + patchwork** — all figures
- **tm** — `removeWords`, `removePunctuation`, `stemDocument`, `stopwords("en")`
- **quanteda** — loaded but currently not the primary preprocessing engine (stm's own `textProcessor` is used)

Run with: `Rscript stm_code.R` or sourced interactively in RStudio.

---

## Workflow Rules

### Plan-First for Non-Trivial Tasks

Before executing any task that involves modifying `stm_code.R`, changing model parameters, producing figures, or writing paper sections: **produce a written plan and wait for approval.**

A "non-trivial task" is anything that:
- changes a modeling parameter (K, thresholds, seed, formula)
- produces or modifies a figure intended for the paper
- adds or removes stopwords from `stopwords_custom`
- edits the narrative or numbers in the paper

Trivial tasks (single-line bug fixes, renaming variables, adding a comment) can proceed without a plan.

### Contractor Mode

After a plan is approved, execute autonomously. Only surface a decision point if:
1. An ambiguity appears that would require a judgment call not covered by the plan
2. R throws an unexpected error that changes the scope of the task
3. A result is qualitatively surprising and might affect the paper argument

Do not return with status updates mid-task unless one of the above applies. Return with a clean summary when done.

### Check-In Cadence (Early Sessions)

In the first ~3 sessions, check in more frequently so Thales can calibrate how the workflow operates. After that, shift to full contractor mode.

### Memory Discipline

Decisions made in one session persist. Do not re-ask questions already resolved:
- K has not been finalized yet — always note this
- Topic labels are placeholders until Thales runs `labelTopics()` and `findThoughts()` interactively
- "North America" is the established reference region for `estimateEffect`
- `lower.thresh = 10` was a deliberate revision (was 35) — do not revert without explicit instruction

---

## Output Standards

### Figures (Publication-Ready)

All figures saved to paper must meet these standards:

```r
# Minimum viable publication figure
ggplot(...) +
  theme_minimal(base_size = 11) +         # or theme_linedraw(); never theme_gray/default
  labs(
    title    = "...",                      # sentence case, no trailing period
    subtitle = "...",                      # optional; used for methodological notes
    x        = "...",                      # always labeled, never blank
    y        = "...",                      # always labeled, never blank
    caption  = "Source: NCR bibliometric corpus (N = 348). STM model, K = X."
  ) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "right"             # or "bottom" for wide figures
  )

ggsave("filename.png", width = ..., height = ..., dpi = 300)
```

**Color palette (established in `stm_code.R`):**
- Blue: `#2c7bb6` (positive / high)
- Red: `#d7191c` (negative / low)
- Green: `#1a9641` (held-out, third metric)
- Diverging scale midpoint: white

**Figure dimensions:**
- Single-panel: `width = 8, height = 5`
- Two-panel side-by-side: `width = 12, height = 5`
- Three-panel grid: `width = 10, height = 7`

**Named figures for the paper:**
- `stm_model_selection.png` — K selection diagnostics (Figure 3 in draft)
- `stm_regional_effects_heatmap.png` — `estimateEffect` heatmap (main result figure)
- `stm_topic_proportions.png` — corpus-level topic distribution

### Tables

Export as CSV, then format in LaTeX/Word separately. `stm_frex_words.csv` is the FREX word table for Supplementary Materials.

---

## R Coding Conventions

1. **Object naming:** `snake_case` throughout. Model objects: `model_chosen`, `effects`, `out`, `processed`. Intermediate frames: `_df` suffix (e.g. `effects_df`, `storage_df`).

2. **Named constants for key parameters** — define at the top of any script or planning session:
   ```r
   K_CHOSEN     <- 5      # update after K selection is finalized
   LOWER_THRESH <- 10
   UPPER_THRESH <- 104
   SEED         <- 42
   ```

3. **Reproducibility:** `set.seed(SEED)` before every stochastic call (`searchK`, `stm`).

4. **Section headers:** Match the existing `# ===...===` style with numbered sections.

5. **Comments:** In English. Substantive comments explaining *why*, not *what*.

6. **ggsave immediately after ggplot call** — never rely on RStudio's plot pane for saved figures.

7. **Do not use `attach()`** or modify the global environment beyond what is in the script.

8. **Pipe operator:** `|>` (base R pipe) preferred; `%>%` acceptable since tidyverse is loaded.

---

## Slash Commands

| Command | Purpose |
|---|---|
| `/stm [stage]` | Plan and execute a pipeline stage (preprocess / k-select / effects / label / diagnostics / export / all) |
| `/figure [name]` | Create or refine a publication-ready figure |
| `/paper-check` | Audit consistency between code, outputs, and paper narrative |
