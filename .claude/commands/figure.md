---
description: Create or refine a publication-ready figure for the NCR paper
argument-hint: "[figure-name or description: k-selection | regional-heatmap | topic-proportions | new: description]"
allowed-tools: Read, Edit, Write, Bash(Rscript:*), Bash(ls:*), Bash(find:*)
---

Read @stm_code.R and @CLAUDE.md before proceeding.

The user wants to create or refine this figure: **$ARGUMENTS**

## Figure Standards (mandatory — no exceptions)

Every figure produced for this project must satisfy all of the following:

**Theme:**
- `theme_minimal(base_size = 11)` or `theme_linedraw(base_size = 11)`
- Never `theme_gray()`, never default ggplot2 theme
- `plot.title = element_text(face = "bold")`
- `plot.subtitle = element_text(color = "grey40")`

**Labels:**
- `title`: sentence case, no trailing period
- `x` and `y`: always explicitly labeled — never `NULL` unless axis is self-evident (e.g., a heatmap with clear category labels)
- `caption`: always include source note: `"Source: NCR bibliometric corpus (N = 348). STM model, K = [K_CHOSEN]."`

**Color palette (established):**
- Positive/high: `#2c7bb6`
- Negative/low: `#d7191c`
- Third metric: `#1a9641`
- Diverging fill: `scale_fill_gradient2(low="#d7191c", mid="white", high="#2c7bb6", midpoint=0)`

**Export:**
- `ggsave()` immediately after the ggplot block
- `dpi = 300` — mandatory
- Width/height from CLAUDE.md:
  - Single panel: `width=8, height=5`
  - Two-panel side-by-side: `width=12, height=5`
  - Three-panel grid: `width=10, height=7`

## Named figures and their locations in stm_code.R

| Argument | File saved | Lines |
|---|---|---|
| `k-selection` | `stm_model_selection.png` | 169–218 |
| `regional-heatmap` | `stm_regional_effects_heatmap.png` | 360–386 |
| `topic-proportions` | `stm_topic_proportions.png` | 418–429 |

## Required behavior

1. **Identify the figure** from `$ARGUMENTS`. If it matches a named figure above, read that block in `stm_code.R` and assess what needs to change against the standards above.

2. **Plan the changes.** Present:
   - Current state of the figure code
   - What is wrong or missing (by the standards above)
   - Proposed revision
   - Filename to be saved

3. **After approval:** Implement the revised figure block in `stm_code.R`, run it with Rscript, and confirm the output PNG file exists.

4. **Do not change any modeling code** while working on a figure. Figure work is isolated to the ggplot + ggsave block only.

If `$ARGUMENTS` is empty, list the three named figures and ask which to work on.
