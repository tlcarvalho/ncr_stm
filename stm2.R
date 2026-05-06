# ==============================================================================
# stm2.R — NCR Global Diffusion: Revised STM Analysis
# "Neoclassical Realism Goes South"
#
# Key differences from stm_code.R:
#   1. Region variable built from C1 affiliation field (was entirely NA before)
#   2. Country names REMOVED from custom stopwords (they are signal, not noise)
#   3. lower.thresh = 35 / upper.thresh = 104 (as stated in paper methods)
#   4. K = 10 (best exclusivity-coherence balance for this vocabulary size)
#   5. estimateEffect now works — region variable exists
# ==============================================================================

library(stm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(patchwork)
library(tm)

# --- Named constants ----------------------------------------------------------
K_CHOSEN     <- 10    # Optimal K: best exclusivity-coherence for 76-term vocab
LOWER_THRESH <- 35    # As stated in paper methods section
UPPER_THRESH <- 104   # ~80% of 348 docs — removes near-universal terms
SEED         <- 42

# ==============================================================================
# 0. LOAD DATA
# ==============================================================================
load("NCRbiblio.RData")
load("ctr.RData")

# ==============================================================================
# 1. BUILD REGION VARIABLE FROM AFFILIATION (C1)
# ==============================================================================
# df$C1: affiliation strings from WoS/Scopus, format varies but country appears
# as the last comma-separated element of each author's affiliation block.
# Strategy: search the full C1 string for any known country name (longest match
# first) to assign the first-author's country, then join to WB regions.

country_lookup <- countries %>%
  select(countries, region, global_group) %>%
  mutate(countries = tolower(trimws(countries)))

# Sort country names longest-first to avoid partial matches
# (e.g. "new zealand" must match before "zealand")
country_names_sorted <- country_lookup$countries[order(-nchar(country_lookup$countries))]

extract_country <- function(c1_str) {
  if (is.na(c1_str) || nchar(trimws(c1_str)) == 0) return(NA_character_)
  c1_norm <- tolower(c1_str)
  c1_norm <- gsub("[^a-z&\\s]", " ", c1_norm)
  c1_norm <- gsub("\\s+", " ", c1_norm)
  for (cname in country_names_sorted) {
    pat <- paste0("(?<![a-z])", gsub("&", "\\&", cname, fixed = TRUE), "(?![a-z])")
    if (grepl(pat, c1_norm, perl = TRUE)) return(cname)
  }
  return(NA_character_)
}

df$country_matched <- vapply(df$C1, extract_country, character(1))

df <- df %>%
  left_join(country_lookup %>% select(countries, region, global_group),
            by = c("country_matched" = "countries")) %>%
  # Turkey coded as MENA per paper (population and academic context)
  mutate(region = if_else(country_matched == "turkey",
                          "Middle East & North Africa", region))

cat("Region distribution (articles with matched affiliations):\n")
print(table(df$region, useNA = "always"))
cat("Global North/South distribution:\n")
print(table(df$global_group, useNA = "always"))
cat("Unmatched affiliations:", sum(is.na(df$country_matched)),
    "of", nrow(df), "articles\n")

# ==============================================================================
# 2. TEXT PREPARATION
# ==============================================================================
dfs <- df %>%
  filter(!is.na(AB)) %>%
  mutate(
    AB   = tolower(AB),
    AB   = gsub("united states", "unitedstates", AB),
    AB   = gsub("defence",       "defense",       AB),
    AB   = gsub("behaviour",     "behavior",      AB),
    # Remove Taylor & Francis boilerplate
    AB   = gsub("informa uk limit trade taylor franci group", "", AB),
    AB   = gsub("taylor franci", "", AB),
    AB   = gsub("©", "", AB),
    year = as.numeric(PY)
  )

# ==============================================================================
# 3. CUSTOM STOPWORDS — functional/boilerplate terms only
#    Country names deliberately excluded: they are signal for regional variation
# ==============================================================================
stopwords_custom <- c(
  # Academic boilerplate
  "however", "since", "important", "also", "therefore", "thus", "context",
  "level", "term", "recent", "suggest", "find", "well", "make", "new",
  "key", "base", "focus", "note", "result", "show", "argue", "paper",
  "article", "study", "analysis", "analys", "approach", "framework",
  "theoretical", "theori", "discuss", "highlight", "draw", "seek",
  "explor", "understand", "address", "particular", "regard", "contribut",
  "provid", "offer", "within", "across", "among", "toward", "whether",
  "although", "despite", "given", "using", "used", "exampl", "import",
  # NCR-universal (in virtually all docs — no topic-discriminating power)
  "neoclassical", "realism", "realist", "ncr", "foreign", "polic",
  "state", "international", "behavior", "behaviour",
  # Purely functional
  "conclud", "method", "appli", "applic", "evalu", "three", "choic",
  "look", "specif", "follow", "consid", "first", "second", "third",
  "point", "hypothesi", "put", "will", "can", "methodolog", "analyt",
  "explan", "aim", "analyz", "case", "question", "compar", "comparison",
  "research", "understood", "still", "explanatori", "paradigm", "neoreal",
  "neorealist", "empir", "test", "account", "unitlevel", "consist",
  "employ", "argu", "analysi", "allow", "argument", "explain", "differ",
  "author", "debat", "logic", "variabl", "includ", "concern", "theoret",
  "respons", "identifi", "impact", "scholar", "present", "reflect",
  "examin", "signific", "perspect", "studi", "relat", "two", "articl",
  "factor", "answer", "puzzl", "informa", "uk", "limit", "trade",
  "taylor", "franci", "group", "critic", "publish", "journal",
  "review", "intern", "press", "univers", "vol", "pp", "doi",
  "via", "per", "one", "use", "show", "make", "take",
  "role", "form", "type", "part", "end", "set", "get",
  "come", "go", "see", "just", "even", "much", "both",
  "such", "while", "other", "been", "have", "from", "with",
  "they", "this", "were", "when", "thi", "thei", "that", "which"
)

data("stop_words")  # tidytext
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = stopwords_custom, lexicon = "custom")
)

# ==============================================================================
# 4. STM PREPROCESSING
# ==============================================================================
processed <- textProcessor(
  documents         = dfs$AB,
  metadata          = dfs,
  lowercase         = TRUE,
  removepunctuation = TRUE,
  removenumbers     = TRUE,
  stem              = TRUE,
  wordLengths       = c(3, Inf),
  customstopwords   = all_stopwords$word,
  verbose           = TRUE
)

out <- prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = LOWER_THRESH,
  upper.thresh = UPPER_THRESH,
  verbose      = TRUE
)

cat("Documents in corpus:", length(out$documents), "\n")
cat("Vocabulary size:", length(out$vocab), "\n")

docs <- out$documents
vocab <- out$vocab
meta  <- out$meta

# ==============================================================================
# 5. K SELECTION DIAGNOSTICS
# ==============================================================================
set.seed(SEED)
storage <- searchK(
  documents = docs,
  vocab     = vocab,
  K         = 8:16,
  data      = meta,
  verbose   = FALSE
)

k_df <- data.frame(
  K        = unlist(storage$results$K),
  exclus   = as.numeric(unlist(storage$results$exclus)),
  semcoh   = as.numeric(unlist(storage$results$semcoh)),
  heldout  = as.numeric(unlist(storage$results$heldout)),
  residual = as.numeric(unlist(storage$results$residual))
)

# --- Figure: K selection diagnostics (Figure 3 in paper) ---------------------
p_esc <- ggplot(k_df, aes(x = K, y = exclus)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_point(color = "#2c7bb6", size = 3) +
  geom_vline(xintercept = K_CHOSEN, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 8:16) +
  labs(title = "Exclusivity", x = "Number of topics (K)", y = "Exclusivity") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_coh <- ggplot(k_df, aes(x = K, y = semcoh)) +
  geom_line(color = "#d7191c", linewidth = 1) +
  geom_point(color = "#d7191c", size = 3) +
  geom_vline(xintercept = K_CHOSEN, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 8:16) +
  labs(title = "Semantic coherence", x = "Number of topics (K)",
       y = "Semantic coherence") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_excl_coh <- ggplot(k_df, aes(x = semcoh, y = exclus, label = K)) +
  geom_text(size = 4.5, fontface = "bold", color = "#2c7bb6") +
  labs(
    title    = "Exclusivity vs. semantic coherence",
    x        = "Semantic coherence",
    y        = "Exclusivity",
    caption  = paste0("Source: NCR bibliometric corpus (N = ", length(docs),
                      "). Dashed lines mark selected K = ", K_CHOSEN, ".")
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "grey40"))

(p_esc | p_coh) / p_excl_coh +
  plot_annotation(
    title    = "STM model selection diagnostics",
    subtitle = paste0("K = ", K_CHOSEN,
                      " selected: highest exclusivity with competitive semantic coherence"),
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    )
  )

ggsave("stm_model_selection.png", width = 10, height = 7, dpi = 300)

# ==============================================================================
# 6. FIT FINAL MODEL (K = K_CHOSEN)
# ==============================================================================
set.seed(SEED)
model_final <- stm(
  documents = docs,
  vocab     = vocab,
  K         = K_CHOSEN,
  data      = meta,
  init.type = "Spectral",
  verbose   = TRUE
)

# Topic quality check
topicQuality(model = model_final, documents = docs)
summary(model_final)

# ==============================================================================
# 7. TOPIC LABELING
# ==============================================================================
labelTopics(model_final, n = 10)

findThoughts(
  model  = model_final,
  texts  = dfs$AB[as.numeric(rownames(meta))],
  topics = 1:K_CHOSEN,
  n      = 3
)

# --- Topic labels (fill after running labelTopics + findThoughts) -------------
# Based on K=10 model interpretation (see stm2.R documentation):
topic_labels <- c(
  "1"  = "T01 — European security & Russia",
  "2"  = "T02 — East Asian relations (China)",
  "3"  = "T03 — NCR causal mechanism",
  "4"  = "T04 — Global IR & NCR debates",
  "5"  = "T05 — Domestic institutions & state responses",
  "6"  = "T06 — Military operations & intervention",
  "7"  = "T07 — US hegemony & systemic change",
  "8"  = "T08 — Threat perception & small states",
  "9"  = "T09 — Peripheral states & sovereignty",
  "10" = "T10 — Strategic culture & adaptation"
)

# ==============================================================================
# 8. REGIONAL EFFECTS
# ==============================================================================
# estimateEffect requires nrow(metadata) == length(documents). Articles with
# unmatched affiliations have NA in region; model.matrix() would silently drop
# those rows, causing a dimension mismatch. Fix: assign a placeholder label
# "Unmatched" so the design matrix stays at full N. Exclude "Unmatched" from
# all downstream interpretation — it is a missingness indicator, not a region.

meta_eff <- meta
meta_eff$region <- if_else(
  is.na(meta_eff$region), "Unmatched", as.character(meta_eff$region)
)
# North America = reference (canonical NCR origin)
meta_eff$region <- relevel(factor(meta_eff$region), ref = "North America")

effects <- estimateEffect(
  formula     = 1:K_CHOSEN ~ region + year,
  stmobj      = model_final,
  metadata    = meta_eff,
  uncertainty = "Global"   # conservative; recommended for inference
)

summary(effects)

# --- Extract coefficients for ggplot -----------------------------------------
extract_effects_df <- function(effects_obj, K, meta_eff) {
  # All levels except the reference (North America) and the placeholder
  regions <- setdiff(levels(meta_eff$region), c("North America", "Unmatched"))
  results <- list()
  for (k in 1:K) {
    tbl <- summary(effects_obj, topics = k)$tables[[k]]
    for (r in regions) {
      row_idx <- grep(paste0("region", r), rownames(tbl))
      if (length(row_idx) > 0) {
        results[[length(results) + 1]] <- data.frame(
          topic  = k,
          region = r,
          est    = tbl[row_idx, "Estimate"],
          se     = tbl[row_idx, "Std. Error"],
          p      = tbl[row_idx, "Pr(>|t|)"]
        )
      }
    }
  }
  bind_rows(results)
}

effects_df <- extract_effects_df(effects, K_CHOSEN, meta_eff) %>%
  mutate(
    topic_label = topic_labels[as.character(topic)],
    sig = case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.1  ~ "*",
      TRUE     ~ ""
    )
  )

# --- Figure: Regional effects heatmap (main result figure) -------------------
n_matched <- sum(meta_eff$region != "Unmatched")

ggplot(effects_df,
       aes(x = region,
           y = reorder(topic_label, -topic),
           fill = est)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sig), size = 3.5, vjust = 0.5) +
  scale_fill_gradient2(
    low      = "#d7191c",
    mid      = "white",
    high     = "#2c7bb6",
    midpoint = 0,
    name     = "Coefficient\n(vs. N. America)"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(
    title    = "Regional variation in NCR topic prevalence",
    subtitle = "Positive values: higher prevalence relative to North America\n*** p<0.01  ** p<0.05  * p<0.1",
    x        = NULL,
    y        = NULL,
    caption  = paste0(
      "Source: NCR bibliometric corpus (N = ", length(docs),
      ", region-matched N = ", n_matched,
      "). STM model, K = ", K_CHOSEN, ". Covariates: region + year."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    panel.grid      = element_blank(),
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey40"),
    plot.caption    = element_text(color = "grey40"),
    legend.position = "right"
  )

ggsave("stm_regional_effects_heatmap.png", width = 12, height = 7, dpi = 300)

# ==============================================================================
# 9. DIAGNOSTICS
# ==============================================================================
# Convergence
plot(model_final$convergence$bound, type = "l",
     main = "Model convergence (ELBO)", xlab = "Iteration", ylab = "ELBO")

# Average topic proportions
topic_props_df <- data.frame(
  topic = 1:K_CHOSEN,
  label = topic_labels[as.character(1:K_CHOSEN)],
  prop  = colMeans(model_final$theta)
)

ggplot(topic_props_df, aes(x = prop, y = reorder(label, prop))) +
  geom_col(fill = "#2c7bb6", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", prop * 100)),
            hjust = -0.1, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title   = "Average topic proportions across the corpus",
    x       = "Average proportion",
    y       = NULL,
    caption = paste0("Source: NCR bibliometric corpus (N = ", length(docs),
                     "). STM model, K = ", K_CHOSEN, ".")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40"),
    panel.grid.major.y = element_blank()
  )

ggsave("stm_topic_proportions.png", width = 9, height = 6, dpi = 300)

# ==============================================================================
# 10. EXPORT FOR PAPER
# ==============================================================================
# FREX words table (Supplementary Materials)
frex_table <- labelTopics(model_final, n = 7)$frex
frex_df <- as.data.frame(frex_table)
rownames(frex_df) <- paste0("T", sprintf("%02d", 1:K_CHOSEN), " — ",
                             gsub("^T\\d+ — ", "", topic_labels))
colnames(frex_df) <- paste0("FREX_", 1:7)
write.csv(frex_df, "stm_frex_words.csv", row.names = TRUE)

# Document-topic matrix
theta_df <- as.data.frame(model_final$theta)
colnames(theta_df) <- paste0("topic_", sprintf("%02d", 1:K_CHOSEN))
theta_df <- bind_cols(meta_eff, theta_df)
write.csv(theta_df, "stm_theta_by_document.csv", row.names = FALSE)

# Regional effects table
write.csv(effects_df, "stm_regional_effects.csv", row.names = FALSE)

cat("\nDone. Files exported:\n")
cat("  stm_model_selection.png\n")
cat("  stm_regional_effects_heatmap.png\n")
cat("  stm_topic_proportions.png\n")
cat("  stm_frex_words.csv\n")
cat("  stm_theta_by_document.csv\n")
cat("  stm_regional_effects.csv\n")
cat("\nK =", K_CHOSEN, "| Vocab =", length(vocab),
    "| N docs =", length(docs),
    "| N region-matched =", sum(!is.na(meta$region)), "\n")
