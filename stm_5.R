# ==============================================================================
# stm_5.R — NCR Global Diffusion: STM with All-Author Regional Dummies
# "Neoclassical Realism Goes South"
#
# Key differences from stm2.R:
#   1. Regional coverage: ALL authors in C1, not just first author
#      → 318/349 articles matched (vs 257) using countrycode::codelist
#   2. Region encoding: 6 binary dummies (1 = ≥1 author from that region)
#      → no NAs, no reference-category constraint, no missingness fix needed
#   3. STM prevalence formula: ~ dummy1 + ... + dummy6 + year
#      Coefficients: effect of having ≥1 author from a region on topic prevalence,
#      controlling for other regional presences and publication year
#   4. Sub-Saharan Africa excluded from dummies (2 articles — no statistical power)
# ==============================================================================

library(stm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(patchwork)
library(tm)
library(countrycode)
library(stringr)

# --- Named constants ----------------------------------------------------------
K_CHOSEN     <- 10    # Optimal K: best exclusivity-coherence balance
LOWER_THRESH <- 35    # As stated in paper methods section
UPPER_THRESH <- 104   # ~80% of 348 docs
SEED         <- 42

# ==============================================================================
# 0. LOAD DATA
# ==============================================================================
load("NCRbiblio.RData")
load("ctr.RData")

# ==============================================================================
# 1. BUILD COUNTRY LOOKUP — replicates ncr_2025.R (countrycode::codelist)
# ==============================================================================
# Use the same country source as the descriptive analyses so region assignments
# are fully consistent across the paper.

countries_cty <- countrycode::codelist %>%
  dplyr::select(cow.name, cowc, region) %>%
  rename(countries = cow.name) %>%
  mutate(
    countries = toupper(as.character(countries)),
    countries = ifelse(
      countries == "UNITED STATES OF AMERICA", "UNITED STATES", countries
    ),
    countries = ifelse(
      grepl("WALES|ENGLAND|SCOTLAND|NORTHERN IRELAND", countries),
      "UNITED KINGDOM", countries
    )
  ) %>%
  na.omit() %>%
  # Turkey coded as MENA — matches ncr_2025.R and paper methods
  mutate(region = ifelse(cowc == "TUR", "Middle East & North Africa", region)) %>%
  distinct(countries, .keep_all = TRUE)

# Regex pattern — longest country names first to prevent partial matches
# (e.g. "PAPUA NEW GUINEA" must match before "NEW GUINEA")
ctr_pat <- paste(
  countries_cty$countries[order(-nchar(countries_cty$countries))],
  collapse = "|"
)

# Lowercase, space-free lookup for joining after extraction
countries_lc <- countries_cty %>%
  mutate(countries_lc = tolower(gsub(" ", "", countries))) %>%
  dplyr::select(countries_lc, region) %>%
  distinct(countries_lc, .keep_all = TRUE)

# ==============================================================================
# 2. EXTRACT ALL COUNTRIES FROM C1 AND BUILD REGIONAL DUMMIES
# ==============================================================================
# Strategy: str_extract_all finds every country name in the full C1 string,
# capturing all co-authors' affiliations. One dummy per region = 1 if ≥1 match.
# No duplication of rows — each article remains a single observation.

df_work <- df %>%
  mutate(
    C1 = gsub("ENGLAND|SCOTLAND|WALES", "UNITED KINGDOM", C1),
    C1 = gsub("TURKIYE|TÜRKIYE",        "TURKEY",          C1),
    C1 = gsub("\\bUSA\\b",             "UNITED STATES",    C1)
  ) %>%
  mutate(
    ctz_raw = str_extract_all(C1, ctr_pat),
    ctz_raw = purrr::map(ctz_raw, ~ unique(toupper(str_squish(.x))))
  )

# Map extracted country names to regions
get_regions <- function(ctz_vec) {
  if (length(ctz_vec) == 0) return(character(0))
  lc <- tolower(gsub(" ", "", ctz_vec))
  r  <- countries_lc$region[match(lc, countries_lc$countries_lc)]
  unique(r[!is.na(r)])
}

df_work$regions_all <- lapply(df_work$ctz_raw, get_regions)

# Binary dummies
region_map <- list(
  has_east_asia  = "East Asia & Pacific",
  has_europe     = "Europe & Central Asia",
  has_latam      = "Latin America & Caribbean",
  has_mena       = "Middle East & North Africa",
  has_north_am   = "North America",
  has_south_asia = "South Asia"
  # Sub-Saharan Africa excluded: 2 articles (insufficient variance)
)

for (col in names(region_map)) {
  region_val <- region_map[[col]]
  df_work[[col]] <- as.integer(
    sapply(df_work$regions_all, function(x) region_val %in% x)
  )
}

cat("Regional dummy distributions (all authors):\n")
for (col in names(region_map)) {
  cat(sprintf("  %-20s %3d articles (%4.1f%%)\n",
              col, sum(df_work[[col]]), mean(df_work[[col]]) * 100))
}
cat("  Articles with no region matched:",
    sum(rowSums(df_work[, names(region_map)]) == 0), "\n")

# ==============================================================================
# 3. TEXT PREPARATION
# ==============================================================================
# Drop list columns before passing to textProcessor (requires atomic columns)
dfs <- df_work %>%
  dplyr::select(-ctz_raw, -regions_all) %>%
  filter(!is.na(AB)) %>%
  mutate(
    AB   = tolower(AB),
    AB   = gsub("united states", "unitedstates", AB),
    AB   = gsub("defence",       "defense",       AB),
    AB   = gsub("behaviour",     "behavior",      AB),
    AB   = gsub("informa uk limit trade taylor franci group", "", AB),
    AB   = gsub("taylor franci", "", AB),
    AB   = gsub("©", "", AB),
    year = as.numeric(PY)
  )

# ==============================================================================
# 4. CUSTOM STOPWORDS — functional/boilerplate terms only
#    Country names deliberately NOT included: kept as topical signal
# ==============================================================================
stopwords_custom <- c(
  "however","since","important","also","therefore","thus","context","level","term",
  "recent","suggest","find","well","make","new","key","base","focus","note","result",
  "show","argue","paper","article","study","analysis","analys","approach","framework",
  "theoretical","theori","discuss","highlight","draw","seek","explor","understand",
  "address","particular","regard","contribut","provid","offer","within","across",
  "among","toward","whether","although","despite","given","using","used","exampl",
  "import",
  # NCR-universal terms (near-zero discriminating power across the corpus)
  "neoclassical","realism","realist","ncr","foreign","polic","state",
  "international","behavior","behaviour",
  # Purely functional
  "conclud","method","appli","applic","evalu","three","choic","look","specif",
  "follow","consid","first","second","third","point","hypothesi","put","will",
  "can","methodolog","analyt","explan","aim","analyz","case","question","compar",
  "comparison","research","understood","still","explanatori","paradigm","neoreal",
  "neorealist","empir","test","account","unitlevel","consist","employ","argu",
  "analysi","allow","argument","explain","differ","author","debat","logic",
  "variabl","includ","concern","theoret","respons","identifi","impact","scholar",
  "present","reflect","examin","signific","perspect","studi","relat","two",
  "articl","factor","answer","puzzl","informa","uk","limit","trade","taylor",
  "franci","group","critic","publish","journal","review","intern","press",
  "univers","vol","pp","doi","via","per","one","use","role","form","type",
  "part","end","set","get","come","go","see","just","even","much","both",
  "such","while","other","been","have","from","with","they","this","were",
  "when","thi","thei","that","which"
)

data("stop_words")
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = stopwords_custom, lexicon = "custom")
)

# ==============================================================================
# 5. STM PREPROCESSING
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

cat("Documents:", length(out$documents),
    "| Vocabulary:", length(out$vocab), "\n")

docs <- out$documents
vocab <- out$vocab
meta  <- out$meta

# ==============================================================================
# 6. PREVALENCE FORMULA
# ==============================================================================
prev_formula <- ~ has_east_asia + has_europe + has_latam +
                  has_mena + has_north_am + has_south_asia + year

# ==============================================================================
# 7. K SELECTION DIAGNOSTICS
# ==============================================================================
set.seed(SEED)
storage <- searchK(
  documents  = docs,
  vocab      = vocab,
  K          = 8:16,
  data       = meta,
  prevalence = prev_formula,
  verbose    = FALSE
)

k_df <- data.frame(
  K       = unlist(storage$results$K),
  exclus  = as.numeric(unlist(storage$results$exclus)),
  semcoh  = as.numeric(unlist(storage$results$semcoh)),
  heldout = as.numeric(unlist(storage$results$heldout))
)

# --- Figure: K selection diagnostics -----------------------------------------
p_exc <- ggplot(k_df, aes(x = K, y = exclus)) +
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

p_ec <- ggplot(k_df, aes(x = semcoh, y = exclus, label = K)) +
  geom_text(size = 4.5, fontface = "bold", color = "#2c7bb6") +
  labs(
    title   = "Exclusivity vs. semantic coherence",
    x       = "Semantic coherence",
    y       = "Exclusivity",
    caption = paste0(
      "Source: NCR bibliometric corpus (N = ", length(docs),
      "). Selected K = ", K_CHOSEN, " (dashed line)."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40")
  )

(p_exc | p_coh) / p_ec +
  plot_annotation(
    title    = "STM model selection diagnostics",
    subtitle = paste0(
      "K = ", K_CHOSEN,
      " selected: best exclusivity-coherence balance for 76-term vocabulary"
    ),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    )
  )

ggsave("stm_model_selection.png", width = 10, height = 7, dpi = 300)

# ==============================================================================
# 8. FIT FINAL MODEL
# ==============================================================================
set.seed(SEED)
model_final <- stm(
  documents  = docs,
  vocab      = vocab,
  K          = K_CHOSEN,
  data       = meta,
  prevalence = prev_formula,
  init.type  = "Spectral",
  verbose    = TRUE
)

topicQuality(model = model_final, documents = docs)
summary(model_final)

# ==============================================================================
# 9. TOPIC LABELING
# ==============================================================================
labelTopics(model_final, n = 10)

findThoughts(
  model  = model_final,
  texts  = dfs$AB[as.numeric(rownames(meta))],
  topics = 1:K_CHOSEN,
  n      = 3
)

# --- Topic labels (update after running labelTopics + findThoughts) -----------
topic_labels <- c(
  "1"  = "T01 — European security & Russia",
  "2"  = "T02 — East Asian relations (China)",
  "3"  = "T03 — NCR causal mechanism",
  "4"  = "T04 — Global IR & NCR debates",
  "5"  = "T05 — Domestic institutions & elite framing",
  "6"  = "T06 — Military operations & intervention",
  "7"  = "T07 — US hegemony & systemic change",
  "8"  = "T08 — Threat perception & small states",
  "9"  = "T09 — Peripheral states & sovereignty",
  "10" = "T10 — Strategic culture & adaptation"
)

# Region labels for display
region_labels <- c(
  has_east_asia  = "East Asia & Pacific",
  has_europe     = "Europe & Central Asia",
  has_latam      = "Latin America & Caribbean",
  has_mena       = "Middle East & North Africa",
  has_north_am   = "North America",
  has_south_asia = "South Asia"
)

# ==============================================================================
# 10. ESTIMATE REGIONAL EFFECTS
# ==============================================================================
# No NA issue: all articles have explicit 0/1 on every dummy.
# Coefficients = marginal effect of having ≥1 author from a region on topic
# prevalence, net of other regional presences and publication year.

effects <- estimateEffect(
  formula     = 1:K_CHOSEN ~ has_east_asia + has_europe + has_latam +
                              has_mena + has_north_am + has_south_asia + year,
  stmobj      = model_final,
  metadata    = meta,
  uncertainty = "Global"
)

summary(effects)

# --- Extract coefficients for ggplot -----------------------------------------
extract_effects_df <- function(effects_obj, K, region_map_list) {
  dummy_names  <- names(region_map_list)
  results <- list()
  for (k in 1:K) {
    tbl <- summary(effects_obj, topics = k)$tables[[k]]
    for (d in dummy_names) {
      row_idx <- grep(paste0("^", d), rownames(tbl))
      if (length(row_idx) > 0) {
        results[[length(results) + 1]] <- data.frame(
          topic   = k,
          dummy   = d,
          region  = region_labels[d],
          est     = tbl[row_idx, "Estimate"],
          se      = tbl[row_idx, "Std. Error"],
          p       = tbl[row_idx, "Pr(>|t|)"]
        )
      }
    }
  }
  bind_rows(results)
}

effects_df <- extract_effects_df(effects, K_CHOSEN, region_map) %>%
  mutate(
    topic_label = topic_labels[as.character(topic)],
    sig = case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.1  ~ "*",
      TRUE     ~ ""
    ),
    ci_lo = est - 1.96 * se,
    ci_hi = est + 1.96 * se
  )

# --- Figure A: Heatmap of regional effects (main result) ---------------------
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
    name     = "Coefficient"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11)) +
  labs(
    title    = "Regional authorship and NCR topic prevalence",
    subtitle = paste0(
      "Coefficient = effect of having ≥1 author from a region on topic prevalence\n",
      "*** p<0.01  ** p<0.05  * p<0.1  (controlling for other regions and year)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = paste0(
      "Source: NCR bibliometric corpus (N = ", length(docs),
      "). STM, K = ", K_CHOSEN,
      ". ", sum(colSums(meta[, names(region_map)])),
      " author-region observations across ",
      sum(rowSums(meta[, names(region_map)]) > 0), " articles."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    panel.grid      = element_blank(),
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey40", size = 9),
    plot.caption    = element_text(color = "grey40"),
    legend.position = "right"
  )

ggsave("stm_regional_effects_heatmap.png", width = 12, height = 7, dpi = 300)

# --- Figure B: Coefficient dot plot with 95% CI (alternative) ----------------
ggplot(effects_df,
       aes(x = est,
           y = reorder(topic_label, -topic),
           color = region,
           shape = region)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.5, alpha = 0.6) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Dark2", name = "Region") +
  scale_shape_manual(
    values = c(16, 17, 15, 18, 8, 4),
    name   = "Region"
  ) +
  labs(
    title    = "Regional authorship and NCR topic prevalence",
    subtitle = paste0(
      "Point estimates with 95% CI. Positive = region predicts higher topic prevalence.\n",
      "Controlling for other regional presences and publication year."
    ),
    x        = "Estimated effect on topic prevalence",
    y        = NULL,
    caption  = paste0(
      "Source: NCR bibliometric corpus (N = ", length(docs),
      "). STM, K = ", K_CHOSEN, ". uncertainty = 'Global'."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey40", size = 9),
    plot.caption    = element_text(color = "grey40"),
    legend.position = "right",
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank()
  )

ggsave("stm_regional_effects_coefplot.png", width = 12, height = 7, dpi = 300)

# ==============================================================================
# 11. TOPIC PROPORTION FIGURE
# ==============================================================================
topic_props_df <- data.frame(
  topic = 1:K_CHOSEN,
  label = topic_labels[as.character(1:K_CHOSEN)],
  prop  = colMeans(model_final$theta)
)

ggplot(topic_props_df, aes(x = prop, y = reorder(label, prop))) +
  geom_col(fill = "#2c7bb6", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", prop * 100)),
            hjust = -0.1, size = 3.2) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title   = "Average topic proportions across the corpus",
    x       = "Average proportion",
    y       = NULL,
    caption = paste0(
      "Source: NCR bibliometric corpus (N = ", length(docs),
      "). STM, K = ", K_CHOSEN, "."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.caption       = element_text(color = "grey40"),
    panel.grid.major.y = element_blank()
  )

ggsave("stm_topic_proportions.png", width = 9, height = 6, dpi = 300)

# ==============================================================================
# 12. EXPORT
# ==============================================================================
frex_table <- labelTopics(model_final, n = 7)$frex
frex_df    <- as.data.frame(frex_table)
rownames(frex_df) <- paste0(
  "T", sprintf("%02d", 1:K_CHOSEN), " — ",
  gsub("^T\\d+ — ", "", topic_labels)
)
colnames(frex_df) <- paste0("FREX_", 1:7)
write.csv(frex_df, "stm_frex_words.csv", row.names = TRUE)

theta_df <- as.data.frame(model_final$theta)
colnames(theta_df) <- paste0("topic_", sprintf("%02d", 1:K_CHOSEN))
theta_df <- bind_cols(meta, theta_df)
write.csv(theta_df, "stm_theta_by_document.csv", row.names = FALSE)

write.csv(effects_df, "stm_regional_effects.csv", row.names = FALSE)

cat("\nDone. Files exported:\n")
cat("  stm_model_selection.png\n")
cat("  stm_regional_effects_heatmap.png\n")
cat("  stm_regional_effects_coefplot.png\n")
cat("  stm_topic_proportions.png\n")
cat("  stm_frex_words.csv\n")
cat("  stm_theta_by_document.csv\n")
cat("  stm_regional_effects.csv\n")
cat("\nK =", K_CHOSEN,
    "| Vocab =", length(vocab),
    "| N docs =", length(docs), "\n")
