# ==============================================================================
# stm_code_final.R — NCR Global Diffusion: Final Structural Topic Model
# "Neoclassical Realism Goes South"
#
# Model specification:
#   K = 10 topics (selected via searchK, K = 8:16; see Section 7)
#   Prevalence covariates: 6 binary regional dummies + year
#   Regional coverage: ALL authors in C1 (not first author only)
#   Country source: countrycode::codelist — consistent with ncr_2025.R
#
# Topic labels derived from joint inspection of:
#   labelTopics(n=10) and findThoughts(n=10) for all 10 topics.
#   Justifications in comments at Section 10.
#
# Run time: ~8–12 min (searchK dominates).
#   Set SEARCH_K <- FALSE to skip and proceed directly to K = 10.
# ==============================================================================

suppressPackageStartupMessages({
  library(stm)
  library(tidyverse)
  library(tidytext)
  library(ggplot2)
  library(patchwork)
  library(tm)
  library(countrycode)
  library(stringr)
  library(scales)
})

# --- Named constants ----------------------------------------------------------
K_CHOSEN     <- 10     # optimal K from searchK
LOWER_THRESH <- 35     # documented in paper methods section
UPPER_THRESH <- 104    # ~30% of N; removes near-universal terms
SEED         <- 42
SEARCH_K     <- TRUE   # set FALSE to skip searchK

# ==============================================================================
# 0. LOAD DATA
# ==============================================================================
load("NCRbiblio.RData")    # -> df  (N = 349 articles)
load("ctr.RData")          # -> countries (loaded for workspace parity with ncr_2025.R)

# ==============================================================================
# 1. COUNTRY LOOKUP — replicates ncr_2025.R for cross-analysis consistency
# ==============================================================================
# Same source (countrycode::codelist) and normalisations as ncr_2025.R
# so that region assignments are identical across all paper analyses.

countries_cty <- countrycode::codelist |>
  dplyr::select(cow.name, cowc, region) |>
  rename(countries = cow.name) |>
  mutate(
    countries = toupper(as.character(countries)),
    countries = ifelse(
      countries == "UNITED STATES OF AMERICA", "UNITED STATES", countries
    ),
    countries = ifelse(
      grepl("WALES|ENGLAND|SCOTLAND|NORTHERN IRELAND", countries),
      "UNITED KINGDOM", countries
    )
  ) |>
  na.omit() |>
  mutate(region = ifelse(cowc == "TUR", "Middle East & North Africa", region)) |>
  distinct(countries, .keep_all = TRUE)

# Longest names first — prevents partial matches (e.g. PAPUA NEW GUINEA > GUINEA)
ctr_pat <- paste(
  countries_cty$countries[order(-nchar(countries_cty$countries))],
  collapse = "|"
)

countries_lc <- countries_cty |>
  mutate(countries_lc = tolower(gsub(" ", "", countries))) |>
  dplyr::select(countries_lc, region) |>
  distinct(countries_lc, .keep_all = TRUE)

# ==============================================================================
# 2. ALL-AUTHOR REGIONAL DUMMIES
# ==============================================================================
# str_extract_all captures every country name in C1, covering all co-authors.
# One binary dummy per region = 1 if >= 1 author is affiliated in that region.
# No row duplication: each article remains a single observation.
# Sub-Saharan Africa excluded: 2 articles — insufficient variance for estimation.

df_work <- df |>
  mutate(
    C1 = gsub("ENGLAND|SCOTLAND|WALES", "UNITED KINGDOM", C1),
    C1 = gsub("TURKIYE|ÜRKİYE",        "TURKEY",          C1),
    C1 = gsub("\\bUSA\\b",              "UNITED STATES",   C1)
  ) |>
  mutate(
    ctz_raw = str_extract_all(C1, ctr_pat),
    ctz_raw = purrr::map(ctz_raw, ~ unique(toupper(str_squish(.x))))
  )

get_regions <- function(ctz_vec) {
  if (length(ctz_vec) == 0) return(character(0))
  lc <- tolower(gsub(" ", "", ctz_vec))
  r  <- countries_lc$region[match(lc, countries_lc$countries_lc)]
  unique(r[!is.na(r)])
}

df_work$regions_all <- lapply(df_work$ctz_raw, get_regions)

region_map <- list(
  has_east_asia  = "East Asia & Pacific",
  has_europe     = "Europe & Central Asia",
  has_latam      = "Latin America & Caribbean",
  has_mena       = "Middle East & North Africa",
  has_north_am   = "North America",
  has_south_asia = "South Asia"
)

for (col in names(region_map)) {
  rv <- region_map[[col]]
  df_work[[col]] <- as.integer(
    sapply(df_work$regions_all, function(x) rv %in% x)
  )
}

cat("Regional dummy distributions (all authors):\n")
for (col in names(region_map))
  cat(sprintf("  %-20s %3d articles (%4.1f%%)\n",
              col, sum(df_work[[col]]), mean(df_work[[col]]) * 100))
cat("  Articles with no region matched:",
    sum(rowSums(df_work[, names(region_map)]) == 0), "\n")

# ==============================================================================
# 3. TEXT PREPARATION
# ==============================================================================
# List columns (ctz_raw, regions_all) must be dropped: textProcessor requires
# an atomic data frame. All normalizations applied to AB before tokenisation.

dfs <- df_work |>
  dplyr::select(-ctz_raw, -regions_all) |>
  filter(!is.na(AB)) |>
  mutate(
    AB   = tolower(AB),
    AB   = gsub("united states", "unitedstates", AB),
    AB   = gsub("defence",       "defense",       AB),
    AB   = gsub("behaviour",     "behavior",      AB),
    AB   = gsub("informa uk limit trade taylor franci group", "", AB),
    AB   = gsub("taylor franci", "", AB),
    AB   = gsub("©",        "", AB),
    year = as.numeric(PY)
  )

# ==============================================================================
# 4. CUSTOM STOPWORDS
# ==============================================================================
# Functional and boilerplate academic terms only.
# Country names deliberately EXCLUDED — they are topical signal, not noise.

stopwords_custom <- c(
  "however", "since", "important", "also", "therefore", "thus", "context",
  "level", "term", "recent", "suggest", "find", "well", "make", "new",
  "key", "base", "focus", "note", "result", "show", "argue", "paper",
  "article", "study", "analysis", "analys", "approach", "framework",
  "theoretical", "theori", "discuss", "highlight", "draw", "seek",
  "explor", "understand", "address", "particular", "regard", "contribut",
  "provid", "offer", "within", "across", "among", "toward", "whether",
  "although", "despite", "given", "using", "used", "exampl", "import",
  # Near-universal in NCR corpus — no discriminating power across topics
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
  "taylor", "franci", "group", "critic", "publish", "journal", "review",
  "intern", "press", "univers", "vol", "pp", "doi", "via", "per",
  "one", "use", "role", "form", "type", "part", "end", "set", "get",
  "come", "go", "see", "just", "even", "much", "both", "such", "while",
  "other", "been", "have", "from", "with", "they", "this", "were",
  "when", "thi", "thei", "that", "which"
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
  verbose           = FALSE
)

out <- prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = LOWER_THRESH,
  upper.thresh = UPPER_THRESH,
  verbose      = TRUE
)

cat(sprintf("Corpus: %d documents | Vocabulary: %d terms\n",
            length(out$documents), length(out$vocab)))

docs  <- out$documents
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
if (SEARCH_K) {
  cat("\nRunning searchK (K = 8:16) — this takes ~6-10 minutes...\n")
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
  cat("\nsearchK results:\n")
  print(k_df)
  write.csv(k_df, "stm_searchK_results.csv", row.names = FALSE)

  p_exc <- ggplot(k_df, aes(x = K, y = exclus)) +
    geom_line(color = "#2c7bb6", linewidth = 1) +
    geom_point(color = "#2c7bb6", size = 3) +
    geom_vline(xintercept = K_CHOSEN, linetype = "dashed", color = "grey50") +
    scale_x_continuous(breaks = 8:16) +
    labs(title = "Exclusivity", x = "Number of topics (K)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  p_coh <- ggplot(k_df, aes(x = K, y = semcoh)) +
    geom_line(color = "#d7191c", linewidth = 1) +
    geom_point(color = "#d7191c", size = 3) +
    geom_vline(xintercept = K_CHOSEN, linetype = "dashed", color = "grey50") +
    scale_x_continuous(breaks = 8:16) +
    labs(title = "Semantic coherence", x = "Number of topics (K)", y = NULL) +
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
        "). Dashed line: selected K = ", K_CHOSEN, "."
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
        "K = ", K_CHOSEN, " selected: best exclusivity–coherence balance",
        " (vocabulary = ", length(vocab), " terms)"
      ),
      theme = theme(
        plot.title    = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(color = "grey40")
      )
    )
}

k_df %>% mutate(K = as.numeric(K)) %>%  
  ggplot(aes(semcoh, exclus)) + geom_text(aes(label=K)) +
  theme_linedraw() +
  labs(x="Semantic Coherence", y="Exclusivity")

ggsave("stm_model_selection.png", width = 10, height = 7, dpi = 300)

# ==============================================================================
# 8. FINAL MODEL — K = 10, Spectral initialisation
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
summary(model_final)
topicQuality(model = model_final, documents = docs)

# ==============================================================================
# 9. TOPIC VERIFICATION (console output)
# ==============================================================================
# These calls reproduce the evidence base for the labels in Section 10.
# Run interactively in RStudio to inspect words and representative documents.

cat("\n--- labelTopics (n = 10) ---\n")
labelTopics(model_final, n = 10)

cat("\n--- findThoughts (n = 10 per topic) ---\n")
findThoughts(
  model  = model_final,
  texts  = dfs$AB[as.numeric(rownames(meta))],
  topics = 1:K_CHOSEN,
  n      = 10
)

# ==============================================================================
# 10. TOPIC LABELS
# ==============================================================================
# Each label was derived from the joint pattern across:
#   (a) FREX, Prob, LIFT, and Score word lists (labelTopics, n=10)
#   (b) 10 representative abstracts per topic (findThoughts, n=10)
# The justification comment preceding each label summarises the evidence.

topic_labels <- c(

  # T01 (5.4%): Russia is the explicit primary actor in all 10 abstracts.
  # Policy domains: liberal international order, EU-Russia, Ukraine/Crimea (2014,
  # 2022), Norway, Gulf security, WTO, Georgia bandwagoning, Poland.
  # FREX: russia (dominant, no peer within this topic).
  "1"  = "Russia's Foreign Policy",

  # T02 (11.3%): China central in all 10 abstracts. The Belt & Road Initiative
  # (BRI) and economic statecraft dominate; bilateral hedging recurs across
  # Southeast Asia (Malaysia x2), South Asia (Bangladesh), Europe (Poland/Ukraine),
  # MENA (Saudi Arabia-Xinjiang), and Northeast Asia (South Korea, Denmark).
  # FREX: china, econom, increas, initi (BRI).
  "2"  = "China's Rise and Economic Statecraft",

  # T03 (10.7%): Metatheoretical. All 10 abstracts debate NCR's causal chain,
  # specifically the intervening variable concept — its typology, operationalisation,
  # and boundary conditions (vs liberalism, vs Copenhagen School). Applied cases
  # (Pakistan-China, Cuba, Libya, Italy, US Cold War) illustrate the mechanism.
  # FREX: interven, variabl, percept — the methodological core of NCR.
  "3"  = "NCR Causal Mechanisms: Intervening Variables",

  # T04 (10.3%): Theory-building debates about NCR's disciplinary identity and
  # its dialogue with Global IR. All 10 discuss NCR variants (Type I/II/III),
  # Global South alternatives (subaltern, peripheral, moral realism), and the
  # "end of theory" debate. No single empirical region dominates.
  # FREX: theori, global, balanc, contribut.
  "4"  = "NCR Theory and the Global IR Debate",

  # T05 (10.3%): Domestic institutional and elite mechanisms as FP determinants.
  # Cases: Australia (elite framing typology), South Korea/North Korea nuclear,
  # UNASUR disintegration, Malaysia/Philippines and BRI, South American Defense
  # Council, Germany ESDP, Turkey-Israel alliance collapse.
  # FREX: institut, explan, elit, respons.
  "5"  = "Domestic Institutions and Elite Strategies",

  # T06 (9.5%): Military force — its use, restraint, privatisation, and doctrine.
  # Cases: Middle East interventions (Syria, Lebanon 2006), Afghanistan, Cod Wars
  # (Iceland-UK), diversionary use of force, Indonesian military doctrine,
  # Iran-Azerbaijan, private military companies (US, UK, Italy), Rwanda.
  # FREX: militari, war, conflict, forc, leader, decis.
  "6"  = "Military Force and Intervention Decisions",

  # T07 (10.7%): US hegemony as systemic backdrop to regional FP choices.
  # Cases: peaceful change and declining hegemony theory, Japan-US space
  # geopolitics, Saudi grand strategy reassessment (post-2003), Saudi-Qatar
  # blockade, New Zealand/AUKUS, neoconservatism vs NCR (Iraq 2003).
  # FREX: geopolit, unitedst, challeng, competit, chang.
  "7"  = "US Hegemony and Geopolitical Competition",

  # T08 (11.0%): Threat perception as the driver of security alignment shifts
  # and the source of continuity/change in foreign policy.
  # Cases: Finland/NATO (Russia threat), Turkey-Cyprus policy shifts (1950s),
  # Georgia post-Soviet balancing, EU-Russia conflict/cooperation dichotomy,
  # Egypt-Ethiopia (Nile/GERD), Turkey-US divergence, Trump FP continuities.
  # FREX: shift, threat, percept, continu — the continuity-change tension is central.
  "8"  = "Threat Perception and Security Alignment Shifts",

  # T09 (14.3%): Largest topic. States navigating national interest and sovereignty
  # under external structural pressure — a foundational NCR concern.
  # Cases span all regions: Colombia-FARC peace process, Thailand-Myanmar
  # bilateralism, Latin American confederative regionalism (1826-1865), sport
  # diplomacy (China-Taiwan, Serbia-Kosovo), Helmand River water dispute,
  # US Arab Spring, India's nuclear deterrence, regional power underbalancing,
  # China's core national interest, Japan security doctrine transformation.
  "9"  = "National Interest, Sovereignty, and Strategic Autonomy",

  # T10 (6.6%): Strategic culture as an intervening variable, primarily in middle
  # power and European security contexts.
  # Cases: Romania Black Sea (strategic expertise deficit), Canada in Afghanistan
  # (elite consensus, strategic narratives), Philippines-Duterte (US-China
  # competition), EU CSDP and strategic culture, China-Europe relations shift,
  # US positive/negative balancing toward Turkey-Greece, middle powers in
  # US-China competition (Canada, South Korea, Argentina).
  # FREX: strateg, cultur, determin, shape.
  "10" = "Strategic Culture and Middle Power Positioning"
)

region_labels <- c(
  has_east_asia  = "East Asia & Pacific",
  has_europe     = "Europe & Central Asia",
  has_latam      = "Latin America & Caribbean",
  has_mena       = "Middle East & North Africa",
  has_north_am   = "North America",
  has_south_asia = "South Asia"
)

# ==============================================================================
# 11. REGIONAL EFFECTS
# ==============================================================================
# Coefficients: marginal effect of having >= 1 author from a region on topic
# prevalence, net of other regional presences and publication year.
# No NA problem: all 348 documents have explicit 0/1 values on every dummy.

effects <- estimateEffect(
  formula     = 1:K_CHOSEN ~ has_east_asia + has_europe + has_latam +
                              has_mena + has_north_am + has_south_asia + year,
  stmobj      = model_final,
  metadata    = meta,
  uncertainty = "Global"
)

summary(effects)

# Extract dummy coefficients and 95% CIs for plotting
extract_effects_df <- function(effects_obj, K, region_map_list) {
  dummy_names     <- names(region_map_list)
  # summary() called once: $tables[[k]] is valid for k in 1:K
  effects_summary <- summary(effects_obj)
  results <- list()
  for (k in 1:K) {
    tbl <- effects_summary$tables[[k]]
    for (d in dummy_names) {
      row_idx <- grep(paste0("^", d), rownames(tbl))
      if (length(row_idx) > 0) {
        results[[length(results) + 1]] <- data.frame(
          topic  = k,
          dummy  = d,
          region = region_labels[d],
          est    = tbl[row_idx, "Estimate"],
          se     = tbl[row_idx, "Std. Error"],
          p      = tbl[row_idx, "Pr(>|t|)"],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(results)
}

effects_df <- extract_effects_df(effects, K_CHOSEN, region_map) |>
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
  ) %>%
  filter(p <= 0.05)

# ==============================================================================
# 12. FIGURE A — Heatmap of regional effects (main result figure)
# ==============================================================================
ggplot(effects_df,
  aes(x = region, y = reorder(topic_label, -topic), fill = est)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sig), size = 3.5, vjust = 0.5) +
  scale_fill_gradient2(
    low      = "#d7191c",
    mid      = "white",
    high     = "#2c7bb6",
    midpoint = 0,
    name     = "Coefficient") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11)) +
  labs(x        = NULL,
    y        = NULL,
    caption  = "Note: Coefficient = marginal effect of having an author from a region on topic prevalence. 
    *** p<0.01  ** p<0.05  * p<0.1") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    axis.text.y     = element_text(size = 9),
    panel.grid      = element_blank(),
    plot.caption    = element_text(color = "grey40"),
    legend.position = "right"
  )

ggsave("stm_regional_effects_heatmap.png", width = 8, height = 5, dpi = 300)

summary(model_final)

# ==============================================================================
# 13. FIGURE B — Coefficient dot plot with 95% CI
# ==============================================================================
ggplot(
  effects_df,
  aes(x = est, y = reorder(topic_label, -topic), color = region, shape = region)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbarh(
    aes(xmin = ci_lo, xmax = ci_hi),
    height    = 0.25,
    linewidth = 0.5,
    alpha     = 0.6
  ) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Dark2", name = "Region") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 4), name = "Region") +
  labs(
    title    = "Regional authorship and NCR topic prevalence",
    subtitle = paste0(
      "Point estimates with 95 % CI. Positive: region predicts higher topic prevalence.\n",
      "Controlling for other regional presences and publication year."
    ),
    x        = "Estimated effect on topic prevalence",
    y        = NULL,
    caption  = paste0(
      "Source: NCR bibliometric corpus (N = ", length(docs),
      "). STM, K = ", K_CHOSEN, ". uncertainty = 'Global'."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(size = 9),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "grey40", size = 9),
    plot.caption       = element_text(color = "grey40"),
    legend.position    = "right",
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank()
  )

ggsave("stm_regional_effects_coefplot.png", width = 12, height = 7, dpi = 300)

# ==============================================================================
# 14. FIGURE C — Average topic proportions
# ==============================================================================
topic_props_df <- data.frame(
  topic = 1:K_CHOSEN,
  label = topic_labels[as.character(1:K_CHOSEN)],
  prop  = colMeans(model_final$theta)
)

ggplot(topic_props_df, aes(x = prop, y = reorder(label, prop))) +
  geom_col(fill = "grey30") +
  geom_text(
    aes(label = sprintf("%.1f%%", prop * 100)),
    hjust = -0.1,
    size  = 3.2
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.14)),
    labels = percent_format(accuracy = 1)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 36)) +
  labs(
    x       = "Average proportion",
    y       = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(size = 9),
    plot.caption       = element_text(color = "grey40"),
    panel.grid.major.y = element_blank())

ggsave("stm_topic_proportions.png", width = 7, height = 5, dpi = 300)

# ==============================================================================
# 15. EXPORT
# ==============================================================================
frex_table <- labelTopics(model_final, n = 7)$frex
frex_df    <- as.data.frame(frex_table)
rownames(frex_df) <- paste0(
  "T", sprintf("%02d", 1:K_CHOSEN), " — ",
  gsub("^T\\d+ — ", "", topic_labels)
)
colnames(frex_df) <- paste0("FREX_", 1:7)
write.csv(frex_df, "stm_frex_words.csv", row.names = TRUE)

theta_df           <- as.data.frame(model_final$theta)
colnames(theta_df) <- paste0("topic_", sprintf("%02d", 1:K_CHOSEN))
theta_df           <- bind_cols(meta, theta_df)
write.csv(theta_df, "stm_theta_by_document.csv", row.names = FALSE)

write.csv(effects_df, "stm_regional_effects.csv", row.names = FALSE)

cat("\nFiles saved:\n")
cat("  stm_model_selection.png          (Figure 3 — K selection)\n")
cat("  stm_regional_effects_heatmap.png (Figure 4A — main result)\n")
cat("  stm_regional_effects_coefplot.png(Figure 4B — alternative view)\n")
cat("  stm_topic_proportions.png        (Figure 5 — corpus distribution)\n")
cat("  stm_frex_words.csv               (Supplementary Table)\n")
cat("  stm_theta_by_document.csv\n")
cat("  stm_regional_effects.csv\n")
cat(sprintf("\nK = %d | Vocabulary = %d terms | N = %d documents | seed = %d\n",
            K_CHOSEN, length(vocab), length(docs), SEED))
