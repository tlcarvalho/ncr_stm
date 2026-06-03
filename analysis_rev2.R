# ============================================================
# analysis_rev.R
# NCR Global Diffusion — Bibliometric Analysis
# Submission-ready | May 2026
# ============================================================
# Reproduces all bibliometric descriptive figures and the
# theory-citation heatmap for the NCR diffusion paper.
# For the Structural Topic Model analysis see stm_code_final.R.
#
# Inputs : wos_main.bib, scopus_full_table.csv, global_south.csv
# Outputs: figure_pubyear.png, figure_globalgroup.png, map.png,
#          figure_cited_authors.png, figure_journals.png,
#          figure_theory_heatmap.png, NCRbiblio.RData, ctr.RData
# ============================================================


# ── 0. Packages ───────────────────────────────────────────────

suppressPackageStartupMessages({
  library(bibliometrix)
  library(tidyverse)
  library(tidytext)
  library(ggrepel)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(tm)
  library(countrycode)
  library(janitor)
  library(kableExtra)
  library(patchwork)
})


# ── 1. Corpus assembly: WoS + Scopus ─────────────────────────
# Both databases are searched for "neoclassical realis*" in title,
# abstract, keywords, or ID field. Scopus duplicates relative to WoS
# are removed by title and DOI before the two sources are merged.
# Only journal articles published before 2026 are retained.

ncr_pat <- "neoclassical realis*"

wos <- convert2df("wos_main.bib", dbsource = "wos", format = "bibtex") |>
  select(AU, affiliations, TI, AB, DE, ID, PY, SO, DI, C1, CR,
         FU, RP, LA, JI, DT, AU_UN, AU1_UN) |>
  rename(Affiliations = affiliations) |>
  mutate(across(c(TI, AB, DE, ID), str_to_lower)) |>
  filter(
    str_detect(TI, ncr_pat) | str_detect(AB, ncr_pat) |
    str_detect(DE, ncr_pat) | str_detect(ID, ncr_pat)
  )

df <- convert2df("scopus_full_table.csv", dbsource = "scopus", format = "csv") |>
  mutate(across(c(TI, AB, DE, ID), str_to_lower)) |>
  filter(
    str_detect(TI, ncr_pat) | str_detect(AB, ncr_pat) |
    str_detect(DE, ncr_pat) | str_detect(ID, ncr_pat)
  ) |>
  select(AU, Affiliations, TI, AB, DE, ID, PY, SO, DI, C1, CR,
         FU, RP, LA, JI, DT, AU_UN, AU1_UN) |>
  filter(!TI %in% wos$TI, !DI %in% wos$DI) |>
  bind_rows(wos) |>
  distinct(TI, SO, DI, .keep_all = TRUE) |>
  mutate(num = 1) |>
  filter(DT %in% c("ARTICLE", "ARTICLE; EARLY ACCESS"), PY < 2026)


# ── 2. Country / region lookup ────────────────────────────────
# COW country names are joined with World Bank regions and with the
# Global North / South classification from global_south.csv.
# Turkey is reclassified from Europe to MENA, consistent with the
# convention adopted throughout the STM analysis.

gs <- read.csv("global_south.csv") |>
  select(-country_name) |>
  rename(cowc = cow_abbrev)

countries <- countrycode::codelist |>
  dplyr::select(cow.name, cowc, region, region23, un.regionsub.name) |>
  rename(countries = cow.name) |>
  mutate(
    countries = toupper(as.character(countries)),
    countries = ifelse(
      countries == "UNITED STATES OF AMERICA", "UNITED STATES", countries
    ),
    # grepl() here — the original used == which never matches a regex pattern
    countries = ifelse(
      grepl("WALES|ENGLAND|SCOTLAND|NORTHERN IRELAND", countries),
      "UNITED KINGDOM", countries
    )
  ) |>
  na.omit() |>
  left_join(gs, by = "cowc") |>
  mutate(region = ifelse(cowc == "TUR", "Middle East & North Africa", region))

rm(gs)

# Regex pattern for extracting country names from C1 affiliation strings.
# Longest names are placed first so "UNITED STATES" is matched before
# "STATES", preventing partial matches within longer country names.
ctr <- paste(
  countries$countries[order(-nchar(countries$countries))],
  collapse = "|"
)

# Lowercase + no-space version of country names for token-level joining
# (unnest_tokens lowercases and splits on whitespace / punctuation)
countries <- countries |>
  mutate(
    countries = tolower(as.character(countries)),
    countries = gsub(" ", "", countries)
  )

load("NCRbiblio.RData")
load("ctr.RData")

# ── 3. Affiliation-to-country extraction ──────────────────────
# Extracts all matched country names from each article's full affiliation
# string (C1 — covers all co-authors). Spaces within multi-word country
# names are removed so each name survives as a single whitespace token
# when unnest_tokens() later splits on word boundaries.

M <- df |>
  select(AU, C1, TI, DI, DT, SO, PY, CR) |>
  mutate(
    C1 = gsub("ENGLAND|SCOTLAND|WALES", "UNITED KINGDOM", C1),
    C1 = gsub("TURKIYE|TÜRKIYE",        "TURKEY",          C1),
    C1 = gsub("\\bUSA\\b",             "UNITED STATES",    C1)
  ) |>
  mutate(
    ctz = str_extract_all(C1, ctr),
    ctz = purrr::map(ctz, ~ unique(toupper(str_squish(.x)))),
    ctz = map_chr(ctz, ~ if (length(.x) == 0) NA_character_
                  else paste(.x, collapse = ", "))
  ) |>
  mutate(
    # Remove intra-name spaces so "UNITED STATES" becomes one token
    ctz = ifelse(str_detect(ctz, ctr), gsub(" ", "", ctz), ctz),
    # Taiwan is not in the COW list; handle separately
    ctz = ifelse(str_detect(C1, "TAIWAN"), "TAIWAN", ctz)
  )


# ── 4. Save / reload ─────────────────────────────────────────

write.csv(df, "NCRbiblio.csv", row.names = TRUE)
save(df,        file = "NCRbiblio.RData")
save(countries, file = "ctr.RData")

# Reload checkpoint (skip corpus assembly above if already saved):
# load("NCRbiblio.RData"); load("ctr.RData")


# ====================================================================
# SECTION A — DESCRIPTIVE ANALYSIS
# ====================================================================

# A1. Publications per year ───────────────────────────────────────────

df |>
  count(PY, wt = num) |>
  filter(PY < 2026) |>
  ggplot(aes(PY, n)) +
  geom_line() +
  theme_minimal(base_size = 11) +
  labs(
    x       = "Year",
    y       = "Publications per year",
    caption = "Source: NCR bibliometric corpus (N = 349)."
  ) +
  theme(plot.title = element_text(face = "bold"))

ggsave("figure_pubyear.png", dpi = 300, height = 3.5, width = 5.5)


# A2. Publications by Global North / South ────────────────────────────
# An article is attributed to a group for each distinct (article × group)
# pair — so a co-authored article with both North and South authors
# contributes to both series. A third "Collaboration" series counts
# articles with at least one author from each group.

Z2_raw <- tidytext::unnest_tokens(M, countries, ctz, token = "words") |>
  mutate(num = 1) |>
  left_join(countries, by = "countries") |>
  distinct(TI, global_group, .keep_all = TRUE)

Z3 <- Z2_raw |>
  filter(!is.na(global_group)) |>
  group_by(global_group, TI, PY) |>
  summarise(n = sum(num), .groups = "drop") |>
  pivot_wider(names_from = global_group, values_from = n, values_fill = 0L) |>
  mutate(num = as.integer(`Global North` >= 1L & `Global South` >= 1L)) |>
  group_by(PY) |>
  summarise(num = sum(num), .groups = "drop") |>
  mutate(global_group = "Collaboration")

Z2 <- Z2_raw |>
  filter(!is.na(global_group)) |>
  group_by(global_group, PY) |>
  summarise(num = sum(num), .groups = "drop") |>
  bind_rows(Z3) |>
  filter(PY < 2025) |>
  tidyr::complete(global_group, PY = 2000:2024, fill = list(num = 0)) |>
  mutate(label = if_else(PY == 2024, str_to_title(global_group), NA_character_))

ggplot(Z2, aes(PY, num, color = global_group, group = global_group)) +
  geom_line() +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3, na.rm = TRUE) +
  theme_minimal(base_size = 11) +
  labs(
    x       = "Year",
    y       = "Publications per year",
    caption = "Source: NCR bibliometric corpus (N = 349)."
  ) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave("figure_globalgroup.png", dpi = 300, height = 3.5, width = 5.5)


# A3. World map of NCR output ─────────────────────────────────────────

map1 <- tidytext::unnest_tokens(M, countries, ctz, token = "words") |>
  mutate(num = 1) |>
  left_join(countries, by = "countries") |>
  select(TI, cowc, num) |>
  group_by(cowc) |>
  summarise(articles = sum(num), .groups = "drop") |>
  mutate(iso3c = countrycode(cowc, origin = "cowc", destination = "iso3c"))

map_df <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(name, iso_a3, geometry) |>
  left_join(map1, by = c("iso_a3" = "iso3c"))

ggplot(map_df) +
  geom_sf(aes(fill = articles), color = "grey70", linewidth = 0.1) +
  scale_fill_gradient(
    low      = "lightgrey",
    high     = "black",
    na.value = "white"
  ) +
  theme_minimal(base_size = 11) +
  labs(
    fill    = "NCR articles",
    caption = "Source: NCR bibliometric corpus (N = 349)."
  ) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("map.png", width = 12, height = 7, dpi = 300)


# A4. Most-cited authors in Global South articles ─────────────────────
# Restricts to articles with at least one Global South-affiliated author,
# then computes the proportion of those articles that cite each key
# IR theorist. Authors are split into Realist and Non-Realist panels.

south_titles <- tidytext::unnest_tokens(M, countries, ctz, token = "words") |>
  mutate(num = 1) |>
  distinct(TI, countries, .keep_all = TRUE) |>
  left_join(countries, by = "countries") |>
  filter(global_group == "Global South") |>
  pull(TI) |>
  unique()

n_south <- length(south_titles)


# ====================================================================
# SECTION B — AUTHORS, THEORIES, AND TOPICS
# ====================================================================
# STM topic analysis: see stm_code_final.R.

# B1. Theory citation patterns: data construction ─────────────────────
# Binary indicators for each theoretical tradition based on whether the
# article's reference list (CR) contains any canonically associated author.
# Built at the article level (one row per paper) for use in regressions.

theory_patterns <- c(
  Constructivism        = "WENDT|ONUF|KATZENSTEIN|FINNEMORE|SIKKINK|RUGGIE|BARNETT|ADLER",
  `Structural Realism`  = "WALTZ|MEARSHEIMER",
  `Classical Realism`   = "MORGENTHAU|CARR",
  `English School`      = "BULL|WATSON|WIGHT|BUZAN|MAYALL",
  Liberalism            = "KEOHANE|IKENBERRY|NYE|MORAVCSIK",
  `Decol. and Feminism` = paste0(
    "QUIJANO|MIGNOLO|CURIEL|TICKNER J|GROSFOGUEL|SANTOS B|",
    "WALLERSTEIN|BALLESTRIN|CONNELL R|DU BOIS|KRISHNA S|MATOS M|",
    "ENLOE|PATEMAN|SLAUGHTER|ZARAKOL|ACHARYA|SHEPHERD|TRUE J"
  ),
  NCR = paste0(
    "RIPSMAN|TALIAFERRO|LOBELL|CHRISTENSEN|SCHWELLER|",
    "STERLING-FOLKER|ROSE|MEIBAUER|RATHBUN|RATHBURN|",
    "FOULON|GOTZ|GÖTZ|JUNEAU|KITCHEN"
  )
)

# Map WB region strings (as stored in countries$region) to dummy names
region_map <- c(
  "East Asia & Pacific"        = "has_east_asia",
  "Europe & Central Asia"      = "has_europe",
  "Latin America & Caribbean"  = "has_latam",
  "Middle East & North Africa" = "has_mena",
  "North America"              = "has_north_am",
  "South Asia"                 = "has_south_asia"
)

dummy_vars <- unname(region_map)

# Regional dummies at the article level: 1 if ≥1 author is affiliated
# with an institution in that WB region, 0 otherwise.
# pivot_wider ensures one row per article even for multi-region articles.
paper_regions <- tidytext::unnest_tokens(M, countries, ctz, token = "words") |>
  left_join(countries, by = "countries") |>
  filter(countries != "c", region %in% names(region_map)) |>
  distinct(TI, region) |>
  mutate(dummy = region_map[region], has = 1L) |>
  select(TI, dummy, has) |>
  pivot_wider(names_from = dummy, values_from = has, values_fill = 0L)

# Article-level analytical dataset: all papers in M, dummies = 0 for
# articles whose affiliations could not be matched to any WB region.
paper_data <- M |>
  distinct(TI, .keep_all = TRUE) |>
  select(TI, CR) |>
  left_join(paper_regions, by = "TI") |>
  mutate(across(all_of(dummy_vars), ~ replace_na(., 0L)))

for (th in names(theory_patterns))
  paper_data[[th]] <- as.integer(str_detect(paper_data$CR, theory_patterns[[th]]))


# B2. Raw citation rate table ─────────────────────────────────────────
# Mean citation rate per article within each WB region. Each (article ×
# region) pair is counted once — prevents multi-country articles from
# inflating within-region means. Sub-Saharan Africa excluded (n < 5).

dfct_raw <- paper_data |>
  left_join(
    tidytext::unnest_tokens(M, countries, ctz, token = "words") |>
      left_join(countries, by = "countries") |>
      filter(countries != "c", region %in% names(region_map)) |>
      distinct(TI, region),
    by = "TI"
  ) |>
  filter(!is.na(region))

dfct_agg <- dfct_raw |>
  group_by(region) |>
  summarise(
    across(all_of(names(theory_patterns)), mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(Region = region) |>
  mutate(Region = str_to_title(Region))

kableExtra::kbl(
  dfct_agg, digits = 3,
  caption = paste0(
    "Mean theory citation rate by region ",
    "(proportion of articles citing each tradition)."
  )
)


# B3. Regression-based heatmap ────────────────────────────────────────
# For each theoretical tradition, an OLS linear probability model (LPM)
# is estimated with the six regional authorship dummies as predictors:
#
#   theory_i ~ has_east_asia + has_europe + has_latam +
#              has_mena + has_north_am + has_south_asia
#
# The intercept captures baseline citation probability for articles with
# no matched regional affiliation. Each coefficient indicates how having
# ≥1 author from that region is associated with the probability of citing
# the tradition, holding the other regional presences constant.
# Sub-Saharan Africa excluded from the model (< 5 articles).

region_labels <- setNames(names(region_map), unname(region_map))
# e.g. region_labels["has_east_asia"] == "East Asia & Pacific"

reg_results <- purrr::map_dfr(names(theory_patterns), function(th) {
  fml <- as.formula(paste0(
    "`", th, "` ~ ", paste(dummy_vars, collapse = " + ")
  ))
  mod <- lm(fml, data = paper_data)
  broom::tidy(mod) |>
    filter(term %in% dummy_vars) |>
    mutate(
      theory = th,
      region = region_labels[term],
      sig    = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",
        TRUE           ~ ""
      )
    ) |>
    select(theory, region, estimate, std.error, p.value, sig)
})

theory_order <- c(
  "NCR", "Structural Realism", "Classical Realism",
  "Liberalism", "Constructivism", "English School", "Decol. and Feminism"
)

region_order <- c(
  "North America", "Europe & Central Asia",
  "East Asia & Pacific", "Middle East & North Africa",
  "South Asia", "Latin America & Caribbean"
)

reg_long <- reg_results |>
  mutate(
    Theory = factor(theory, levels = theory_order),
    Region = factor(region, levels = rev(region_order)),   # rev: North Am on top
    label  = sprintf("%.3f%s", estimate, sig)
  ) %>%
  filter(p.value <= 0.05)


# ── Panel A: overall mean citation rate per theory ─────────────────────
# Computed across all articles in paper_data (one row per article),
# so the mean is the proportion of papers that cite each tradition.

theory_means <- paper_data |>
  summarise(across(all_of(names(theory_patterns)), mean, na.rm = TRUE)) |>
  pivot_longer(everything(), names_to = "theory", values_to = "mean_rate") |>
  mutate(Theory = factor(theory, levels = theory_order))

p_bar <- ggplot(theory_means, aes(x = Theory, y = mean_rate)) +
  geom_col(fill = "grey35", width = 0.65) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_minimal(base_size = 11) +
  labs(
    title    = "Panel A: proportion of all articles citing each tradition",
    x = NULL,
    y = "Articles citing (%)"
  ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin        = margin(t = 5, r = 5, b = 5, l = 5)
  )

# ── Panel B: regression coefficients by region × theory ───────────────

p_heat <- ggplot(reg_long, aes(x = Theory, y = Region, fill = estimate)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 2.8, color = "grey20") +
  scale_fill_gradient2(
    low      = "#2c7bb6",
    mid      = "white",
    high     = "#d7191c",
    midpoint = 0,
    name     = "Coefficient"
  ) +
  theme_minimal(base_size = 11) +
  labs(title = "Panel B: OLS coefficients",
       x       = NULL,
       y       = NULL,
       caption = paste0(
         "Notes: Sub-Saharan Africa excluded (n < 5). * p<0.05  ** p<0.01  *** p<0.001."
       )
  ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    axis.text.x     = element_text(angle = 35, hjust = 1),
    panel.grid      = element_blank(),
    legend.position = "right",
    plot.margin     = margin(t = 0, r = 5, b = 5, l = 5)
  )


# ── Combined figure ────────────────────────────────────────────────────

fig_combined <- cowplot::plot_grid(
  p_bar, p_heat,
  ncol        = 1,
  rel_heights = c(2, 1.7),
  align       = "v",
  axis        = "lr"
)

ggsave("figure_theory_heatmap.png", fig_combined, width = 8, height = 8, dpi = 300)
