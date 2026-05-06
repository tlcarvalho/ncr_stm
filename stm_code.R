# ==============================================================================
# STM REVISADO — NCR Global Diffusion
# Resolve: stopwords residuais, dicionário pequeno, seleção de K, efeitos regionais
# ==============================================================================

library(stm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(patchwork)
library(tm)
library(quanteda)


load("NCRbiblio.RData")
load("ctr.RData")

dfs <- df %>%
  select(AU, SO, TI, DT, PY, LA, AB) %>% na.omit(AB) %>% 
  mutate(AB = tolower(AB), AB = gsub("united states", "unitedstates", AB),
         AB = gsub("defence", "defense", AB), AB = gsub("behaviour", "behavior", AB),
         tty = 1) %>%
  filter(!is.na(AB))

dfs$AB <- removeWords(dfs$AB, stopwords("en")) 
dfs$AB <- removePunctuation(dfs$AB) 
dfs$AB <- stemDocument(dfs$AB) 



dfs$AB <- gsub("autonomi", "autonom", dfs$AB)
dfs$AB <- gsub("leadership", "leader", dfs$AB)
dfs$AB <- gsub("leaders", "leader", dfs$AB)
dfs$AB <- gsub("leader", "lead", dfs$AB)
dfs$AB <- gsub("informa uk limit trade taylor franci group", "", dfs$AB)
dfs$AB <- gsub("taylor franci", "", dfs$AB)
dfs$AB <- gsub("critic", "", dfs$AB)
dfs$AB <- gsub("©", "", dfs$AB)


# ==============================================================================
# 1. PRÉ-PROCESSAMENTO REVISADO
# ==============================================================================

# Assumindo que seu dataframe se chama `df` com colunas:
# - `abstract`  : texto do abstract
# - `region`    : região do autor (variável covariável)
# - `year`      : ano de publicação
# - `north_south`: "Global North" / "Global South"
#
# Adapte os nomes das colunas conforme necessário.

# --- 1.1 Stopwords customizadas -------------------------------------------

# Stopwords padrão em inglês
data("stop_words")  # do pacote tidytext

ctrad <- substr(countries$countries,1,nchar(countries$countries)-1)
ctrad1 <- tm::stemDocument(countries$countries)
ctrad <- paste(ctrad, sep="", "*")


# Palavras funcionais residuais identificadas nos tópicos do modelo anterior
stopwords_custom <- c(
  # Funcionais genéricas que sobreviveram ao pré-processamento anterior
  "however", "since", "important", "also", "therefore", "thus",
  "context", "level", "term", "recent", "suggest", "find", "well",
  "make", "new", "key", "base", "focus", "note", "result", "show",
  "argue", "paper", "article", "study", "analysis", "analys",
  "approach", "framework", "theoretical", "theori", "discuss",
  "highlight", "draw", "seek", "explor", "understand", "address",
  "particular", "regard", "contribut", "provid", "offer",
  "within", "across", "among", "toward", "whether", "although",
  "despite", "given", "using", "used", "exampl", "import",
  # Termos NCR genéricos que vão aparecer em quase todos os documentos
  # (se aparecerem em >80% dos docs, o threshold já os remove; mas por segurança:)
  "neoclassical", "realism", "realist", "ncr", "foreign", "polic",
  "state", "international", "behavior", "behaviour",
  ## OLD
  "russia", "russian", "moscow", "korea", "north", "south",
  "chines", "chinese", "china",
  "beijing", "beij", "saudi", "arabia", 
  "japan", "japanes", "japanese", "turkey", "turkish",
  "conclud", "method", "appli", "applic", "evalu", "model", "three", 
  "choic", "look", "specif", "follow", "consid", "result", "first",
  "second", "third", "point", "hypothesi", "paper",
  "put", "will", "can", "methodolog", "understand", "analyt", "explan",
  "aim", "analyz", "ncr", "case", "question", "compar", "comparison",
  "research", "within", "understood", "still", "explanatori",
  "paradigm", "neoreal", "neorealist", "realist", "empir", "test",
  "account", "unitlevel", "consist", "show", "employ",
  "theori", "argu", "analysi", "allow", "contribut", "argument",
  "explain", "differ", "author", "debat", "logic", "framework",
  "variabl", "address", "includ", "concern", "toward",
  "theoret", "offer", "respons", "identifi", "impact", "scholar",
  "present", "reflect", "approach", "discuss", "examin",
  "signific", "perspect", "studi", "relat", "two",
  "articl", "visvi", "visavi", "factor", "answer", "puzzl",
  ctrad, ctrad1
)



# Combinando stopwords padrão com customizadas
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = stopwords_custom, lexicon = "custom"))

# --- 1.2 Processamento com stm::textProcessor --------------------------------
# O textProcessor do pacote stm permite passar stopwords customizadas

processed <- textProcessor(
  documents  = dfs$AB,
  metadata   = dfs,                        # dataframe completo como metadata
  lowercase  = TRUE,
  removepunctuation = TRUE,
  removenumbers = TRUE,
  stem       = TRUE,
  wordLengths = c(3, Inf),               # remove tokens com < 3 caracteres
  customstopwords = all_stopwords$word,    # stopwords customizadas
  verbose    = TRUE
)

# --- 1.3 Preparação do corpus com thresholds revisados -----------------------
# Threshold anterior: lower = 35 (~10% do corpus), upper = 104
# Threshold revisado: lower = 10 (~3% do corpus) para capturar termos regionais

out <- prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = 10,    # REVISADO: era 35, agora 10
  upper.thresh = 104,   # ~80% dos 348 documentos (mantém termos quase universais fora)
  verbose = TRUE
)

# Verificar tamanho do dicionário resultante
cat("Documentos no corpus:", length(out$documents), "\n")
cat("Palavras no dicionário:", length(out$vocab), "\n")
# Objetivo: dicionário entre 150-300 palavras; ajuste os thresholds se necessário

docs <- out$documents
vocab <- out$vocab
meta  <- out$meta

# ==============================================================================
# 2. SELEÇÃO DO NÚMERO DE TÓPICOS (K)
# ==============================================================================

# Testar K = 6, 8, 10, 12, 14 para encontrar o melhor ajuste
# Isso pode demorar alguns minutos dependendo do hardware

set.seed(42)

K_candidates <- c(5:15)

storage <- searchK(
  documents = docs,
  vocab     = vocab,
  K         = K_candidates,
  data      = meta,
  verbose   = TRUE
)

k2 <- as.data.frame(storage$results)
k2$exclus <- as.numeric(k2$exclus)
k2$semcoh <- as.numeric(k2$semcoh)

## Figure 3 - Exclusivity and Semantic Coherence, 
## based on the number of topics
k2 %>% mutate(K = as.numeric(K)) %>%  
  ggplot(aes(semcoh, exclus)) + geom_text(aes(label=K)) +
  theme_linedraw() +
  labs(x="Semantic Coherence", y="Exclusivity")


# --- 2.1 Plot de diagnóstico para seleção de K --------------------------------

# Gráfico padrão do pacote stm
plot(storage)

# Versão mais elaborada em ggplot2
storage_df <- data.frame(
  K                 = unlist(storage$results$K),
  exclus            = unlist(storage$results$exclus),
  semcoh            = unlist(storage$results$semcoh),
  heldout           = unlist(storage$results$heldout),
  residual          = unlist(storage$results$residual),
  lbound            = unlist(storage$results$lbound)
)

p_excl <- ggplot(storage_df, aes(x = K, y = exclus)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_point(color = "#2c7bb6", size = 3) +
  labs(title = "Exclusivity", x = "Number of Topics (K)", y = "Exclusivity") +
  theme_minimal()

p_coh <- ggplot(storage_df, aes(x = K, y = semcoh)) +
  geom_line(color = "#d7191c", linewidth = 1) +
  geom_point(color = "#d7191c", size = 3) +
  labs(title = "Semantic Coherence", x = "Number of Topics (K)", y = "Semantic Coherence") +
  theme_minimal()

p_held <- ggplot(storage_df, aes(x = K, y = heldout)) +
  geom_line(color = "#1a9641", linewidth = 1) +
  geom_point(color = "#1a9641", size = 3) +
  labs(title = "Held-Out Likelihood", x = "Number of Topics (K)", y = "Held-Out Likelihood") +
  theme_minimal()

# Plot combinado
(p_excl | p_coh) / p_held +
  plot_annotation(
    title = "STM Model Selection Diagnostics",
    subtitle = "Higher exclusivity and held-out likelihood are better; semantic coherence closer to 0 is better",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave("stm_model_selection.png", width = 10, height = 7, dpi = 300)

# --- 2.2 Topic Quality plot (exclusivity x semantic coherence por tópico) -----
# Rodar para o K escolhido — ajuste K_chosen após avaliar o gráfico acima

K_chosen <- 5  # AJUSTE após avaliar o plot acima

set.seed(42)
model_chosen <- stm(
  documents  = docs,
  vocab      = vocab,
  K          = K_chosen,
  data       = meta,
  init.type  = "Spectral",
  verbose    = TRUE
)

# Topic quality: exclusivity vs. semantic coherence por tópico
topicQuality(model = model_chosen, documents = docs)

summary(model_chosen)

# ==============================================================================
# 3. ESTIMATIVA DOS EFEITOS REGIONAIS (estimateEffect)
# ==============================================================================

# Este é o passo mais importante para o argumento do artigo.
# Estima se a prevalência de cada tópico varia sistematicamente entre regiões.

# Certifique-se de que `region` é um factor com a região de referência correta
# (sugestão: "North America" como referência, para comparar Sul com Norte)

meta$region <- relevel(factor(meta$region), ref = "North America")

# Estimar efeitos
effects <- estimateEffect(
  formula   = 1:K_chosen ~ region + year,
  stmobj    = model_chosen,
  metadata  = meta,
  uncertainty = "Global"   # "Global" é mais conservador e recomendado para inferência
)

# --- 3.1 Sumário dos efeitos -------------------------------------------------
summary(effects)

# --- 3.2 Plot dos efeitos regionais por tópico --------------------------------
# Para cada tópico, mostra o efeito estimado de cada região vs. referência (North America)

regions_of_interest <- c(
  "Latin America & Caribbean",
  "East Asia & Pacific",
  "Middle East & North Africa",
  "South Asia",
  "Europe & Central Asia"
)

# Plot para todos os tópicos simultaneamente
plot(
  effects,
  covariate   = "region",
  topics      = 1:K_chosen,
  model       = model_chosen,
  method      = "difference",
  cov.value1  = "Latin America & Caribbean",
  cov.value2  = "North America",
  xlab        = "Difference in Topic Proportion (Latin America vs. North America)",
  main        = "Regional Variation in Topic Prevalence",
  labeltype   = "frex",
  n           = 5,
  verbose.labels = FALSE
)

# --- 3.3 Plot individual por tópico (mais detalhado) --------------------------
# Útil para os tópicos que você vai destacar na análise

plot_topic_region <- function(topic_n, model, effects, meta) {
  plot(
    effects,
    covariate  = "region",
    topics     = topic_n,
    model      = model,
    method     = "pointestimate",
    xlab       = "Expected Topic Proportion",
    main       = paste("Topic", topic_n, "— Top FREX words:",
                       paste(labelTopics(model, n = 5)$frex[topic_n, ], collapse = ", ")),
    verbose.labels = TRUE
  )
}

# Exemplo: plota tópico 3 e tópico 6
par(mfrow = c(1, 2))
plot_topic_region(3, model_chosen, effects, meta)
plot_topic_region(6, model_chosen, effects, meta)
par(mfrow = c(1, 1))

# --- 3.4 Extração dos coeficientes para ggplot2 --------------------------------
# Para um plot de alta qualidade adequado ao paper

extract_effects <- function(effects, model, K) {
  
  regions <- levels(meta$region)
  results <- list()
  
  for (k in 1:K) {
    for (r in regions) {
      est <- summary(effects, topics = k)$tables[[k]]
      row <- grep(paste0("region", r), rownames(est))
      if (length(row) > 0) {
        results[[length(results) + 1]] <- data.frame(
          topic  = k,
          region = r,
          est    = est[row, "Estimate"],
          se     = est[row, "Std. Error"],
          p      = est[row, "Pr(>|t|)"]
        )
      }
    }
  }
  bind_rows(results)
}

effects_df <- extract_effects(effects, model_chosen, K_chosen)

# Adicionar labels dos tópicos (você vai preencher após interpretar)
topic_labels <- c(
  "1" = "Topic 1 — LABEL",  # preencha após interpretar
  "2" = "Topic 2 — LABEL",
  "3" = "Topic 3 — LABEL",
  "4" = "Topic 4 — LABEL",
  "5" = "Topic 5 — LABEL",
  "6" = "Topic 6 — LABEL",
  "7" = "Topic 7 — LABEL",
  "8" = "Topic 8 — LABEL",
  "9" = "Topic 9 — LABEL",
  "10" = "Topic 10 — LABEL"
)

effects_df <- effects_df %>%
  mutate(
    topic_label = topic_labels[as.character(topic)],
    sig         = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
  )

# Plot heatmap de efeitos regionais
ggplot(effects_df, aes(x = region, y = reorder(topic_label, topic), fill = est)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sig), size = 4, vjust = 0.5) +
  scale_fill_gradient2(
    low      = "#d7191c",
    mid      = "white",
    high     = "#2c7bb6",
    midpoint = 0,
    name     = "Coefficient\n(vs. N. America)"
  ) +
  labs(
    title    = "Regional Variation in NCR Topic Prevalence",
    subtitle = "Positive values indicate higher prevalence relative to North America\n*** p<0.01, ** p<0.05, * p<0.1",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_text(angle = 30, hjust = 1),
    panel.grid   = element_blank(),
    plot.title   = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave("stm_regional_effects_heatmap.png", width = 12, height = 7, dpi = 300)

# ==============================================================================
# 4. INTERPRETAÇÃO E LABELLING DOS TÓPICOS
# ==============================================================================

# Top words por métrica para todos os tópicos
labelTopics(model_chosen, n = 7)

# Documentos mais representativos por tópico
# (útil para nomear os tópicos com segurança)
findThoughts(
  model     = model_chosen,
  texts     = df$abstract[as.numeric(rownames(meta))],  # abstracts alinhados com meta
  topics    = 1:K_chosen,
  n         = 3   # 3 documentos mais representativos por tópico
)

# ==============================================================================
# 5. DIAGNÓSTICOS FINAIS DO MODELO ESCOLHIDO
# ==============================================================================

# Convergência
plot(model_chosen$convergence$bound, type = "l",
     main = "Model Convergence", xlab = "Iteration", ylab = "ELBO")

# Distribuição de tópicos (proporção média no corpus)
topic_props <- colMeans(model_chosen$theta)
topic_props_df <- data.frame(
  topic = 1:K_chosen,
  prop  = topic_props
)

ggplot(topic_props_df, aes(x = reorder(factor(topic), prop), y = prop)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average Topic Proportions Across the Corpus",
    x     = "Topic",
    y     = "Average Proportion"
  ) +
  theme_minimal()

ggsave("stm_topic_proportions.png", width = 8, height = 5, dpi = 300)

# ==============================================================================
# 6. EXPORTAR RESULTADOS PARA O PAPER
# ==============================================================================

# Tabela de top words por tópico (FREX) — pronta para o Supplementary Materials
frex_table <- labelTopics(model_chosen, n = 7)$frex
frex_df <- as.data.frame(frex_table)
rownames(frex_df) <- paste0("Topic ", 1:K_chosen)
colnames(frex_df) <- paste0("Word ", 1:7)

write.csv(frex_df, "stm_frex_words.csv", row.names = TRUE)

# Theta matrix (proporção de cada tópico por documento) — para análises adicionais
theta_df <- as.data.frame(model_chosen$theta)
colnames(theta_df) <- paste0("topic_", 1:K_chosen)
theta_df <- bind_cols(meta, theta_df)

write.csv(theta_df, "stm_theta_by_document.csv", row.names = FALSE)

cat("\nConcluído. Arquivos exportados:\n")
cat("  - stm_model_selection.png\n")
cat("  - stm_regional_effects_heatmap.png\n")
cat("  - stm_topic_proportions.png\n")
cat("  - stm_frex_words.csv\n")
cat("  - stm_theta_by_document.csv\n")