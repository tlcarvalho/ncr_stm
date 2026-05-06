## Loading Packages

library(bibliometrix)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggrepel)
library(quanteda)
library(stringr)
library(quanteda)
library(tm)
library(stm)
library(countrycode)


wos <- convert2df("wos_main.bib", dbsource = "wos", format = "bibtex") %>%
  select("AU", "affiliations", "TI", "AB", "DE", "ID", "PY", "SO", "DI", "C1", "CR", 
         "FU", "RP", "LA", "JI", "DT", "AU_UN", "AU1_UN") %>%
  rename(Affiliations = affiliations) %>%
  mutate(TI = str_to_lower(TI), AB = str_to_lower(AB), DE = str_to_lower(DE), ID = str_to_lower(ID)) %>%
  filter(str_detect(TI, "neoclassical realis*") | str_detect(AB, "neoclassical realis*") |
           str_detect(DE, "neoclassical realis*") | str_detect(ID, "neoclassical realis*")) 

df <- convert2df("scopus_full_table.csv", dbsource = "scopus", format = "csv") %>%
  mutate(TI = str_to_lower(TI), AB = str_to_lower(AB), DE = str_to_lower(DE), ID = str_to_lower(ID)) %>%
  filter(str_detect(TI, "neoclassical realis*") | str_detect(AB, "neoclassical realis*") |
           str_detect(DE, "neoclassical realis*") | str_detect(ID, "neoclassical realis*")) %>%
  select("AU", "Affiliations", "TI", "AB", "DE", "ID", "PY", "SO", "DI", "C1", "CR", 
         "FU", "RP", "LA", "JI", "DT", "AU_UN", "AU1_UN") %>%
  filter(!TI %in% wos$TI) %>% filter(!DI %in% wos$DI) %>%
  rbind(., wos) %>% distinct(TI, SO, DI, .keep_all = T) %>%
  mutate(num = 1) %>%
  filter(DT %in% c("ARTICLE", "ARTICLE; EARLY ACCESS")) %>%
  filter(PY < 2026)

gs <- read.csv("global_south.csv") %>%
  select(-country_name) %>%
  rename(cowc = cow_abbrev)

countries <- countrycode::codelist %>%
  dplyr::select(cow.name, cowc, region, region23, un.regionsub.name) %>%
  rename(countries = cow.name) %>% mutate(countries = toupper(as.character(countries))) %>%
  mutate(countries = ifelse(countries == 'UNITED STATES OF AMERICA', 'UNITED STATES', countries),
         countries = ifelse(countries == 'WALES|ENGLAND|SCOTLAND|NORTHERN IRELAND', 'UNITED KINGDOM' , countries)) %>%
  na.omit() %>%
  left_join(gs, by = "cowc") %>%
  mutate(region = ifelse(cowc == "TUR", "Middle East & North Africa", region))


ctr <- paste(countries$countries, collapse = "|")

M <- df %>%
  select(AU, C1, TI, DI, DT, SO, PY, CR) %>%
  mutate(C1 = gsub('ENGLAND', "UNITED KINGDOM", C1),
         C1 = gsub('SCOTLAND', "UNITED KINGDOM", C1),
         C1 = gsub('WALES', "UNITED KINGDOM", C1),
         C1 = gsub('TURKIYE|TÜRKIYE', "TURKEY", C1),
         C1 = gsub('USA', "UNITED STATES", C1)) %>%
  mutate(ctz = str_extract_all(C1, ctr),                               # list of matches per row
    ctz = purrr::map(ctz, ~ unique(toupper(str_squish(.x)))),
            # trim/squish + uppercase + unique per row
    ctz = map_chr(ctz, ~ if (length(.x) == 0) NA_character_       # collapse to "A, B, C"
                  else paste(.x, collapse = ", "))) %>%
  mutate(ctz = ifelse(str_detect(ctz, ctr), gsub(" ", "", ctz), ctz),
         ctz = ifelse(str_detect(C1, "TAIWAN"), "TAIWAN", ctz))

countries <- countries %>% 
  mutate(countries = tolower(as.character(countries)),
         countries = gsub(" ", "", countries))

rm(gs, ctr)
write.csv(df, "NCRbiblio.csv", row.names = T)
xlsx::write.xlsx(df, "NCRbiblio.xlsx")

save(df, file = "NCRbiblio.RData")

save(countries, file = "ctr.RData")

load("NCRbiblio.RData")
load("ctr.RData")

################################# Descriptive Analysis #################################

### Papers per year

pubyear <- aggregate(num ~ PY, sum, data=df)
pubyear %>% filter(PY < 2026) %>%
  ggplot(aes(PY, num)) + geom_line() +
  theme_bw() + 
  labs(x="Year", y="Publications per year")

ggsave(filename = "figure2.png", dpi = 300, height = 3.5, width = 5.5)

### Papers per continent

gs <- read.csv("global_south.csv") %>%
  select(-country_name) %>%
  rename(cowc = cow_abbrev)
Z2 <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num=1)%>%
  left_join(countries, by="countries") %>%
  distinct(TI, global_group, .keep_all = T)

ZR <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num=1)%>%
  left_join(countries, by="countries") %>%
  distinct(TI, region, .keep_all = T) %>%
  mutate(num = 1) %>%
  aggregate(num ~ region, sum, data=.)

ZC <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num=1)%>%
  left_join(countries, by="countries") %>%
  distinct(TI, cowc, .keep_all = T) %>%
  mutate(num = 1) %>%
  aggregate(num ~ cowc, sum, data=.)


Z3 <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num=1)%>%
  left_join(countries, by="countries") %>%
  distinct(TI, global_group, .keep_all = T) %>%
  mutate(num = 1) %>%
  aggregate(num ~ global_group + TI + PY, sum, data=.) %>%
  reshape2::dcast(TI + PY ~ global_group) %>%
  mutate(num = ifelse(`Global North` == 1 & `Global South` == 1, 1, 0)) %>%
  aggregate(num ~ PY, sum, data=.) %>%
  mutate(global_group = "Collaboration")

Z2 <- aggregate(num ~ global_group + PY, sum, data=Z2) %>%
  rbind(., Z3)

Z2 %>% mutate(countries = stringr::str_to_title(global_group)) %>% 
  filter(PY < 2025) %>%
  mutate(label = if_else(PY == max(PY), as.character(countries), NA_character_)) %>% 
  tidyr::complete(global_group, PY = 2000:2024, fill = list(num = 0)) %>% 
  ggplot(aes(PY, num, color=global_group, group = global_group)) + geom_line() + theme_bw() + 
  labs(x="Year", y="Publications per year", color = "region") + 
  geom_label_repel(aes(label = label), nudge_x = 1,size = 3, na.rm = TRUE) + 
  theme(legend.position = "none", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10))

ggsave(filename = "figure3.png", dpi = 300, height = 3.5, width = 5.5)



Z3 <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num=1)%>%
  left_join(countries, by="countries") %>%
  distinct(TI, global_group, .keep_all = T) %>%
  mutate(num = 1) %>%
  aggregate(num ~ global_group + TI + PY, sum, data=.) %>%
  reshape2::dcast(TI + PY ~ global_group) %>%
  mutate(coop = ifelse(`Global North` == 1 & `Global South` == 1, 1, 0)) %>%
  aggregate(coop ~ PY, sum, data=.)

map1 <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num=1)%>%
  left_join(countries, by="countries") %>%
  select(TI, cowc, num) %>%
  aggregate(num ~ cowc, sum, data=.)

# --- Packages ---
# install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "countrycode"))
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# --- 1) Read your data ---
# If your file is in your working directory, keep "map.csv"
map2 <- map1 %>%
  janitor::clean_names() %>%
  rename(cowc = cowc, articles = num) %>%
  mutate(
    iso3c = countrycode(cowc, origin = "cowc", destination = "iso3c")
  )

# Quick check: any country codes not converted?
bad <- map2 %>% filter(is.na(iso3c))

# --- 2) Get world geometry (sf) ---
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, iso_a3, geometry)

# --- 3) Join and plot ---
map_df <- world %>% left_join(map2, by = c("iso_a3" = "iso3c")) 


ggplot(map_df) +
  geom_sf(aes(fill = articles), color = "grey70", linewidth = 0.1) +
  labs(fill = "NCR articles") +
  theme_minimal() +
  scale_fill_gradient(low = "lightgrey",
                      high = "black", 
                      na.value = "white") +
  theme(legend.position = "bottom")
  
# --- 4) Save (optional) ---
ggsave("map.png", width = 12, height = 7, dpi = 300)


### Most cited authors

south <- tidytext::unnest_tokens(
  M, countries, ctz, token = "words") %>% mutate(num = 1) %>%
  distinct(TI, countries, .keep_all = T) %>%
  left_join(countries, by="countries") %>%
  filter(global_group == "Global South")

ppc <- df %>% filter(TI %in% south$TI) %>%
  filter(!is.na(CR)) %>%
  mutate(`TALIAFERRO JW` = ifelse(str_detect(CR, "TALIAFERRO"), 1, 0),
                     `RIPSMAN NM` = ifelse(str_detect(CR, c("RIPSMAN")), 1, 0),
                     `SCHWELLER RL` = ifelse(str_detect(CR, c("SCHWELLER")), 1, 0),
                     `ROSE G` = ifelse(str_detect(CR, c("ROSE")), 1, 0),
                     `WALTZ KN` = ifelse(str_detect(CR, "WALTZ"), 1, 0),
                     `MEARSHEIMER JJ` = ifelse(str_detect(CR, "MEARSHEIMER"), 1, 0),
                     `LOBELL SE` = ifelse(str_detect(CR, c("LOBELL")), 1, 0),
                     `ZAKARIA F` = ifelse(str_detect(CR, c("ZAKARIA")), 1, 0),
                     `WALT S` = ifelse(str_detect(CR, c("WALT")), 1, 0),
                     `WOHLFORTH WC` = ifelse(str_detect(CR, "WOHLFORTH"), 1, 0),
                     `STERLING-FOLKER J` = ifelse(str_detect(CR, c("STERLING")), 1, 0),
                     `WENDT A` = ifelse(str_detect(CR, "WENDT"), 1, 0),
                     `MORAVCSIK A` = ifelse(str_detect(CR, "MORAVCSIK"), 1, 0),
                     `SNYDER J` = ifelse(str_detect(CR, c("SNYDER J")), 1, 0),
                     `BUZAN B` = ifelse(str_detect(CR, c("BUZAN")), 1, 0),
                     `CHRISTENSEN TJ` = ifelse(str_detect(CR, c("CHRISTENSEN")), 1, 0),
                     `GILPIN RG` = ifelse(str_detect(CR, "GILPIN"), 1, 0),
                     `KEOHANE R` = ifelse(str_detect(CR, c("KEOHANE")), 1, 0),
                     `MORGENTHAU HJ` = ifelse(str_detect(CR, "MORGENTHAU"), 1, 0),
                     `IKENBERRY J` = ifelse(str_detect(CR, "IKENBERRY"), 1, 0),
                     `JOHNSTON A` = ifelse(str_detect(CR, "JOHNSTON"), 1, 0),
                     `KATZENSTEIN P` = ifelse(str_detect(CR, "KATZENSTEIN"), 1, 0),
                     `MEIBAUER G` = ifelse(str_detect(CR, c("MEIBAUER")), 1, 0),
                     `RATHBUN B` = ifelse(str_detect(CR, c("RATHBUN|RATHBURN")), 1, 0),
                     `FOULON M` = ifelse(str_detect(CR, c("FOULON")), 1, 0),
                     `GOTZ E` = ifelse(str_detect(CR, c("GOTZ|GÖTZ")), 1, 0),
                     `JUNEAU T` = ifelse(str_detect(CR, c("JUNEAU")), 1, 0),
                     `KITCHEN N` = ifelse(str_detect(CR, c("KITCHEN")), 1, 0)
                     
                     ) %>% mutate(article = "article")



ppc <- aggregate(cbind(`TALIAFERRO JW`, `RIPSMAN NM`, `SCHWELLER RL`, `ROSE G`, 
                       `WALTZ KN`, `MEARSHEIMER JJ`, `LOBELL SE`, `ZAKARIA F`,
                       `WALT S`, `WOHLFORTH WC`, `STERLING-FOLKER J`,
                       `WENDT A`, `MORAVCSIK A`, `BUZAN B`, `CHRISTENSEN TJ`,
                       `KEOHANE R`, `MORGENTHAU HJ`, `IKENBERRY J`,
                       `JOHNSTON A`, `KATZENSTEIN P`, `MEIBAUER G`,
                       `RATHBUN B`, `FOULON M`, `GOTZ E`, `JUNEAU T`,
                       `KITCHEN N`) ~ article,
                 sum, data=ppc) %>% reshape2::melt(id.vars = "article") %>%
  dplyr::select(-article) %>% rename(Author = variable, Citations = value) %>%
  mutate(realist = ifelse(Author %in% c("WENDT A", "MORAVCSIK A", "BUZAN B",
                                        "KEOHANE R", "IKENBERRY J",
                                        "JOHNSTON A", "KATZENSTEIN P"), "Non-Realist", "Realist")) %>%
  mutate(prop_cite = Citations/159)

r1 <- ppc %>% filter(realist == "Realist") %>%
  ggplot(aes(x=reorder(`Author`, prop_cite), y= prop_cite)) + 
  geom_bar(stat="identity") + coord_flip() + theme_bw() +
  labs(x= "Author", y="Total citations") + 
  theme(axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8),
        plot.title = element_text(size=12)) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Realists")

r2 <- ppc %>% filter(realist == "Non-Realist") %>%
  ggplot(aes(x=reorder(`Author`, prop_cite), y= prop_cite)) + 
  geom_bar(stat="identity") + coord_flip() + theme_bw() +
  labs(x= "Author", y="Total citations") + 
  theme(axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8),
        plot.title = element_text(size=12)) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Non-Realists")

gridExtra::grid.arrange(r1, r2, nrow=1)

### Journals

per <- aggregate(num ~ SO, sum, data=df)

per %>% filter(num > 5) %>%
  ggplot(aes(x= reorder(SO, num), y=num))+
  geom_bar(stat="identity") + coord_flip() + theme_bw() +
  labs(x= "Publication", y="Articles")


################################# Authors, Theories, and Topics #################################

## Authors/Theories 


dfct <- M %>% mutate(Constructivism = ifelse(str_detect(CR, "WENDT|ONUF|KATZENSTEIN|FINNEMORE|SIKKINK|RUGGIE|BARNETT|ADLER"), 1, 0),
                     `Structural Realism` = ifelse(str_detect(CR, c("WALTZ|MEARSHEIMER")), 1, 0),
                     `Classical Realism` = ifelse(str_detect(CR, c("MORGENTHAU|CARR")), 1, 0),
                     `English School` = ifelse(str_detect(CR, c("BULL|WATSON|WIGHT|BUZAN|MAYALL")), 1, 0),
                     Liberalism = ifelse(str_detect(CR, "KEOHANE|IKENBERRY|NYE|MORAVCSIK"), 1, 0), 
                     `Decol. and Feminism` = ifelse(str_detect(CR, "QUIJANO|MIGNOLO|CURIEL|TICKNER J|GROSFOGUEL|SANTOS B|WALLERSTEIN|BALLESTRIN|CONNELL R|DU BOIS|KRISHNA S|MATOS M|ENLOE|PATEMAN|SLAUGHTER|ZARAKOL|ACHARYA|SHEPHERD|TRUE J"), 1, 0),
                     NCR = ifelse(str_detect(CR, "RIPSMAN|TALIAFERRO|LOBELL|CHRISTENSEN|SCHWELLER|STERLING-FOLKER|ROSE|MEIBAUER|RATHBUN|RATHBURN|FOULON|GOTZ|GÖTZ|JUNEAU|KITCHEN"), 1, 0),) %>%
  tidytext::unnest_tokens( ## total papers per country
    ., countries, ctz, token = "words") %>%
  distinct(TI, countries, PY, .keep_all = T) %>% mutate(total_papers=1) %>%
  left_join(countries, by="countries") %>%
  mutate(num = 1) %>%
  filter(countries != "c")

dfct <- aggregate(cbind(Constructivism, `Structural Realism`, `Classical Realism`, Liberalism, `English School`,
                        `Decol. and Feminism`, NCR) ~ region,  mean, data=dfct)%>% 
  rename(Region = region) %>%
  mutate(Region = str_to_title(Region)) %>% filter(Region != "Sub-Saharan Africa")

kableExtra::kbl(dfct, digits = 3)

#### STM

library(tm)
library(quanteda)
library(stm)

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

eval <- dfm(corpus(dfs$AB))
eval <- as.data.frame(docfreq(eval))
eval$words <- row.names(eval)
eval <- eval %>% filter(`docfreq(eval)` > 3)

ctrad <- substr(countries$countries,1,nchar(countries$countries)-1)
ctrad1 <- stemDocument(countries$countries)
ctrad <- paste(ctrad, sep="", "*")
cstop <- c("russia", "russian", "moscow", "korea", "north", "south",
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
           "signific", "perspect", "studi", "relat", "two")


processed <- textProcessor(dfs$AB, metadata=dfs, language="en", verbose=TRUE, 
                             stem = FALSE,
                           customstopwords = c(ctrad, ctrad1, cstop))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     lower.thresh = 17, upper.thresh = 104)
stm::topicQuality(model = topic2, documents = processed$docsuments)

set.seed(9999)
findingk1 <- searchK(out$documents, out$vocab, K=c(5:15),
                     data=out$meta,
                     init.type="Spectral", verbose=TRUE,
                     heldout.seed = 9999)
findingk2 <- searchK(out$documents, out$vocab, K=c(16:25),
                     data=out$meta,
                     init.type="Spectral", verbose=TRUE,
                     heldout.seed = 9999)

k1 <- as.data.frame(findingk2$results)
k2 <- as.data.frame(findingk1$results)
k2 <- rbind(k1, k2)
k2$exclus <- as.numeric(k2$exclus)
k2$semcoh <- as.numeric(k2$semcoh)

## Figure 3 - Exclusivity and Semantic Coherence, 
## based on the number of topics
k2 %>% mutate(K = as.numeric(K)) %>%  
  ggplot(aes(semcoh, exclus)) + geom_text(aes(label=K)) +
  theme_linedraw() +
  labs(x="Semantic Coherence", y="Exclusivity")


topic2 <- stm(documents = out$documents, vocab = out$vocab,
              K =12, data = out$meta,
              init.type = "Spectral", set.seed(9999))

df2 <- make.dt(topic2, meta=out$meta) 
summary(topic2)

df3 <- tidytext::unnest_tokens(M, countries, ctz, token = "words" ) %>% 
  mutate(num = 1)  %>% distinct(TI, AU, countries)  %>%
  left_join(df2, by=c("AU", "TI")) %>% distinct(TI, AU, countries, .keep_all = T) %>%
  mutate(PY = as.character(PY)) %>%
  #mutate(across(where(is.numeric), ~ ifelse(.>0.1, 1, 0)) %>%
  mutate(PY = as.numeric(PY), num=1) %>%
  left_join(countries, by="countries") 

#t4
labelTopics(topic2, n=20)
dput(paste("Topic", sep="", 1:14))

dfag <- aggregate(cbind(Topic4,
                        Topic5, Topic6, Topic8,
                        Topic9, Topic10, 
                        Topic12, Topic13) ~ region, mean,
                  data=df3)%>%
 rename("Elites" = Topic4,
         "Military Force" = Topic5, "Variables" = Topic6,
          "Threat Perception" = Topic8, 
        "Competition" = Topic9, "Strategic Culture" = Topic10,
        "Elites" = Topic11, "Trade" = Topic7,
        "Trade" = Topic7, "Trade" = Topic7,) 


dfag2 <- dfag %>%
  #select(-num) %>% 
  reshape2::melt(id.vars=c("region")) 

save(dfag2, file = "topics_data.RData")

## Figure 5 - Topics with the highest average prevalence per year


asia <- filter(dfag2, region == "East Asia & Pacific") %>%
  top_n(n=5, value) %>%
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Topic", y="Mean Prevalence") +
  theme_bw() + ggtitle("East Asia & Pacific") + ylim(0, 0.3)

europe <- filter(dfag2, region == "Europe & Central Asia") %>%
  top_n(n=5, value) %>%
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Topic", y="Mean Prevalence") +
  theme_bw() + ggtitle("Europe & Central Asia")+ ylim(0, 0.3)

ntam <- filter(dfag2, region == "North America") %>%
  top_n(n=5, value) %>%
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Topic", y="Mean Prevalence") +
  theme_bw() + ggtitle("North America")+ ylim(0, 0.3)

oce <- filter(dfag2, region == "South Asia") %>%
  top_n(n=5, value) %>%
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Topic", y="Mean Prevalence") +
  theme_bw() + ggtitle("South Asia")+ ylim(0, 0.3)
stam <- filter(dfag2, region == "Latin America & Caribbean") %>%
  top_n(n=5, value) %>%
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Topic", y="Mean Prevalence") +
  theme_bw() + ggtitle("Latin America & Caribbean")+ ylim(0, 0.3)

africa <- filter(dfag2, region == "Middle East & North Africa") %>%
  top_n(n=5, value) %>%
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Topic", y="Mean Prevalence") +
  theme_bw() + ggtitle("Middle East & North Africa")+ ylim(0, 0.3)


gridExtra::grid.arrange(ntam, europe, asia, stam, oce, africa, ncol = 2)

