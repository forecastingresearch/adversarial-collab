# This document is in-progress.

#### Setup####
rm(list = ls())
# replace with own dir
yourHome <- "~/fri"
library(googlesheets4)
library(dplyr)
library(qdap)
library(data.table)
gs4_auth()

setwd(paste0(yourHome, "/voi-vod"))
source("sources/functions.R")

#### Import from P8 composite - Raw####
url <- "***REMOVED***/edit#gid=1396622057"

# P8 composite sheet will be parsed as _decimals_ rather than 0-100 percentages.
p8_composite <- read_sheet(url, "Raw") %>%
  rename(PU = `P(U)`, PC = `P(C)`, PUC = `P(U|C)`)

#### Get CID tags from P8 composite - Composite####
cid_tags <- read_sheet(url, "Composite")
#names(cid_tags) <- unlist(cid_tags[1, ])
cid_tags <- cid_tags %>% select(Id, Tag)
cid_tags$Id <- unlist(cid_tags$Id)
cid_tags <- cid_tags[1:nrow(cid_tags), ]

# New dataframe just for Molly - merge p8_composite with cid_tags by `C id`
molly_composite <- merge(p8_composite, cid_tags, by.x = "C id", by.y = "Id", all.x = TRUE)
molly_composite <- molly_composite %>% rename(Pc = PC, PUc = PUC, ID = Tag, Person = Name)
write.csv(molly_composite, "molly_composite.csv")

#### Initialize composite sheet with Id and Tag####
composite_sheet <- cid_tags

#### Add columns for composite sheet, grouped by Concerned/Skeptical...
# P(U),
pu_newcols <- getProbComposite_p8("PU")
composite_sheet <- bindComposite_p8(composite_sheet, "PU", pu_newcols)
# P(C),
pc_newcols <- getProbComposite_p8("PC")
composite_sheet <- bindComposite_p8(composite_sheet, "PC", pc_newcols)

# U-c correlation,
cor_table <- p8_composite %>% select(C_id, PU, PC)
cor_table <- cor_table[complete.cases(cor_table), ]

C_ids <- unique(cor_table$C_id)

PU_PC_correlation <- c()

for (i in 1:length(C_ids)) {
  cor_tbl_id <- cor_table %>% filter(C_id == C_ids[i])
  correlate <- cor(cor_tbl_id$PU, cor_tbl_id$PC, method = "spearman")
  PU_PC_correlation <- c(PU_PC_correlation, correlate)
}

composite_sheet <- composite_sheet %>% mutate(PU_PC_correlation = PU_PC_correlation)

# P(U|C),
puc_newcols <- getProbComposite_p8("PUC")
composite_sheet <- bindComposite_p8(composite_sheet, "PUC", puc_newcols)

# P(U|¬C),
p8_composite <- p8_composite %>%
  rowwise() %>%
  mutate(punotc = punotc(pc = PC, puc = PUC, pu = PU))
punotc_newcols <- getProbComposite_p8("punotc")
composite_sheet <- bindComposite_p8(composite_sheet, "punotc", punotc_newcols)

# Effect of C,
# raw
p8_composite <- p8_composite %>%
  rowwise() %>%
  mutate(effect_c_raw = PUC - punotc)
# standardized_raw
max_effects <- aggregate(abs(effect_c_raw) ~ Name, max, data = p8_composite)
p8_composite <- p8_composite %>%
  rowwise() %>%
  mutate(max_effect = max_effects[max_effects$Name == Name, ]$"abs(effect_c_raw)")
p8_composite <- p8_composite %>%
  mutate(effect_c_standardized = effect_c_raw / max_effect)
# absolute
p8_composite <- p8_composite %>%
  rowwise() %>%
  mutate(effect_c_raw_abs = abs(effect_c_raw)) %>%
  mutate(effect_c_standardized_abs = abs(effect_c_standardized))

# raw
effect_c_raw_abs_newcols <- getProbComposite_p8("effect_c_raw_abs")
composite_sheet <- bindComposite_p8(composite_sheet, "effect_c_raw_abs_mean", effect_c_raw_abs_newcols)
composite_sheet <- composite_sheet %>%
  mutate(effect_c_raw_abs_mean_concerned_composite_rank = rank(-effect_c_raw_abs_mean_concerned, ties.method = "min", na.last = "keep")) %>%
  mutate(effect_c_raw_abs_mean_skeptical_composite_rank = rank(-effect_c_raw_abs_mean_skeptical, ties.method = "min", na.last = "keep"))
composite_sheet <- bindComposite_p8(composite_sheet, "effect_c_raw_abs_median", effect_c_raw_abs_newcols)
composite_sheet <- composite_sheet %>%
  mutate(effect_c_raw_abs_median_concerned_composite_rank = rank(-effect_c_raw_abs_median_concerned, ties.method = "min", na.last = "keep")) %>%
  mutate(effect_c_raw_abs_median_skeptical_composite_rank = rank(-effect_c_raw_abs_median_skeptical, ties.method = "min", na.last = "keep"))
# standardized
effect_c_standardized_abs_newcols <- getProbComposite_p8("effect_c_standardized_abs")
composite_sheet <- bindComposite_p8(composite_sheet, "effect_c_standardized_abs_mean", effect_c_standardized_abs_newcols)
composite_sheet <- composite_sheet %>%
  mutate(effect_c_standardized_abs_mean_concerned_composite_rank = rank(-effect_c_standardized_abs_mean_concerned, ties.method = "min", na.last = "keep")) %>%
  mutate(effect_c_standardized_abs_mean_skeptical_composite_rank = rank(-effect_c_standardized_abs_mean_skeptical, ties.method = "min", na.last = "keep"))
composite_sheet <- bindComposite_p8(composite_sheet, "effect_c_standardized_abs_median", effect_c_standardized_abs_newcols)
composite_sheet <- composite_sheet %>%
  mutate(effect_c_standardized_abs_median_concerned_composite_rank = rank(-effect_c_standardized_abs_median_concerned, ties.method = "min", na.last = "keep")) %>%
  mutate(effect_c_standardized_abs_median_skeptical_composite_rank = rank(-effect_c_standardized_abs_median_skeptical, ties.method = "min", na.last = "keep"))

##### VoI (naive),####
p8_composite <- p8_composite %>%
  rowwise() %>%
  mutate(VoI_naive = VoI_naive(pu = PU, puc = PUC, pc = PC, punotc = punotc))
VoI_naive_newcols <- getProbComposite_p8("VoI_naive")
composite_sheet <- bindComposite_p8(composite_sheet, "VoI_naive", VoI_naive_newcols)

# VoI (naive) individual rank averages
p8_composite <- p8_composite %>%
  group_by(Name) %>%
  mutate(VoI_naive_avg_ind_rank = rank(-VoI_naive, ties.method = "min", na.last = "keep"))
VoI_naive_avg_ind_rank_newcols <- getProbComposite_p8("VoI_naive_avg_ind_rank")
composite_sheet <- bindComposite_p8(composite_sheet, "VoI_naive_avg_ind_rank", VoI_naive_avg_ind_rank_newcols)

#composite level
composite_sheet <- composite_sheet %>%
  rowwise() %>%
  mutate(VoI_naive_mean_concerned_COMPONENT = VoI_naive(pu=PU_mean_concerned, puc=PUC_mean_concerned, pc=PC_mean_concerned, punotc=punotc_mean_concerned)) %>% 
  mutate(VoI_naive_median_concerned_COMPONENT = VoI_naive(pu=PU_median_concerned, puc=PUC_median_concerned, pc=PC_median_concerned, punotc=punotc_median_concerned)) %>%
  mutate(VoI_naive_mean_skeptical_COMPONENT = VoI_naive(pu=PU_mean_skeptical, puc=PUC_mean_skeptical, pc=PC_mean_skeptical, punotc=punotc_mean_skeptical)) %>% 
  mutate(VoI_naive_median_skeptical_COMPONENT = VoI_naive(pu=PU_median_skeptical, puc=PUC_median_skeptical, pc=PC_median_skeptical, punotc=punotc_median_skeptical)) %>%
  ungroup()

##### VoD (naive)#####
#composite level
composite_sheet <- composite_sheet %>%
  rowwise() %>%
  mutate(VoD_naive_mean = VoD_naive(pu_a = PU_mean_concerned,
                                    pu_b = PU_mean_skeptical,
                                    puc_a = PUC_mean_concerned,
                                    puc_b = PUC_mean_skeptical,
                                    pc_a = PC_mean_concerned,
                                    pc_b = PC_mean_skeptical,
                                    punotc_a = punotc_mean_concerned,
                                    punotc_b = punotc_mean_skeptical)) %>%
  mutate(VoD_naive_median = VoD_naive(pu_a = PU_median_concerned,
                                    pu_b = PU_median_skeptical,
                                    puc_a = PUC_median_concerned,
                                    puc_b = PUC_median_skeptical,
                                    pc_a = PC_median_concerned,
                                    pc_b = PC_median_skeptical,
                                    punotc_a = punotc_median_concerned,
                                    punotc_b = punotc_median_skeptical)) %>%
  ungroup()

#### bootstrapped standard errors####

#### impute raw to completion####

#### Effect of C - pct changes####

#### write sheet####
names(composite_sheet) <- gsub("VoI_naive", "VoI (naive)", names(composite_sheet))
names(composite_sheet) <- gsub("punotc", "P(U|¬C)", names(composite_sheet))
names(composite_sheet) <- gsub("PUC", "P(U|C)", names(composite_sheet))
names(composite_sheet) <- gsub("PU", "P(U)", names(composite_sheet))
names(composite_sheet) <- gsub("PC", "P(C)", names(composite_sheet))
# comment below line after writing so as not to accidentally overwrite formatted changes
#write_sheet(composite_sheet, url, sheet = "Composite")

#### Effect of C table ####
# concerned_positive = # effect_c_standardized > 0,
# concerned_negative = # effect_c_standardized < 0,
# concerned_zero = # effect_c_standardized == 0
# etc...

effect_positive <- aggregate(effect_c_standardized > 0 ~ Camp + C_id, sum, data = p8_composite)
names(effect_positive) <- c("Camp", "C_id", "PositiveEffect")
effect_negative <- aggregate(effect_c_standardized < 0 ~ Camp + C_id, sum, data = p8_composite)
names(effect_negative) <- c("Camp", "C_id", "NegativeEffect")
effect_zero <- aggregate(effect_c_standardized == 0 ~ Camp + C_id, sum, data = p8_composite)
names(effect_zero) <- c("Camp", "C_id", "ZeroEffect")

effects <- effect_positive %>%
  left_join(effect_negative) %>%
  left_join(effect_zero) %>%
  rename("Id" = "C_id")
effects$Id <- as.character(effects$Id)

effects_concerned <- effects %>%
  filter(Camp == "Concerned") %>%
  rename("PositiveEffect_concerned" = "PositiveEffect") %>%
  rename("NegativeEffect_concerned" = "NegativeEffect") %>%
  rename("ZeroEffect_concerned" = "ZeroEffect") %>%
  select(-Camp)
effects_skeptical <- effects %>%
  filter(Camp == "Skeptical") %>%
  rename("PositiveEffect_skeptical" = "PositiveEffect") %>%
  rename("NegativeEffect_skeptical" = "NegativeEffect") %>%
  rename("ZeroEffect_skeptical" = "ZeroEffect") %>%
  select(-Camp)

composite_sheet <- composite_sheet %>%
  left_join(effects_concerned) %>%
  left_join(effects_skeptical)

effect_tab <- composite_sheet %>%
  select(Id, Tag, effect_c_standardized_abs_median_concerned, PositiveEffect_concerned, NegativeEffect_concerned, ZeroEffect_concerned, effect_c_standardized_abs_median_skeptical, PositiveEffect_skeptical, NegativeEffect_skeptical, ZeroEffect_skeptical) %>%
  arrange(-effect_c_standardized_abs_median_concerned)

# write_sheet(effect_tab, ss=url, sheet="Effect of C differences table")
