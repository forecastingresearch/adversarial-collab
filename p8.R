# This document is in-progress.

#### Setup####
rm(list = ls())
# replace with own dir
yourHome <- "~/Documents/FRI"
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
p8_composite <- read_sheet(url, "Raw")
names(p8_composite) <- c("Name", "Camp", "PU", "C_id", "PC", "PUC")

#### Get CID tags from P8 composite - Composite####
cid_tags <- read_sheet(url, "Composite [old]")
names(cid_tags) <- unlist(cid_tags[1, ])
cid_tags <- cid_tags %>% select(Id, Tag)
cid_tags$Id <- unlist(cid_tags$Id)
cid_tags <- cid_tags[2:nrow(cid_tags), ]

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

for (i in 1:length(C_ids)) {
  cor_tbl_id <- cor_table %>% filter(C_id == C_ids[i])
  correlate <- cor(cor_tbl_id$PU, cor_tbl_id$PC, method = "spearman")
}

write_sheet(composite_sheet, url, sheet = "Composite")

# P(U|C),
# P(U|¬C),
# Effect of C,
# VoI,
# VoD (naive)

#### Effect of C - plain rank and standardized rank####

#### VoD naive - standardized, ranked####

#### Reproduce "other quant analyses from doc in notes####

#### bootstrapped standard errors####

#### impute raw to completion####

#### Effect of C - pct changes####
