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

#P8 composite sheet will be parsed as _decimals_ rather than 0-100 percentages.
p8_composite = read_sheet(url, "Raw")
names(p8_composite) = c("Name", "Camp", "PU", "C_id", "PC", "PUC")

#### Get CID tags from P8 composite - Composite####
cid_tags = read_sheet(url, "Composite")
names(cid_tags) = unlist(cid_tags[1,])
cid_tags = cid_tags %>% select(Id, Tag)
cid_tags$Id = unlist(cid_tags$Id)
cid_tags = cid_tags[2:nrow(cid_tags),]

#### Initialize composite sheet with Id and Tag####

#### Add columns for composite sheet, grouped by Concerned/Skeptical... 
#P(U), 
#P(C), 
#U-c correlation, 
#P(U|C), 
#P(U|¬C), 
#Effect of C, 
#VoI, 
#VoD (naive)

#### Effect of C - plain rank and standardized rank####

#### VoD naive - standardized, ranked####

#### Reproduce "other quant analyses from doc in notes####

#### bootstrapped standard errors####

#### impute raw to completion####

#### Effect of C - pct changes####
