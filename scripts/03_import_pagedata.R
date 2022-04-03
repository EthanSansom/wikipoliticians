# Preamble ---------------------------------------------------------------------
# Purpose: Import pageviews and edits data from 2021-01-01 to 2022-01-01 via the 
# Mediawiki API (for every politician with a Wikipedia page). Additionally, scrape 
# the number of reverted edits over this period (per page) from the XTools website.
# Author: Ethan Sansom
# Contact: ethan.sansom@mail.utotoronto.ca
# Date: 2022-04-03

# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(here)       # For file path management
library(httr)       # For accessing APIs

# Load Cleaned Politicians Data ------------------------------------------------
wiki_data_clean <- read_csv(file = here("inputs/data/wiki_data_clean.csv"))

# Get the Wikipedia Page Associated with Each Politician -----------------------
# Get the Wikidata IDs of Each Politician
polit_ids <- gsub(x = wiki_data_clean$polit, pattern = ".*(Q.*)", replacement = "\\1")

# 'https://www.wikidata.org/w/api.php?action=wbgetentities&props=sitelinks/urlsf'&ids={wikidata_id}&format=json'
# https://www.wikidata.org/w/api.php?action=wbgetentities&format=xml&props=sitelinks&ids=Q19675&sitefilter=frwiki
id <- "Q1000439"
pre_id <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&props=sitelinks/urls&ids='
post_id <- "&sitefilter=enwiki|frwiki"
url <- paste0(pre_id, id, post_id)


test <- httr::GET(url = url)
content(test)

