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
library(rvest)      # For web-scraping

# Define Functions -------------------------------------------------------------
# Return the Wikipedia page URL associated with a Wikidata QID
get_page <- function(qid){
  # Format an API query for the Wikipedia page corresponding to Wikidata qid
  # Appropriate API request with help from:
  # https://stackoverflow.com/questions/37079989/how-to-get-wikipedia-page-from-wikidata-id
  prefix <- 'https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&props=sitelinks/urls&ids='
  suffix <- "&sitefilter=enwiki|frwiki"
  response_url <- paste0(prefix, qid, suffix)
  
  # Get the API response
  response <- httr::GET(url = response_url)
  
  # Get the English and French Wikipedia Page URLs
  page_url_en <- content(response)$entities[[qid]]$sitelinks$enwiki$url
  page_url_fr <- content(response)$entities[[qid]]$sitelinks$frwiki$url
  
  # Return NA if neither URL exists
  if (is.null(page_url_en) & is.null(page_url_fr)) { return(NA) }
  
  # Return English page URL if available, French page URL otherwise
  page_url <- ifelse(is.null(page_url_en), page_url_fr, page_url_en)
  return(page_url)
}

# Return the monthly number of Wikipedia page views from 2021/01/01 to 2022/01/01
get_page_views <- function(page_name, page_lang) {
  # Format an API query for the number of monthly views of page_name
  prefix <- 'https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/'
  lang <- paste0(page_lang, '.wikipedia.org/')
  suffix <- '/monthly/20210101/20220101'
  response_url <- paste0(prefix, lang, "all-access/user/", page_name, suffix)
  
  # Get the API response
  response <- httr::GET(url = response_url)
  
  # If response status is not success, return NA
  if (response$status_code != 200) { return(NA) }
  
  # Get the number of views from response data
  views <- 
    # Unpack the response lists to get the list of monthly views 
    lapply(content(response)$items, "[", "views") |>
    unlist()
  
  # If Wikipedia page created during period, pre-pend 0's to views list
  pre_views <- rep(0, 12 - length(views))
  views <- c(pre_views, views)

  return(views)
}

# Load Cleaned Politicians Data ------------------------------------------------
wiki_data_clean <- read_csv(file = here("inputs/data/wiki_data_clean.csv"))

# Get the Wikipedia Page Associated with Each Politician -----------------------
# Get the Wikidata QIDs of Each Politician
wiki_data_clean <- 
  wiki_data_clean |>
  mutate(qid = gsub(x = polit, pattern = ".*(Q.*)", replacement = "\\1"))

# Get the URLs, page name, and language of each Wikipedia page (if available)
wiki_data_pages <-
  wiki_data_clean |>
  rowwise() |>
  mutate(
    url = get_page(qid),
    page_name = gsub(x = url, pattern = ".*wikipedia.org/wiki/(.*)", replacement = "\\1"),
    page_lang = gsub(x = url, pattern = "https://(.{2}).*", replacement = "\\1")
    )

# Create empty columns for monthly page views (over a 12 month period)
view_col_names <- paste0("views_m", 1:12)
wiki_data_pages[view_col_names] <- NA

# Get monthly page views from 2021/01/01 to 2022/01/01
num_rows <- dim(wiki_data_pages)[1]
for (row in 1:num_rows) {
  p_name <- wiki_data_pages[row, ]$page_name
  p_lang <- wiki_data_pages[row, ]$page_lang
  wiki_data_pages[row, ][view_col_names] <- as.list(get_page_views(p_name, p_lang))
}

# Save Data
write_csv(x = wiki_data_pages, file = here("inputs/data/wiki_data_pages.csv"))
