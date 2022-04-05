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

# Return the total number of Wikipedia page edits from 2021/01/01 to 2022/01/01
get_page_edits <- function(page_name, page_lang) {
  # Format an API query for the number of monthly edits of page_name
  prefix <- 'https://wikimedia.org/api/rest_v1/metrics/edits/per-page/'
  lang <- paste0(page_lang, '.wikipedia.org/')
  suffix <- '/all-editor-types/monthly/20210101/20220101'
  response_url <- paste0(prefix, lang, page_name, suffix)
  
  # Get the API response
  response <- httr::GET(url = response_url)
  
  # If response status is not success, return NA
  if (response$status_code != 200) { return(NA) }
  
  # Get the number of edits from response data
  edits <-
    # Unpack the response lists to get the list of monthly edits 
    lapply(content(response)$items, "[", "results")[[1]][[1]] |>
    lapply("[", "edits") |>
    unlist() |>
    
    # Sum the monthly edits to get the year total
    sum()
  
  return(edits)
}

# Return the total number of Wikipedia page views from 2021/01/01 to 2022/01/01
get_page_views <- function(page_name, page_lang) {
  # Format an API query for the number of monthly views of page_name
  prefix <- 'https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/'
  lang <- paste0(page_lang, '.wikipedia.org/')
  suffix <- '/monthly/20210101/20220101'
  response_url <- paste0(prefix, lang, "all-access/all-agents/", page_name, suffix)
  
  # Get the API response
  response <- httr::GET(url = response_url)
  
  # If response status is not success, return NA
  if (response$status_code != 200) { return(NA) }
  
  # Get the number of views from response data
  views <- 
    # Unpack the response lists to get the list of monthly views 
    lapply(content(response)$items, "[", "views") |> 
    unlist() |>
    
    # Sum the monthly views to get the year total
    sum()
  
  return(views)
}

# Return the number of Wikipedia reverts from 2021/01/01 to 2022/01/01 and page size in characters
get_page_reverts_size <- function(page_name, page_lang, time = 15) {
  # Format a Xtools URL for the webpage containing article info in page_name
  prefix <- "https://xtools.wmflabs.org/articleinfo/"
  lang <- paste0(page_lang, ".wikipedia.org/")
  suffix <- "/2021-01-01/2022-01-01"
  url <- paste0(prefix, lang, page_name, suffix)
  
  # Close the connection to the URL upon function exit
  on.exit(close(url))
  
  # Pause between function calls (recommended for high volume scraping)
  Sys.sleep(time = time)
  
  # Read the page HTML and extract the HTML tables
  html <- read_html(url)
  html_tables <- html_elements(html, "table")
  
  # Get the number of reverted edits of page_name
  reverted_edits <- html_table(x = html_tables[[2]]) |> filter(X1 == "Reverted edits")
  reverted_edits <- 
    # Remove Any Commas from Count and Convert to Numeric
    gsub(",", "", reverted_edits[[2]]) |>
    as.numeric()
  
  # Get the size of page_name in characters
  pagesize <- html_table(x = html_tables[[5]]) |> filter(X1 == "Characters")
  pagesize <- 
    # Remove Any Commas from Count and Convert to Numeric
    gsub(",", "", pagesize[[2]]) |>
    as.numeric()
  
  return(c(reverted_edits, pagesize))
}

# Load Cleaned Politicians Data ------------------------------------------------
wiki_data_clean <- read_csv(file = here("inputs/data/wiki_data_clean.csv"))

# Get the Wikipedia Page Associated with Each Politician -----------------------
# Get the Wikidata QIDs of Each Politician
wiki_data_clean <- 
  wiki_data_clean |>
  mutate(qid = gsub(x = polit, pattern = ".*(Q.*)", replacement = "\\1"))

# Get the URLs of each Wikipedia Page (if available)
wiki_data_page_names <-
  wiki_data_clean |>
  rowwise() |>
  mutate(url = get_page(qid))

# Define the page name and page language (English or French) from the URLs
wiki_data_page_names <-
  wiki_data_page_names |>
  mutate(
    page_name = gsub(x = url, pattern = ".*wikipedia.org/wiki/(.*)", replacement = "\\1"),
    page_lang = gsub(x = url, pattern = "https://(.{2}).*", replacement = "\\1")
  )

# Get Wikipedia Page Views and Edits Data --------------------------------------
# Get total edits and page views
wiki_data_page_stats <-
  wiki_data_page_names |>
  rowwise() |>
  mutate(
    page_edits = get_page_edits(page_name, page_lang),
    page_views = get_page_views(page_name, page_lang)
  )

# Get the Page Reverted Edits Data ---------------------------------------------
# Create a tibble with empty rows for page reverts and page size in characters
wiki_data_page_names <-
  wiki_data_page_stats |>
  select(qid, page_name, page_lang, page_edits) |>
  mutate(
    reverts = NA,
    page_chars = NA
  )

# Iterate through every politician in the dataset, getting their page reverts and page size
num_rows <- dim(wiki_data_page_names)[1]

# NOTE -> YOU HAVE LIMITED TO FIRST 1000 ROWS
num_rows <- 1:1000
for (row in num_rows) {
  # Set page reverts to 0 and continue to next politician if page_edits == 0
  if( wiki_data_page_names$page_edits[[row]] == 0) {
    wiki_data_page_names$reverts[[row]] <- 0
    next
  }
  
  # Get the page name and language of the politician's Wikipedia Page
  page_name <- wiki_data_page_names$page_name[[row]]
  page_lang <- wiki_data_page_names$page_lang[[row]]
  print(row)
  
  # Attempt to get the page reverts and size data
  page_data <- try(get_page_reverts_size(page_name, page_lang, time = 15))
  
  # If attempt successful, add page reverts and size to wiki_data_page_names
  if (class(page_data) != "try-error") {
    wiki_data_page_names$reverts[[row]] <- page_data[1]
    wiki_data_page_names$page_chars[[row]] <- page_data[2]
  }
}

# Some Regressions -------------------------------------------------------------
wiki_data <- read_csv(file = here("inputs/data/wiki_data_page_stats.csv"))
wiki_data <- wiki_data |> mutate(gender = if_else(gender == "male", 1, 0))
women <- wiki_data |> filter(gender == 0) |> arrange(birthyear)

pms <- wiki_data |> filter(prime_minister == 1) |> arrange(page_views)

mod <- 
  lm(
    formula = page_views ~ . - 1, 
    data = wiki_data |> 
      filter(prime_minister == 0) |>
      select(
        page_views,
        gender, 
        liberal, 
        ndp, 
        green, 
        bq, 
        cp, 
        minister,
        house_member,
        senate_member,
        mayor,
        party_leader
        )
    )

summary(mod)

wiki_data_living <-
  wiki_data |>
  filter(!is.na(birthyear) & is.na(deathyear)) |>
  filter(birthyear > 1922) |>
  mutate(age = 2022 - birthyear)

mod <- 
  lm(
    formula = page_views ~ . + I(age^2), 
    data = wiki_data_living |> 
      filter(prime_minister == 0, minister == 0, party_leader == 0) |>
      select(
        page_views,
        gender,
        ndp, 
        green, 
        bq, 
        cp, 
        minister,
        house_member,
        senate_member,
        mayor,
        party_leader,
        age
      )
  )

summary(mod)
