library(tidyverse)
library(stringr)
library(dplyr)
library(here)

# Load Data --------------------------------------------------------------------
wiki_data_raw <- read_csv(here("inputs/data/wiki_data_raw.csv"))

# Clean Party Variable ---------------------------------------------------------
# Define target parties to include in cleaned data
target_parties <- 
  c(
    "Liberal Party",
    "Green Party",
    "New Democratic Party",
    "Conservative Party",
    "Bloc Québécois"
  )

# Define parties that contain similar names to, but are not affiliated with, target (or related) parties
to_remove_parties <-
  c(
    "Northwest Territories Liberal-Conservative Party",
    "Nationalist Conservative",
    "Liberal Party of Australia",
    "Scottish Green Party",
    "Australian Greens",
    "Freedom Conservative Party of Alberta",
    "Independent Conservative",
    "Liberal Unionist Party",
    "Estonian Reform Party",
    "Reform movement",
    "United Conservative Party"
  )

# Select only politicians from target parties
wiki_data_clean_party <-
  wiki_data_raw |>
  
  # Remove parties in to_remove_parties
  filter(!(partyLabel %in% to_remove_parties)) |>
  
  # Recode local/regional parties to their federal party name and combine merged parties
  mutate(
    party =
      case_when(
        str_detect(partyLabel, "(?i)Liberal.*Party") ~ "Liberal Party",
        str_detect(partyLabel, "(?i)libèral") ~ "Liberal Party",
        str_detect(partyLabel, "(?i)Green") ~ "Green Party",
        str_detect(partyLabel, "(?i)New.*Democratic") ~ "New Democratic Party",
        # Progressive Conservative Party and Canadian Alliance merged to form Conservative Party
        str_detect(partyLabel, "(?i)Progressive.*Conservative") ~ "Conservative Party",
        str_detect(partyLabel, "(?i)Canadian.*Alliance") ~ "Conservative Party",
        # Reform Party is a predecessor of Canadian Alliance
        str_detect(partyLabel, "(?i)Reform.*Party") ~ "Conservative Party",
        # Liberal-Conservative Party is predecessor of modern Conservative Party
        str_detect(partyLabel, "(?i)Liberal-Conservative.*Party") ~ "Conservative Party",
        str_detect(partyLabel, "(?i)Conservative") ~ "Conservative Party",
        str_detect(partyLabel, "(?i)Bloc.*Québécois.") ~ "Bloc Québécois",
        TRUE ~ partyLabel
      )
  ) |>
  
  # Filter for party names matching target parties
  filter(party %in% target_parties)

# Create indicator variables for each party and drop party factor variables
wiki_data_clean_party <-
  wiki_data_clean_party |>
  mutate(
    liberal = as.numeric(party == "Liberal Party"),
    ndp = as.numeric(party == "New Democratic Party"),
    cp = as.numeric(party == "Conservative Party"),
    green = as.numeric(party == "Green Party"),
    bq = as.numeric(party == "Bloc Québécois")
  ) |>
  select(-c(party, partyLabel))

# Clean Position Variable ------------------------------------------------------
# Define target positions to include in cleaned data
target_positions <- 
  c(
    "Prime Minister",
    "Premier",
    "Minister",
    "Party Leader",
    "Senate Member",
    "House Member",
    "Mayor"
  )

# Create indicator variable for target positions
wiki_data_clean_position <-
  wiki_data_clean_party |>
  
  # Recode positions names to match target positions
  mutate(
    position = 
      case_when(
        str_detect(positionLabel, "(?i)Deputy|(?i)Associate|(?i)Spouse|(?i)List") ~ positionLabel,
        str_detect(positionLabel, "(?i)Prime.*Minister") ~ "Prime Minister",
        str_detect(positionLabel, "(?i)Premier") ~ "Premier",
        str_detect(positionLabel, "(?i)Minister") ~ "Minister",
        str_detect(positionLabel, "(?i)Leader.*Party|(?i)Party.*Leader|(?i)Leader.*Opposition") ~ "Party Leader",
        str_detect(positionLabel, "(?i)Member.*of.*Senate") ~ "Senate Member",
        str_detect(positionLabel, "(?i)Member.*of.*House.*Commons") ~ "House Member",
        str_detect(positionLabel, "(?i)Mayor") ~ "Mayor",
        TRUE ~ positionLabel
      )
  ) |>
  
  # Create indicator variables for each position and drop position factor variable
  mutate(
    prime_minister = as.numeric(position == "Prime Minister"),
    premier = as.numeric(position == "Premier"),
    minister = as.numeric(position == "Minister"),
    party_leader = as.numeric(position == "Party Leader"),
    senate_member = as.numeric(position == "Senate Member"),
    house_member = as.numeric(position == "House Member"),
    mayor = as.numeric(position == "Mayor")
  ) |>
  select(-position)

# Merge Politician Observations ------------------------------------------------
wiki_data_clean <-
  wiki_data_clean_position |>
  
  # Group by individual politician ID
  group_by(polit) |>
  
  # Merge indicator variables from multiple observations of each politician
  summarize(
    name = politLabel,
    gender = genderLabel,
    birthyear = birthyear,
    deathyear = deathyear,
    liberal = as.numeric(0 < sum(liberal)),
    ndp = as.numeric(0 < sum(ndp)),
    cp = as.numeric(0 < sum(cp)),
    green = as.numeric(0 < sum(green)),
    bq = as.numeric(0 < sum(bq)),
    prime_minister = as.numeric(0 < sum(prime_minister)),
    minister = as.numeric(0 < sum(minister)),
    party_leader = as.numeric(0 < sum(party_leader)),
    senate_member = as.numeric(0 < sum(senate_member)),
    house_member = as.numeric(0 < sum(house_member)),
    mayor = as.numeric(0 < sum(mayor))
  ) |>
  
  # Remove duplicate observations
  distinct()

# Resolve Conflicting Birth and Death Dates ------------------------------------
# Get list of politicians with multiple recorded years of birth/death
polits_multi_dates <-
  wiki_data_clean |>
  group_by(polit) |>
  summarize(
    name = name,
    birth = birthyear,
    death = deathyear,
    count = n()
  ) |>
  filter(count > 1)

polits_multi_dates <- unique(polits_multi_dates$polit)

# Recode birth/death years of these politicians as NA (missing)
wiki_data_clean <-
  wiki_data_clean |>
  mutate(
    birthyear = if_else(polit %in% polits_multi_dates, NA_real_, birthyear),
    deathyear = if_else(polit %in% polits_multi_dates, NA_real_, deathyear)
  ) |>
  
  # Remove duplicate observations of multiple-birth/death year politicians
  distinct()

# If politician belongs to > 1 party, recode to only include their most recent party
# 81 Cases of multiple party affiliation
multi_parties <-
  wiki_data_clean |>
  mutate(multi = green + liberal + cp + bq + ndp) |>
  filter(multi > 1)

party_map <-
  c(
    "http://www.wikidata.org/entity/Q128655" = "cp",        # Robert Borden
    "http://www.wikidata.org/entity/Q128677" = "cp",        # Mackenzie Bowell
    "http://www.wikidata.org/entity/Q128702" = "cp",        # John A. Macdonald
    "http://www.wikidata.org/entity/Q14915601" = "cp"       # James Alexander Calder
  )


# Save Cleaned Data ------------------------------------------------------------
write_csv(x = wiki_data_clean, file = here("inputs/data/wiki_data_clean.csv"))

