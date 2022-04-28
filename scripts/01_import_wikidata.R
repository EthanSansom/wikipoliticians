# Preamble ---------------------------------------------------------------------
# Purpose: Import the full dataset of Canadian Politicians available on Wikidata
# who have a recorded political position and political party. Retrieve additional
# optional variables for politicians' year of birth/death and gender.
# Author: Ethan Sansom
# Contact: ethan.sansom@mail.utotoronto.ca
# Date: April 27, 2022

# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(WikidataQueryServiceR)  # For accessing Wikidata Query Service API
library(here)                   # For file path management

#  Import Data -----------------------------------------------------------------
# Define the SPARQL (Sparkle) query for the Wikidata Query Service API, which
# returns the desired set of Canadian Politicians
query <-
  '# Return the Wikidata ID, Name, Political Position(s), Gender, Birthdate (Year), 
   # Deathdate (Year), and Political Party(s) of all Canadian Politicians in the Wikidata Database
   SELECT ?polit ?politLabel ?positionLabel ?genderLabel (year(?birthdate) as ?birthyear) (year(?deathdate) as ?deathyear) ?partyLabel
   WHERE 
   {
    # Filter all WikiData Entries
    ?polit wdt:P31 wd:Q5.                  # Must be a (non-fictional) human
    ?polit wdt:P27 wd:Q16.                 # Must be a Canadian citizen
    ?polit wdt:P106 wd:Q82955.             # Must be a polititian
  
    # Define and Retrieve Variables
    ?polit wdt:P39 ?position.              # Define and get political position held variable (required)
    ?polit wdt:P102 ?party.                # Define and get political party affiliation variable (required)
    OPTIONAL{?polit wdt:P21 ?gender.}      # Define and get gender variable (optional)
    OPTIONAL{?polit wdt:P569 ?birthdate.}  # Define and get birthdate variable (optional)
    OPTIONAL{?polit wdt:P570 ?deathdate.}  # Define and get deathdate variable (optional)

    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". } # Provide English Labels to Variables
  }'

# Load the dataset of all Canadian Politicians in the Wikidata database
wiki_data_raw <- query_wikidata(query)

# Save the dataset
write_csv(x = wiki_data_raw, file = here("inputs/data/wiki_data_raw.csv"))
