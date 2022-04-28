# Preamble ---------------------------------------------------------------------
# Purpose: Clean the data imported using 03_import_pagedata.R.
# Author: Ethan Sansom
# Contact: ethan.sansom@mail.utotoronto.ca
# Date: April 27, 2022
# Precondition: Run 03_import_pagedata.R

# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(here)           # For file path management

# Load and Clean Page Data -----------------------------------------------------
wiki_data <- read_csv(here("inputs/data/wiki_data_pages.csv"))

# Get the total page views for each politician
wiki_data <-
  wiki_data |>
  rowwise() |>
  mutate(page_views = sum(across(starts_with("views"), na.rm = TRUE)))

# Find politicians which have been assigned multiple party affiliations
multi_party_polits <-
  wiki_data |>
  mutate(num_parties = cp + liberal + green + ndp + bq) |>
  filter(num_parties > 1) |>
  select(liberal, cp, ndp, green, ndp, name, qid, url)

# Assign each politician in multi_party_polits to their most recent party
# NOTE: There are 81 such politicians of the 7856 in the sample, for which I have
# manually altered their affiliation. In future analysis, with a different sample,
# you may wish to simply drop these politicians or accept multi-party affiliation.

party_map <-
  c(
    "https://en.wikipedia.org/wiki/Robert_Borden" = "cp",
    "https://en.wikipedia.org/wiki/Mackenzie_Bowell" = "cp",
    "https://en.wikipedia.org/wiki/John_A._Macdonald" = "cp",
    "https://en.wikipedia.org/wiki/James_Alexander_Calder" = "cp",
    "https://en.wikipedia.org/wiki/Robert_James_Manion" = "cp",
    "https://en.wikipedia.org/wiki/William_Thomas_White" = "cp",
    "https://en.wikipedia.org/wiki/William_Evan_Price" = "cp",
    "https://en.wikipedia.org/wiki/Wajid_Khan_(Canadian_politician)" = "cp",
    "https://en.wikipedia.org/wiki/Sylvester_Perry_Ryan" = "cp",
    "https://en.wikipedia.org/wiki/Stephen_Harper" = "cp",
    "https://en.wikipedia.org/wiki/Stan_Woloshyn" = "cp",
    "https://en.wikipedia.org/wiki/Scott_Brison" = "liberal",
    "https://en.wikipedia.org/wiki/Sandra_Jansen" = "ndp",
    "https://en.wikipedia.org/wiki/Robert_Muir_(politician)" = "cp",
    "https://en.wikipedia.org/wiki/Robert_Lanct%C3%B4t" = "bq",
    "https://en.wikipedia.org/wiki/Rick_Laliberte" = "liberal",
    "https://en.wikipedia.org/wiki/Richard_John_Cartwright" = "liberal",
    "https://en.wikipedia.org/wiki/Raymond_Rock" = "cp",
    "https://en.wikipedia.org/wiki/Raymond_Cho_(politician)" = "liberal",
    "https://en.wikipedia.org/wiki/Rathika_Sitsabaiesan" = "liberal",
    "https://en.wikipedia.org/wiki/Ralph_Stewart_(Canadian_politician)" = "cp",
    "https://en.wikipedia.org/wiki/Raj_Sherman" = "liberal",
    "https://en.wikipedia.org/wiki/Pierrette_Venne" = "bq",
    "https://en.wikipedia.org/wiki/Pierre_Nantel" = "green",
    "https://en.wikipedia.org/wiki/Peter_Ittinuar" = "green",
    "https://en.wikipedia.org/wiki/Pauline_Jewett" = "ndp",
    "https://en.wikipedia.org/wiki/Paul_Hellyer" = "liberal",
    "https://en.wikipedia.org/wiki/Nic_Leblanc" = "cp",
    "https://en.wikipedia.org/wiki/Nancy_MacBeth" = "liberal",
    "https://en.wikipedia.org/wiki/Mo%C3%AFse_Houde" = "cp",
    "https://en.wikipedia.org/wiki/Maria_Mourani" = "bq",
    "https://en.wikipedia.org/wiki/Lucien_Bouchard" = "bq",
    "https://en.wikipedia.org/wiki/Louis_Plamondon" = "bq",
    "https://en.wikipedia.org/wiki/Lise_St-Denis" = "liberal",
    "https://en.wikipedia.org/wiki/Lillian_Dyck" = "liberal",
    "https://en.wikipedia.org/wiki/Lex_MacKenzie" = "cp",
    "https://en.wikipedia.org/wiki/Julius_Yankowsky" = "cp",
    "https://en.wikipedia.org/wiki/John_Sandfield_Macdonald" = "cp",
    "https://en.wikipedia.org/wiki/John_Loney" = "liberal",
    "https://en.wikipedia.org/wiki/John_Crosbie" = "cp",
    "https://en.wikipedia.org/wiki/Joe_Comuzzi" = "liberal",
    "https://en.wikipedia.org/wiki/Joan_Beatty" = "liberal",
    "https://en.wikipedia.org/wiki/Jean_Rousseau_(politician)" = "green",
    "https://en.wikipedia.org/wiki/Jean_Lapierre" = "liberal",
    "https://en.wikipedia.org/wiki/James_Domville" = "liberal",
    "https://en.wikipedia.org/wiki/Jacques_Lavoie" = "liberal",
    "https://en.wikipedia.org/wiki/Jack_Horner_(politician)" = "liberal",
    "https://en.wikipedia.org/wiki/Hunter_Tootoo" = "liberal",
    "https://en.wikipedia.org/wiki/Hugh_Guthrie" = "cp",
    "https://en.wikipedia.org/wiki/Honor%C3%A9_Achim" = "liberal",
    "https://en.wikipedia.org/wiki/Hiram_Blanchard" = "liberal",
    "https://en.wikipedia.org/wiki/Guy_St-Julien" = "liberal",
    "https://en.wikipedia.org/wiki/Giorgio_Mammoliti" = "cp",
    "https://en.wikipedia.org/wiki/Gilles_Rocheleau" = "bq",
    "https://en.wikipedia.org/wiki/Gilbert_Chartrand" = "cp",
    "https://en.wikipedia.org/wiki/George_Valentine_McInerney" = "cp",
    "https://en.wikipedia.org/wiki/Gaston_Gourde" = "bq",
    "https://en.wikipedia.org/wiki/Gary_Carr_(politician)" = "liberal",
    "https://en.wikipedia.org/wiki/Garth_Turner" = "liberal",
    "https://en.wikipedia.org/wiki/Fran%C3%A7oise_Boivin" = "ndp",
    "https://en.wikipedia.org/wiki/Fran%C3%A7ois_G%C3%A9rin" = "bq",
    "https://en.wikipedia.org/wiki/Elijah_Harper" = "liberal",
    "https://en.wikipedia.org/wiki/Edmund_Burke_Wood" = "liberal",
    "https://en.wikipedia.org/wiki/Diane_St-Jacques" = "liberal",
    "https://en.wikipedia.org/wiki/David_Price_(Canadian_politician)" = "liberal",
    "https://en.wikipedia.org/wiki/David_Kilgour" = "liberal",
    "https://en.wikipedia.org/wiki/David_Emerson" = "cp",
    "https://en.wikipedia.org/wiki/Claude_Patry" = "bq",
    "https://en.wikipedia.org/wiki/Charles_Bruno_Blondeau" = "cp",
    "https://en.wikipedia.org/wiki/Bud_Miller" = "cp",
    "https://en.wikipedia.org/wiki/Bruce_Hyer" = "ndp",
    "https://en.wikipedia.org/wiki/Bob_Rae" = "liberal",
    "https://en.wikipedia.org/wiki/Blair_Wilson" = "liberal",
    "https://en.wikipedia.org/wiki/Bill_Matthews" = "liberal",
    "https://en.wikipedia.org/wiki/Bill_Dickie_(politician)" = "cp",
    "https://en.wikipedia.org/wiki/Beno%C3%AEt_Tremblay" = "bq",
    "https://en.wikipedia.org/wiki/Belinda_Stronach" = "liberal",
    "https://en.wikipedia.org/wiki/Angela_Vautour" = "cp",
    "https://en.wikipedia.org/wiki/Andrew_B._Ingram" = "cp",
    "https://en.wikipedia.org/wiki/Andr%C3%A9_Harvey" = "liberal",
    "https://en.wikipedia.org/wiki/Alexa_McDonough" = "ndp"
  )

# Get the urls corresponding to each party assignment
party_map_urls <- names(party_map)

# Recode the mulit-party politician's party indicators
for (polit_url in party_map_urls) {
  # Replace every party affiliation indicator of multi-party politicians with 0
  index <- which(wiki_data$url == polit_url)
  wiki_data[index, ][c("liberal", "ndp", "cp", "green", "bq")] <- 0
  
  # Assign a 1 to the politician's new assigned party affiliation
  new_party <- party_map[polit_url]
  wiki_data[index, ][new_party] <- 1
}

# Create a party factor variable
wiki_data <-
  wiki_data |>
  mutate(
    party = case_when(
      cp == 1 ~ "CPC",
      liberal == 1 ~ "Liberal",
      green == 1 ~ "Green",
      ndp == 1 ~ "NDP",
      bq == 1 ~ "BQ"
    )
  )

# Save Cleaned Data ------------------------------------------------------------
# Select desired variables
wiki_data <-
  wiki_data |>
  select(-c(polit, page_name, page_lang, url))

# Test that the number of mulit-party politicians in now 0
multi_party_polits <-
  wiki_data |>
  mutate(num_parties = cp + liberal + green + ndp + bq) |>
  filter(num_parties > 1)

print(c("0 Remaining Multi-Party Politicians:", dim(multi_party_polits)[1] == 0))

# Save data
write_csv(x = wiki_data, file = here("inputs/data/wiki_data_pages_clean.csv"))
