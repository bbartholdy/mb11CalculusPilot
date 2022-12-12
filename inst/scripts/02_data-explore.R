# Soft-deprecated
# should this all be done in the report?
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Upload data -------------------------------------------------------------

metadata <- read_tsv("inst/data/raw_data/metadata.tsv")
lloq <- read_tsv("inst/data/raw_data/lloq.tsv")
uhplc_calculus <- read_tsv("inst/data/derived_data/uhplc-data_cleaned.csv")
dental_inv <- read_csv("inst/data/raw_data/dental-inv.csv")
caries <- read_csv("inst/data/raw_data/caries.csv")
periodont <- read_csv("inst/data/raw_data/periodontitis.csv")
periap <- read_csv("inst/data/raw_data/periapical.csv")
calculus <- read_csv("inst/data/raw_data/calculus.csv")
demography <- read_csv("inst/data/raw_data/demography.csv")
sinusitis <- read_csv("inst/data/derived_data/sinusitis_cleaned.csv")


# Convert to long ---------------------------------------------------------

dental_inv_long <- dental_inv %>% # Inventory
  right_join(demography) %>%
  select(!c(occupation, own_grave, tax, pipe_notch)) %>%
  pivot_longer(t11:t48, names_to = "tooth",
               values_to = "status") %>%
  mutate(status = case_when(status == "dna" ~ "m", # teeth missing due to DNA sampling
                            TRUE ~ status))

teeth_list <- list(caries, periodont, periap, calculus)

test<- lapply(
  teeth_list,
  pivot_longer,
  cols = t11:t48,
  names_to = "tooth",
  values_to = "score"
)

caries_long <- caries %>% # Caries
  pivot_longer(t11:t48, names_to = "tooth",
               values_to = "caries_score")

periodont_long <- periodont %>% # Periodontitis
  pivot_longer(cols = t11:t48,
               names_to = "tooth",
               values_to = "periodont_score")

periap_long <- periap %>% # Periapical lesions
  pivot_longer(cols = t11:t48,
               names_to = "tooth",
               values_to = "periap_score")

calculus_long <- calculus %>% # Calculus
  pivot_longer(cols = t11:t48,
               names_to = "tooth",
               values_to = "calculus_score")

# prepare to merge data frames
teeth_list <- list(dental_inv_long, caries_long, periodont_long,
                   periap_long, calculus_long)

# Long dental frame

dental_long <- teeth_list %>%
  reduce(inner_join, by = c("id", "tooth")) %>%
  tooth_position()
