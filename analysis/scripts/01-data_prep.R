# Preparing data for analysis

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyselect)
library(purrr)

# Upload data -------------------------------------------------------------

# See README for details
metadata <- read_tsv("inst/data/raw_data/metadata.tsv")
lloq <- read_tsv("inst/data/raw_data/lloq.tsv")
uhplc_data_raw <- read_csv("inst/data/raw_data/uhplc-results.csv")
uhplc_data_raw2 <- read_csv("inst/data/raw_data/uhplc-results_batch2.csv")
sinusitis <- read_csv("inst/data/raw_data/sinusitis.csv")
path_cond <- read_csv("inst/data/raw_data/path-conditions.csv")

# CMS = chronic maxillary sinusitis
# IPR = periosteal reaction on visceral surface of ribs
# present only if active (pulmonary infection)

# Data cleaning -----------------------------------------------------------

# UHPLC-MS/MS results

# removal internal validation samples
uhplc_data <- uhplc_data_raw %>%
  filter(sample != "16b") %>% # sample 16b removed (test sample that differs from 16 in sampling location)
  group_by(sample) %>%
  mutate(across(weight:cocaine_calculus, sum)) %>% # combine 12.1 and 12.2 (split samples from same sampling location - for internal validation)
  filter(sample != "12.2") %>% # remove duplicate
  select(!contains("weight")) %>% # remove unnecessary weights of calculus samples
  rename(theophyl_calc = thyephyl_calc, # fix typos
         theophyl_wash1 = thephyl_wash1,
         theophyl_wash2 = thephyl_wash2,
         theophyl_wash3 = thephyl_wash3)

uhplc_data$sample[uhplc_data$sample == "12.1"] <- "12" # change sample number to 12
uhplc_data$sample <- as.factor(uhplc_data$sample)

uhplc_data_batch1 <- uhplc_data %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>% # replace NAs with 0 - COMMENT OUT IF IT BREAKS DOWNSTREAM ANALYSIS
  left_join(select(metadata, sample, batch1_weight), by = "sample") %>%
  #mutate(across(where(is.numeric), ~ .x / batch1_weight)) %>% # convert quantity to ng/mg calculus
  rename(cocaine_calc = cocaine_calculus)


uhplc_data_batch2 <- uhplc_data_raw2 %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>% # replace NAs with 0 - COMMENT OUT IF IT BREAKS DOWNSTREAM ANALYSIS
  rename(
    theophyl_calc = theophylline_calc,
    theophyl_wash1 = theophylline_wash1,
    theophyl_wash2 = theophylline_wash2,
    theophyl_wash3 = theophylline_wash3
    ) %>%
  mutate(sample = as.factor(sample)) %>%
  select(!contains("weight")) %>%
  left_join(select(metadata, sample, batch2_weight), by = "sample") #%>%
  #mutate(across(where(is.numeric), ~ .x / batch2_weight)) # convert quantity to ng/mg calculus

# Combine batches 1 and 2
uhplc_data_comb <- uhplc_data_batch1 %>%
  full_join(uhplc_data_batch2, by = "sample", suffix = c("_batch1", "_batch2"))

#uhplc_calculus <- uhplc_data_comb %>%
  #mutate(across(where(is.numeric), ~ .x / batch2_weight)) %>% # convert quantity to ng/mg calculus
  #select(sample, contains("calc"))


# Sinusitis data

sinusitis_clean <- sinusitis %>% # convert yes/no to true/false
  mutate(across(CMS:IPR,
                function(x) case_when(x == "YES" ~ TRUE,
                                      x == "NO" ~ FALSE)))

# Pathological conditions

path_cond_clean <- path_cond %>%
  mutate(across(
    -id,
    function(x) case_when(x == "Y" ~ TRUE,
                          x == "Y?" ~ FALSE, # conservative approach - questioned lesions considered absent
                          x == "N" ~ FALSE)
                )
         )


# Export data -------------------------------------------------------------

# write cleaned data to data/derived_data
write_csv(uhplc_data_comb, "inst/data/derived_data/uhplc-data_combined.csv")
#write_csv(uhplc_calculus, "inst/data/derived_data/uhplc-calculus_cleaned.csv")
write_csv(sinusitis_clean, "inst/data/derived_data/sinusitis_cleaned.csv")
write_csv(path_cond_clean, "inst/data/derived_data/path-conditions_cleaned.csv")

#write_csv(efa_data, "inst/data/derived_data/efa-analysis_data.csv")
