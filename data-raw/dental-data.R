library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyselect)
library(purrr)

# Download raw data -------------------------------------------------------

uri_metadata <- "https://zenodo.org/record/7648757/files/metadata.tsv"
uri_lloq <- "https://zenodo.org/record/7648757/files/lloq.tsv"
uri_uhplc_data1 <- "https://zenodo.org/record/7648757/files/uhplc-results.csv"
uri_uhplc_data2 <- "https://zenodo.org/record/7648757/files/uhplc-results_batch2.csv"
uri_sinusitis <- "https://zenodo.org/record/7648757/files/sinusitis.csv"
uri_path_cond <- "https://zenodo.org/record/7648757/files/path-conditions.csv"
uri_demography <- "https://zenodo.org/record/7648757/files/demography.csv"
uri_dental_inv <- "https://zenodo.org/record/7648757/files/dental-inv.csv"
uri_caries <- "https://zenodo.org/record/7648757/files/caries.csv"
uri_calculus_full <- "https://zenodo.org/record/7648757/files/calculus_full.csv"
uri_periodont <- "https://zenodo.org/record/7648757/files/periodontitis.csv"
uri_periap <- "https://zenodo.org/record/7648757/files/periapical.csv"

# Upload data -------------------------------------------------------------

names_of_datf <- sort(stringr::str_extract(ls(), "(?<=uri_).+"))

uri_all <- c(
  uri_calculus_full,
  uri_caries,
  uri_demography,
  uri_dental_inv,
  uri_lloq,
  uri_metadata,
  uri_path_cond,
  uri_periap,
  uri_periodont,
  uri_sinusitis,
  uri_uhplc_data1,
  uri_uhplc_data2
  )

data_raw <- sapply(uri_all, read_delim)
names(data_raw) <- names_of_datf

#metadata <- data_raw$metadata
#lloq <- data_raw$lloq

list2env(data_raw, .GlobalEnv)

# Data cleaning -----------------------------------------------------------

# UHPLC-MS/MS results

# removal internal validation samples
uhplc_data <- uhplc_data1 %>%
  filter(sample != "16b") %>% # sample 16b removed (test sample that differs from 16 in sampling location)
  group_by(sample) %>%
  mutate(across(weight:cocaine_calculus, sum)) %>% # combine 12.1 and 12.2 (split samples from same sampling location - for internal validation)
  filter(sample != "12.2") %>% # remove duplicate
  rename(
    theophyl_calc = thyephyl_calc,
    theophyl_wash1 = thephyl_wash1,
    theophyl_wash2 = thephyl_wash2,
    theophyl_wash3 = thephyl_wash3,
    cocaine_calc = cocaine_calculus
  ) %>%
  select(!contains("weight"))

uhplc_data$sample[uhplc_data$sample == "12.1"] <- "12" # change sample number to 12
uhplc_data$sample <- as.factor(uhplc_data$sample)

uhplc_data_batch1 <- uhplc_data %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>% # replace NAs with 0 - COMMENT OUT IF IT BREAKS DOWNSTREAM ANALYSIS
  left_join(select(data_raw$metadata, sample, batch1_weight), by = "sample")
  #mutate(across(where(is.numeric), ~ .x / batch1_weight)) %>% # convert quantity to ng/mg calculus


uhplc_data_batch2 <- uhplc_data2 %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>% # replace NAs with 0 - COMMENT OUT IF IT BREAKS DOWNSTREAM ANALYSIS
  rename(
    theophyl_calc = theophylline_calc,
    theophyl_wash1 = theophylline_wash1,
    theophyl_wash2 = theophylline_wash2,
    theophyl_wash3 = theophylline_wash3
  ) %>%
  mutate(sample = as.factor(sample)) %>%
  select(!contains("weight")) %>%
  left_join(select(data_raw$metadata, sample, batch2_weight), by = "sample") #%>%
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

usethis::use_data(metadata, overwrite = TRUE)
usethis::use_data(lloq, overwrite = TRUE)
usethis::use_data(demography, overwrite = TRUE)
usethis::use_data(uhplc_data_comb, overwrite = TRUE)
usethis::use_data(path_cond_clean, overwrite = TRUE)
usethis::use_data(sinusitis_clean, overwrite = TRUE)
usethis::use_data(caries, overwrite = TRUE)
usethis::use_data(calculus_full, overwrite = TRUE)
usethis::use_data(periodont, overwrite = TRUE)
usethis::use_data(periap, overwrite = TRUE)
usethis::use_data(dental_inv, overwrite = TRUE)

#write_csv(uhplc_data_comb, "inst/data/derived_data/uhplc-data_combined.csv")
#write_csv(sinusitis_clean, "inst/data/derived_data/sinusitis_cleaned.csv")
#write_csv(path_cond_clean, "inst/data/derived_data/path-conditions_cleaned.csv")

