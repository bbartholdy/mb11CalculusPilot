# Preparing data for analysis

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyselect)
library(purrr)

# Upload data -------------------------------------------------------------

# See README for details
metadata <- read_tsv("inst/data/raw_data/metadata.tsv") # ids need to be changed to match key
lloq <- read_tsv("inst/data/raw_data/lloq.tsv")
uhplc_data_raw <- read_csv("inst/data/raw_data/uhplc-results.csv")
uhplc_data_raw2 <- read_csv("inst/data/raw_data/uhplc-results_batch2.csv")
dental_inv <- read_csv("inst/data/raw_data/dental-inv.csv")
sinusitis <- read_csv("inst/data/raw_data/sinusitis.csv")
path_cond <- read_csv("inst/data/raw_data/path-conditions.csv")
caries <- read_csv("inst/data/raw_data/caries.csv")
periodont <- read_csv("inst/data/raw_data/periodontitis.csv")
periap <- read_csv("inst/data/raw_data/periapical.csv")
calculus <- read_csv("inst/data/raw_data/calculus.csv")
calculus_full <- read_csv("inst/data/raw_data/calculus_full.csv")
demography <- read_csv("inst/data/raw_data/demography.csv")


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


# Compounds presence/absence

# uhplc_calculus_bin <- uhplc_calculus %>%
#   mutate(
#     across(
#       where(is.numeric),
#       function(x) if_else(!is.na(x), T, F) # convert concentration to boolean
#       )
#     ) %>%
#   pivot_longer( # convert to long form to include only compounds present or absent in both batches
#     cols = -sample,
#     names_to = c("compound", "batch"),
#     names_pattern = "(.*)_(.*)",
#     values_to = c("value")
#     ) %>%
#     mutate(compound = str_remove(compound, "_calc"), # redundant suffix
#            value = as.numeric(value)) %>% # convert to 0s and 1s
#     group_by(sample, compound) %>%
#     summarise(value = sum(value))

# create data frame with replicated compounds and remove compounds absent in all individuals
# uhplc_calculus_replicated <- uhplc_calculus_bin %>%
#   group_by(id,compound) %>%
#   summarise(presence = sum(presence)) %>%
#   filter(presence == 0 | presence == 2) %>%
#   #filter(sum(presence) != 0) #%>% # remove compounds absent in all individuals in sample
#   mutate(presence = if_else(presence == 2, 1, 0)) %>% # convert replications to presence/absence
#   pivot_wider(names_from = compound, values_from = presence) %>%
#   mutate(across(where(is.numeric), replace_na, 0)) %>% # replace NAs with 0 (not ideal solution, may be reconsidered)
#   left_join(select(metadata, id, sample), by = "id")


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
#
# # dental data
#
# # Caries rate
#
# # convert caries data from location to number of caries and caries rate
#
# surface <- c("mes", "dis", "occ", "buc", "lin", "root", "crown") # here occ = incisal
#
# caries_rate <- caries %>%
#   pivot_longer(t11:t48, names_to = "tooth",
#                values_to = "caries_score") %>%
#   na.omit() %>%
#   separate_rows(caries_score, sep = ";") %>% # one lesion per row
#   mutate(
#     caries_count = if_else(
#     caries_score == "none", 0L, 1L
#     )
#   ) %>%
#   group_by(id) %>%
#   summarise(
#     n_teeth = n(),
#     count = sum(caries_count, na.rm = T), # crude caries rate
#     caries_rate = count / n_teeth
#   ) %>%
#   select(id, caries_rate)
#
# # Median periodontal status
#
# periodont_status <- periodont %>%
#   mutate(periodont_status = apply(.[,-1], MARGIN = 1, FUN = median, na.rm = T)) %>%
#   select(id, periodont_status)
#
# # number of periapical lesions
#
# periap_num <- periap %>%
#   mutate(across(-id, ~ if_else(.x == "none", 0, 1))) %>%
#   mutate(periap_num = apply(.[,-1], MARGIN = 1, FUN = sum, na.rm = T)) %>%
#   select(id, periap_num)
#
# # calculus index
#
# calc_index <- calculus_full %>%
#   dental_longer(id) %>%
#   calculus_index(simple = T) %>%
#   select(!c(n_surf, score_sum))
#
# # Prepare data for EFA ----------------------------------------------------
#
# data_list <- list(path_cond_clean, sinusitis_clean, caries_rate, periodont_status, periap_num, uhplc_calculus_replicated, calc_index)
#
# efa_data <- data_list %>%
#   reduce(inner_join, by = "id") %>%
#   mutate(across(where(is.logical), as.numeric)) %>%
#   filter(complete.cases(.))


# Export data -------------------------------------------------------------

# write cleaned data to data/derived_data
write_csv(uhplc_data_comb, "inst/data/derived_data/uhplc-data_combined.csv")
#write_csv(uhplc_calculus, "inst/data/derived_data/uhplc-calculus_cleaned.csv")
write_csv(sinusitis_clean, "inst/data/derived_data/sinusitis_cleaned.csv")
write_csv(path_cond_clean, "inst/data/derived_data/path-conditions_cleaned.csv")

#write_csv(efa_data, "inst/data/derived_data/efa-analysis_data.csv")
