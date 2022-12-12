library(tidyverse)
library(here)
source(here("scripts/dental-plot.R"))

dental_long <- readr::read_csv(here("data/derived_data/dental-full_long.csv"))



# Data wrangling ----------------------------------------------------------

# Caries rate

# convert caries data from location to number of caries and caries rate
# number of lesions per tooth observed
# separate anterior and posterior teeth
surface <- c("mes", "dis", "occ", "buc", "lin", "root", "crown") # here occ = incisal

caries_rate <- dental_long %>%
  filter(status == "p") %>%
  separate_rows(caries_score, sep = ";") %>% # one lesion per row
  mutate(caries_count = if_else(
    caries_score == "none", 0L, 1L
    )
  ) %>%
  group_by(id, tooth_type, position) %>%
  summarise(
    n_teeth = n(),
         count = sum(caries_count, na.rm = T),
         rate = count / n_teeth
            )


  # mutate(caries_count = if_else(caries_score == "none", 0L, 
  #                               stringi::stri_count_words(caries_score)
  #                               )
  #       )
  
#caries_rate <- 
# caries_long %>%
#   group_by(id, position) %>%
#   filter(status == "p") %>%
#   summarise(n_teeth = n(),
#             count = sum(caries_count, na.rm = T),
#             rate = count / n_teeth
#             ) %>%
#   ungroup()
#filter(status == "p") %>%
#  summarise(caries_count = sum(caries_count, na.rm = T)) %>% #view
#  group_by(id) %>%
#  summarise(n_teeth = n(),
#            caries_rate = sum(caries_count, na.rm = T) / n_teeth)

# caries_rate_type <- dental_long %>%
#   group_by(tooth_type) %>%
#   summarise(n_teeth = sum(!is.na(caries_count)),
#             n_caries = sum(caries_count, na.rm = T),
#             caries_rate = n_caries / n_teeth) # shouldn't be any NA??

# CARIES RATE CALCULATION

# Caries longform data with row for each surface of each tooth?

# convert periodontitis and calculus to mean/median score?  



#full_data_long

# compare replicated samples across batch 1 and 2

caffeine_rep <- uhplc_data_comb %>%
  select(sample, contains("caffeine")) %>%
  select(sample, contains("calc")) %>%
  summarise(caffeine = sum(caffeine_calc_batch1, caffeine_calc_batch2)) %>%
  mutate(caffeine = if_else(!is.na(caffeine), "yes", "no"))
#  mutate(caffeine = rowSums()) %>%
#  select(sample, caffeine)

replicates <- data.frame("sample" = uhplc_data_batch2$sample)

tobacco <- compounds %>%
  #na.omit() %>%
  filter(compound %in% c("nicotine", "cotinine")) %>%
  inner_join(demography, by = "id")
# Summary data frames -----------------------------------------------------

# Central tendency of dental lesions per individual



# Plots -------------------------------------------------------------------

# types of lesions
dental_long %>%
  filter(!is.na(caries_score),
         caries_score != "none",
         caries_score != "root?") %>%
  separate_rows(caries_score, sep = ";") %>%
  dental_plot(fill = caries_score)

# overall distribution of dental lesions in sample

# dental_long %>%
#   dental_plot(caries_count)
# 
# dental_long %>%
#   dental_plot(periodont_score)
# 
# dental_long %>%
#   dental_plot(periap_score)

## this plot is essentially useless since I targeted individuals with calculus
# on lower incisors
# dental_long %>%
#   dental_plot(calculus_score) 

# distribution of 

