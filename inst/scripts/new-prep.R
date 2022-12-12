library(tidyverse)
library(here)

# Upload data -------------------------------------------------------------

# See README for details
lloq <- readr::read_tsv(here("data/lloq.tsv"))
uhplc_data_raw <- readr::read_csv(here("data/raw/anon/uhplc-results.csv"))
uhplc_data_raw2 <- readr::read_csv(here("data/raw/anon/uhplc-results_batch2.csv"))
dental_inv <- readr::read_csv(here("data/raw/anon/dental-inv.csv"))
caries <- readr::read_csv(here("data/raw/anon/caries.csv"))
periodont <- readr::read_csv(here("data/raw/anon/periodontitis.csv"))
periap <- readr::read_csv(here("data/raw/anon/periapical.csv"))
calculus <- readr::read_csv(here("data/raw/anon/calculus.csv"))
demography <- readr::read_csv(here("data/raw/anon/demography.csv"))
sinusitis <- readr::read_csv(here("data/raw/anon/sinusitis.csv"), na = "UNOBS")
# CMS = chronic maxillary sinusitis
# IPR = periosteal reaction on visceral surface of ribs 
  # present only if active (pulmonary infection)

# Helper objects ----------------------------------------------------------

# vectors to categorise teeth by position and type (FDI notation)
maxilla <- c(paste0("t", 11:18), paste0("t", 21:28))
mandible <- c(paste0("t", 31:38), paste0("t", 41:48)) 
left <- c(paste0("t", 21:28), paste0("t", 31:38))
posterior <- c(paste0("t", 14:18), paste0("t", 24:28), 
               paste0("t", 34:38), paste0("t", 44:48))
molar <- c(paste0("t", 16:18), paste0("t", 26:28), 
           paste0("t", 36:38), paste0("t", 46:48))
incisor <- c(paste0("t", 11:12), paste0("t", 21:22), 
             paste0("t", 31:32), paste0("t", 41:42))
premolar <- c(paste0("t", 14:15), paste0("t", 24:25), 
              paste0("t", 34:35), paste0("t", 44:45))
canine <- c(paste0("t", 13), paste0("t", 23), 
            paste0("t", 33), paste0("t", 43))

# Data cleaning -----------------------------------------------------------

# UHPLC-MS/MS results

uhplc_data <- uhplc_data_raw %>%
  filter(sample != "16b") %>% # sample 16b removed (test sample that differs from 16 in sampling location)
  group_by(sample) %>%
  mutate(across(weight:cocaine_calculus, sum)) %>% # combine 12.1 and 12.2 (split samples from same sampling location)
  filter(sample != "12.2") %>%
  rename(theophyl_calc = thyephyl_calc) #%>%

uhplc_data$sample[uhplc_data$sample == "12.1"] <- "12" # change value to 12
uhplc_data$sample <- as.factor(uhplc_data$sample)

write_csv(uhplc_data, here("data/derived_data/uhplc-data_cleaned.csv"))
# write cleaned data to data/derived_data

id_sample_key <- uhplc_data %>%
  select(id, sample)

uhplc_data_batch2 <- uhplc_data_raw2 %>%
  rename(theophyl_calc = theophylline_calc) %>%
  mutate(sample = as_factor(sample))

write_csv(uhplc_data_batch2, here("data/derived_data/uhplc-data-batch2_cleaned.csv"))
# write cleaned data to data/derived_data

# Combine batch 1 and 2

uhplc_data_comb <- uhplc_data %>%
  full_join(uhplc_data_batch2, by = "sample", suffix = c("_batch1", "_batch2"))
names(uhplc_data_comb)

uhplc_long_batch1 <- uhplc_data %>%
  pivot_longer(cols = caffeine_wash1:cocaine_calculus, 
               names_to = c("compound", "wash"), names_sep = "_", 
               values_to = "quantity") %>% # combine wash1-3 into single column
  select(!c(weight, weight_wash1, weight_wash2, weight_wash3)) %>%
  mutate(batch = 1)

uhplc_long_batch2 <- uhplc_data_batch2 %>%
  semi_join(id_sample_key) %>%
  pivot_longer(cols = caffeine_wash1:cocaine_calc, 
               names_to = c("compound", "wash"), names_sep = "_", 
               values_to = "quantity") %>% # combine wash1-3 into single column
  select(!c(weight, weight_wash1, weight_wash2, weight_wash3)) %>%
  mutate(batch = 2)

uhplc_long <- uhplc_long_batch1 %>%
  bind_rows(uhplc_long_batch2)

## NEED TO CALCULATE CONCENTRATION BEFORE COMBINING (although concentration is not really relevant) ##

compounds_batch1 <- uhplc_data %>%
  select(!contains(c("wash", "weight"))) %>%
  pivot_longer(cols = where(is.numeric), 
               names_to = "compound", values_to = "quant") %>%
  mutate(batch = 1) %>%
  mutate(compound = str_replace(compound, "_calc", ""))

compounds_batch2 <- uhplc_data_batch2 %>%
  select(!contains(c("wash", "weight"))) %>%
  pivot_longer(cols = caffeine_calc:cocaine_calc, 
               names_to = "compound", values_to = "quant") %>%
  mutate(batch = 2) %>%
  mutate(compound = str_replace(compound, "_calc", ""))

# combined data frame of compound detection across both batches
compounds <- compounds_batch2 %>%
  left_join(id_sample_key) %>%
  bind_rows(compounds_batch1) %>%
  mutate(detection = if_else(is.na(quant), FALSE, TRUE)) # presence/absence variable


# Calculate concentration (ng / mg) of each compound in the calculus

uhplc_data_conc <- uhplc_data %>%
  select(!contains(c("_wash1", "_wash2"))) %>%
  mutate(across(contains("_calc"), ~ .x / weight_wash3)) %>%
  select(!contains("_wash3"))

sinusitis <- sinusitis %>%
  mutate(across(CMS:IPR, 
                function(x) case_when(x == "YES" ~ TRUE,
                                      x == "NO" ~ FALSE)))

# Dental data -------------------------------------------------------------

# convert to long form

dental_inv_long <- dental_inv %>% # Inventory
  right_join(demography) %>%
  select(!c(occupation, own_grave, tax, pipe_notch)) %>%
  pivot_longer(t11:t48, names_to = "tooth", 
               values_to = "status") %>%
  mutate(status = case_when(status == "dna" ~ "m", # teeth missing due to DNA sampling
                            TRUE ~ status))

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

# Combined data -----------------------------------------------------------

# prepare to merge data frames
teeth_list <- list(dental_inv_long, caries_long, periodont_long, 
                   periap_long, calculus_long)

# Long dental frame

dental_long <- teeth_list %>%
  reduce(inner_join, by = c("id", "tooth")) %>%
  mutate(region = if_else(tooth %in% maxilla, "maxilla", "mandible"),
         side = if_else(tooth %in% left, "left", "right"), 
         position = if_else(tooth %in% posterior, "posterior", "anterior"),
         tooth_type = case_when(tooth %in% incisor ~ "incisor",
                                tooth %in% canine ~ "canine",
                                tooth %in% premolar ~ "premolar",
                                tooth %in% molar ~ "molar"),
         .before = status) %>%
  mutate(comb_sex = case_when(sex == "pm" ~ "m",
                         sex == "pf" ~ "f",
                         TRUE ~ sex))

write_csv(dental_long, here("data/derived_data/dental-full_long.csv"))

# data frame with one row per individual
  # one column per dental disease
  # one column for presence/absence of compound

# all_data_wide <- caries_rate %>%
#   left_join(aml_rate)
  
