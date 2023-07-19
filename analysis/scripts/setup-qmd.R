library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggcorrplot)
library(stringr)
library(glue)
library(purrr)
library(psych)
library(patchwork)
library(readr)

devtools::load_all()
try(generate_bib())

# upload data
#metadata <- read_tsv(here("analysis/data/raw_data/metadata.tsv"))
#demography <- read_csv(here("analysis/data/raw_data/demography.csv"))
#lloq <- read_tsv(here("analysis/data/raw_data/lloq.tsv"))
#uhplc_data_comb <- read_csv(here("analysis/data/derived_data/uhplc-data_combined.csv"))
#dental_inv <- read_csv(here("analysis/data/raw_data/dental-inv.csv"))
#caries <- read_csv(here("analysis/data/raw_data/caries.csv"))
#periodont <- read_csv(here("analysis/data/raw_data/periodontitis.csv"))
#periap <- read_csv(here("analysis/data/raw_data/periapical.csv"))
#calculus <- read_csv(here("analysis/data/raw_data/calculus.csv"))
#calculus_full <- read_csv(here("analysis/data/raw_data/calculus_full.csv"))
#sinusitis_clean <- read_csv(here("analysis/data/derived_data/sinusitis_cleaned.csv"))
#path_cond_clean <- read_csv(here("analysis/data/derived_data/path-conditions_cleaned.csv"))

#source(here("analysis/scripts/setup-qmd.R"))
#source(here("analysis/supplementary-materials/supp-mat.qmd"), local = knitr::knit_global())
# set viridis as default pallette



# UHPLC analysis ----------------------------------------------------------

weight <- uhplc_data_comb %>%
  select(sample, batch1_weight, batch2_weight) %>%
  pivot_longer(
    cols = -sample,
    names_to = c("batch"),
    #names_pattern = "(.*)_(.*)",
    values_to = "weight"
  ) %>%
  mutate(batch = str_remove(batch, "_weight"),
         sample = as.character(sample))

uhplc_data_long <- uhplc_data_comb %>%
  #select(!contains("id_batch")) %>%
  mutate(sample = as.character(sample)) %>%
  select(!c(batch1_weight, batch2_weight)) %>%
  left_join(select(metadata, id, sample), by = "sample") %>%
  pivot_longer(
    -c(sample, id),
    names_to = c("compound", "extraction", "batch"),
    names_pattern = "(.*)_(.*)_(.*)",
    #names_to = c("compound", "batch"),
    #names_pattern = "(.*)_(.*)",
    values_to = "quant"
  )

# filter to remove all compounds that weren't detected within each sample (they clog up the lower part of the plots and are uninformative)
quant_filter <- uhplc_data_long %>%
  group_by(sample, compound) %>%
  summarise(
    sum = sum(quant, na.rm = T)
  ) %>%
  filter(sum > 0) %>%
  select(!sum)

# uhplc_calculus_id <- uhplc_calculus %>%
#   mutate(sample = as.character(sample)) %>%
#   left_join(select(metadata, id, sample), by = "sample") # add individual IDs to data frame

uhplc_calculus_long <- uhplc_data_long %>%
  # pivot_longer(
  #   -c(sample, id),
  #   names_to = c("compound", "batch"),
  #   names_pattern = "(.*)_(.*)",
  #   values_to = c("quant")
  # ) %>%
  filter(extraction == "calc") %>%
  #mutate(compound = str_remove(compound, "_calc")) %>%
  #remove_missing() %>%
  left_join(weight) %>%  # add weight of calculus samples
  mutate(presence = if_else(quant > 0, 1, quant)) %>%
  group_by(id, sample, compound, batch) %>%
  mutate(conc = quant / weight) %>%
  left_join(demography, by = "id") %>%
  mutate(preservation = factor(preservation, levels = c("fair", "good", "excellent")))

uhplc_conc_wide <- uhplc_calculus_long %>%
  filter(batch == "batch2") %>%
  #left_join(select(metadata, id, batch2_weight)) %>%
  #remove_missing() %>%
  ungroup() %>%
  # mutate(compound = case_when(compound == "nicotine" ~ "tobacco",
  #                             compound == "cotinine" ~ "tobacco",
  #                             TRUE ~ compound)) %>%
  # group_by(id, sample, compound) %>%
  # summarise(presence = sum(presence)) #%>% # combine nicotine and cotinine
  #remove_missing() %>%
  # mutate(presence = case_when(presence > 0 ~ TRUE,
  #                             TRUE ~ FALSE)) %>%
  select(
    !c(presence, quant, weight, pipe_notch, sex, age, preservation, completeness)
    ) %>%
  remove_missing() %>%
  pivot_wider(names_from = "compound", values_from = "conc") %>%
  select(!c(extraction, batch, sample))


# Dental analysis ---------------------------------------------------------

dental_inv_long <- dental_inv %>% # Inventory
  right_join(demography) %>%
  select(!c(pipe_notch)) %>%
  pivot_longer(t11:t48, names_to = "tooth",
               values_to = "status") #%>% no reason to recode dna...
  #mutate(status = case_when(status == "dna" ~ "m", # teeth missing due to DNA sampling recoded as missing. WILL affect aml calculations...
                            #TRUE ~ status))

teeth_list <- list(caries, periodont, periap)#, calculus) # calculus_full too wide - one column per surface

# convert all data frames in list to long format
dental_long_list <- lapply(
  teeth_list,
  pivot_longer,
  cols = t11:t48,
  names_to = "tooth",
  values_to = "score"
)

dental_long_list <- lapply(teeth_list, dental_longer, -id)

# combine to single long-format data frame
dental_long <- reduce(
  dental_long_list,
  full_join,
  by = c(
    "id", "tooth", "region", "side", "position", "class", "type", "quadrant"
    )
  ) %>%
  full_join(dental_inv_long, by = c("id", "tooth")) %>%
  tooth_position() %>%
  rename(
    caries = score.x,
    periodont = score.y,
    periap = score,
    #calculus = score.y.y
    )


# dental data

# Caries rate

# convert caries data from location to number of caries and caries rate

surface <- c("mes", "dis", "occ", "buc", "lin", "root", "crown") # here occ = incisal

caries_count <- dental_long %>%
  separate_rows(caries, sep = ";") %>% # one lesion per row
  mutate(
    caries_count = if_else(
      caries == "none", 0L, 1L, missing = NA # convert lesion location to binary (present = 1; absent = 0)
    )
  ) %>%
  group_by(id, tooth) %>%
  summarise(
    #n_teeth = n(),
    count = sum(caries_count),
  ) %>%
  tooth_position() %>%
  ungroup()

caries_ratio_id <- caries_count %>%
  group_by(id) %>%
  caries_ratio(.caries = count) %>%
  select(id, ratio) %>%
  rename(caries_ratio = ratio)

caries_ratio_site <- caries_count %>%
  caries_ratio(.caries = count)

# caries_ratio <- caries %>%
#   pivot_longer(t11:t48, names_to = "tooth",
#                values_to = "caries_score") %>%
#   na.omit() %>%
#   separate_rows(caries_score, sep = ";") %>% # one lesion per row
#   mutate(
#     caries_count = if_else(
#       caries_score == "none", 0L, 1L
#     )
#   ) %>%
#   group_by(id) %>%
#   summarise(
#     n_teeth = n(),
#     count = sum(caries_count, na.rm = T), # crude caries rate
#     caries_ratio = count / n_teeth
#   ) %>%
#   select(id, caries_ratio)

# Median periodontal status

periodont_status <- periodont %>%
  mutate(periodont_status = apply(.[,-1], MARGIN = 1, FUN = median, na.rm = T)) %>%
  select(id, periodont_status)

# number of periapical lesions

periap_num <- periap %>%
  mutate(across(-id, ~ if_else(.x == "none", 0, 1))) %>%
  mutate(periap_num = apply(.[,-1], MARGIN = 1, FUN = sum, na.rm = T)) %>%
  select(id, periap_num)

# calculus index

calc_index <- calculus_full %>%
  dental_longer(-id) %>%
  calculus_index(simple = T) %>% # single index per individual (not ideal...)
  select(!c(n_surf, score_sum, n_teeth))

# Prepare data for explore ----------------------------------------------------

data_list <- list(
  path_cond_clean,
  sinusitis_clean,
  caries_ratio_id,
  periodont_status,
  periap_num,
  uhplc_conc_wide,
  calc_index
  )

explore_data <- data_list %>%
  reduce(inner_join, by = "id") %>%
  mutate(across(where(is.logical), as.numeric)) %>%
  left_join(select(demography, age, id, pipe_notch), by = "id") %>%  # add age to the data frame
  mutate(age = case_when(
    age == "eya" ~ 1,
    age == "lya" ~ 2,
    age == "ma" ~ 3,
    age == "old" ~ 4
  ))

# filter out low frequency variables
explore_filter <- explore_data %>%
  #select(!c(OD)) %>% # vertebral osteophytosis highly correlated with OA
  summarise(across(where(is.numeric), function(x) sum(x, na.rm = T) / length(x))) %>%
  select(which(colSums(.) > 0.05), theophyl, caffeine, nicotine, cotinine, salicyl)

explore_select <- explore_data %>%
  select(names(explore_filter)) %>%
  select(!c(IPR, periap_num, Mastoiditis, OD, PNBF))

# discretisation of caries and calculus using quartiles
caries_discrete <- quantile(caries_ratio_id$caries_ratio)
calculus_discrete <- quantile(calc_index$calc_index)

explore_discrete <- explore_select %>%
  mutate(
    across(c(caffeine, cotinine, nicotine, theophyl),
           ~ if_else(.x == 0, 0, 1)),
    calc_index = case_when(
      calc_index <= calculus_discrete[1] ~ 0,
      calc_index >= calculus_discrete[1] & calc_index < calculus_discrete[2] ~ 1,
      calc_index >= calculus_discrete[2] & calc_index < calculus_discrete[3] ~ 2,
      calc_index >= calculus_discrete[3] & calc_index < calculus_discrete[4] ~ 3,
      calc_index >= calculus_discrete[4] & calc_index <= calculus_discrete[5] ~ 4,
    ),
    caries_ratio = case_when(
      caries_ratio <= caries_discrete[1] ~ 0,
      caries_ratio >= caries_discrete[1] & caries_ratio < caries_discrete[2] ~ 1,
      caries_ratio >= caries_discrete[2] & caries_ratio < caries_discrete[3] ~ 2,
      caries_ratio >= caries_discrete[3] & caries_ratio < caries_discrete[4] ~ 3,
      caries_ratio >= caries_discrete[4] & caries_ratio <= caries_discrete[5] ~ 4,
    )
  ) %>%
  select(!c(salicyl, pipe_notch)) %>% # too high frequency for polychoric corr
  as.matrix()


# Correlation -------------------------------------------------------------

# Pearson

conc_cor <- cor(
  explore_select,
  use = "pairwise.complete.obs",
  method = "pearson"
  # remove correlations between dichotomous variables
  )[,c("caries_ratio", "nicotine", "salicyl", "calc_index", "pipe_notch", "theophyl", "caffeine", "cotinine")]

conc_cor_tib <- conc_cor %>%
  as_tibble(rownames = "var") %>%
  pivot_longer(-var, values_to = "r") %>%
  mutate(
    strength = case_when(
      abs(r) >= 0.8 ~ "strong",
      abs(r) < 0.8 & abs(r) >= 0.4 ~ "moderate",
      abs(r) < 0.4 ~ "weak"
    ),
    direction = case_when(
      r > 0 ~ "positive",
      r < 0 ~ "negative"
    )
  ) %>%
  filter(r != 1)

# Polychoric correlation

polycorr <- polychoric(explore_discrete)

# Correlation tibble with strength and direction
polycorr_tib <- polycorr$rho %>%
  as_tibble(rownames = "var") %>%
  pivot_longer(-var, values_to = "corr") #%>%
  # mutate(
  #   strength = case_when(
  #     abs(corr) >= 0.8 ~ "strong",
  #     abs(corr) < 0.8 & abs(corr) >= 0.4 ~ "moderate",
  #     abs(corr) < 0.4 ~ "weak"
  #   ),
  #   direction = case_when(
  #     corr > 0 ~ "positive",
  #     corr < 0 ~ "negative"
  #   )
  # ) %>%
  # filter(corr != 1)


