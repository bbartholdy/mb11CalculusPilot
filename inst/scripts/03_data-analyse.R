# data analysis
library(psych)
library(dplyr)
library(readr)
library(corrplot)

# Upload data -------------------------------------------------------------

#efa_data <- read_csv("inst/data/derived_data/efa-analysis_data.csv")

# combine nicotine and cotinine to tobacco
efa_comb <- efa_data %>%
  mutate(tobacco = case_when(nicotine == 1 | cotinine == 1 ~ 1,
                             TRUE ~ 0)) %>%
  select(!c(nicotine, cotinine))

# Exploratory factor analysis (psych package?)

  # tetrachoric correlation for dichotomous variables


# Missing values



# selection of variables (remove very low and high frequencies)

# dichotomise caries and calculus ratios for polychoric correlation
  # caries ratios falling below the 25th percentile are considered absent
  # discretisation of caries and calculus
caries_discrete <- quantile(caries_rate$caries_rate)
calculus_discrete <- quantile(calc_index$calc_index)

caries_bin <- caries_rate %>%
  mutate(caries_rate = case_when(caries_rate > quantile(caries_rate)[2] ~ 1,
                                 TRUE ~ 0
  ))


efa_filter <- efa_comb %>%
  select(!c(OD)) %>% # vertebral osteophytosis highly correlated with OA
  summarise(across(where(is.numeric), function(x) sum(x) / length(x))) %>%
  select(which(colSums(.) > 0.1))

efa_select <- efa_comb %>%
  select(names(efa_filter)) %>%
  select(!c(periap_num)) %>%
  filter(complete.cases(.)) #%>%
  as.matrix()

efa_discrete <- efa_select %>%
  mutate(
    calc_index = case_when(
      calc_index <= calculus_discrete[1] ~ 0,
      calc_index >= calculus_discrete[1] & calc_index < calculus_discrete[2] ~ 1,
      calc_index >= calculus_discrete[2] & calc_index < calculus_discrete[3] ~ 2,
      calc_index >= calculus_discrete[3] & calc_index < calculus_discrete[4] ~ 3,
      calc_index >= calculus_discrete[4] & calc_index <= calculus_discrete[5] ~ 4,
      ),
    caries_rate = case_when(
      caries_rate <= caries_discrete[1] ~ 0,
      caries_rate >= caries_discrete[1] & caries_rate < caries_discrete[2] ~ 1,
      caries_rate >= caries_discrete[2] & caries_rate < caries_discrete[3] ~ 2,
      caries_rate >= caries_discrete[3] & caries_rate < caries_discrete[4] ~ 3,
      caries_rate >= caries_discrete[4] & caries_rate <= caries_discrete[5] ~ 4,
    )
  ) %>%
  as.matrix()



KMO(cor(efa_discrete)) # lower than suggested 0.6, maybe factor analysis not good
cortest.bartlett(efa_discrete) # small value, factor analysis okay



# Polychoric correlation

cor(efa_select)[,c("caries_rate","calc_index")] # for comparing caries and calculus with compounds before dichotomisation

polycorr <- polychoric(efa_discrete)

corrplot(polycorr$rho, method = "number", type = "lower", diag = F)

# Factor analysis ---------------------------------------------------------

efa_init <- fa(efa_discrete, nfactors = ncol(efa_discrete), rotate = "none")
eigenval <- efa_init$e.values
data.frame(
  n_fact = as.factor(1:length(eigenval)),
  eigenval
           ) %>%
  ggplot(aes(x = n_fact, y = eigenval, group = 1)) +
    geom_line() +
    geom_point()

efa_analysis <- fa(efa_select)
fa.diagram(efa_analysis)



# Multiple correspondence analysis ----------------------------------------


