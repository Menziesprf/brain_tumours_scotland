library(janitor)
library(tidyverse)
library(readxl)
library(stringi)

# --------- DATASET 1: Annual Incidence -----------

excel_sheets("data/cancer-incidence-brain-and-cns.xls")

all_ann_inc_data <- read_xls("data/cancer-incidence-brain-and-cns.xls", sheet = 7)

all_ann_inc_data_clean <- all_ann_inc_data %>%
  select(-id, -hbnew, -sitenew, -sexnew, -label) %>% 
  mutate(trans1.1994 = as.double(trans1.1994),
         trans1.1995 = as.double(trans1.1995),
         trans1.1996 = as.double(trans1.1996),
         trans1.1997 = as.double(trans1.1997),
         trans1.1998 = as.double(trans1.1998),
         trans1.1999 = as.double(trans1.1999)) %>%
  pivot_longer(cols = trans1.1994:trans1.2018,
               names_to = "year",
               values_to = "values") %>% 
  mutate(year = str_replace(year, "trans1.", ""),
         age_label = str_replace_all(age_label, 
                                 c("Number of registrations: " = "num_", 
                                   "Incidence rate: " = "inc_",
                                   "Under 5" = "0-4",
                                   #"5.." = "5-9",
                                   "80x4" = "80-84",
                                   "85x9" = "85-89",
                                   "All Ages" = "all",
                                   "(Incidence)" = "")),
         sex_label = str_replace_all(sex_label,
                                     c("All Persons" = "all",
                                       "Males" = "male",
                                       "Females" = "female")),
         hb_label = str_to_lower(hb_label),
         site_label = recode(site_label,
                             "Malignant brain cancer" = "mal_brain",
                             "Malignant brain cancer (incl pituitary gland, craniopharyngeal duct and pineal gland)" = "mal_brain_plus_glands",
                             "Non-malignant brain cancer (incl pituitary gland, craniopharyngeal duct and pineal gland)" = "non_mal_plus_glands",
                             "All brain and CNS tumours (malignant and non-malignant)" = "all")) %>% 
  rename(hb = hb_label,
         cancer_type = site_label,
         sex = sex_label,
         age = age_label)

# 1A: AGES 

incidence_all_ages <- all_ann_inc_data_clean %>% 
  filter(str_detect(age, "num_|inc_|Crude Rate ()")) %>% 
  filter(!str_detect(age, "Upper|Lower")) %>% 
  mutate(age = str_replace(age, "Crude Rate ()", "inc_all"),
         age = stri_replace_all_fixed(age, "()", "")) %>% 
  mutate(inc_flag = case_when(str_detect(age, "num") ~ "num_cases",
                              T ~ "incidence")) %>% 
  mutate(age = str_replace_all(age, c("num_" = "", "inc_" = ""))) %>% 
  pivot_wider(names_from = inc_flag, 
               values_from = values) 

  
# 1B: Incidence + stats 

incidence_all_stats <- all_ann_inc_data_clean %>% 
  filter(str_detect(age, "Crude|EASR|WASR|Standardised|SIR")) %>% 
  mutate(age = recode(age,
                      "Standardised  Ratio" = "SIR")) %>% 
  pivot_wider(names_from = age,
              values_from = values) %>%   # CONSIDER DROPPING SIR == 100.00000 only seem to be in scotland rows
  clean_names()


  

rm(all_ann_inc_data, all_ann_inc_data_clean)

# ------------ DATASET 2: Mortality  ------------


excel_sheets("data/mal_cns_ann_mort.xls")

mal_cns_mort_data <- read_xls("data/mal_cns_ann_mort.xls", sheet = 7)

mortality_clean <- mal_cns_mort_data %>% 
  select(-id, -hbnew, -sitenew, -sexnew, -label) %>% 
  pivot_longer(cols = trans1.1993:trans1.2018,
               names_to = "year",
               values_to = "values") %>% 
  mutate(year = str_replace(year, "trans1.", ""),
         age_label = str_replace_all(age_label, 
                                     c("Number of deaths: " = "num_", 
                                       "Mortality rate: " = "mort_",
                                       "Under 5" = "0-4",
                                       #"5.." = "5-9",
                                       "80x4" = "80-84",
                                       "85x9" = "85-89",
                                       "All Ages" = "all",
                                       "(Mortality)" = "")),
         sex_label = str_replace_all(sex_label,
                                     c("All Persons" = "all",
                                       "Males" = "male",
                                       "Females" = "female")),
         hb_label = str_to_lower(hb_label),
         site_label = recode(site_label,
                             "Malignant brain cancer" = "mal_brain",
                             "Malignant brain cancer (incl pituitary gland, craniopharyngeal duct and pineal gland)" = "mal_brain_plus_glands")) %>% 
  rename(hb = hb_label,
         cancer_type = site_label,
         sex = sex_label,
         age = age_label)

# 2A: AGES 

mort_ages <- mortality_clean %>% 
  filter(str_detect(age, "num_|mort_|Crude Rate ()")) %>% 
  filter(!str_detect(age, "Upper|Lower")) %>% 
  mutate(age = str_replace(age, "Crude Rate ()", "mort_all"),
         age = stri_replace_all_fixed(age, "()", "")) %>% 
  mutate(mort_flag = case_when(str_detect(age, "num") ~ "num_deaths",
                              T ~ "mortality_rate")) %>% 
  mutate(age = str_replace_all(age, c("num_" = "", "mort_" = ""))) %>% 
  pivot_wider(names_from = mort_flag, 
              values_from = values)

# 2B: STATS 

mort_stats <- mortality_clean %>% 
  filter(str_detect(age, "Crude|EASR|WASR|Standardised|SMR")) %>% 
  mutate(age = recode(age,
                      "Standardised  Ratio" = "SMR")) %>% 
  pivot_wider(names_from = age,
              values_from = values) %>%   # CONSIDER DROPPING SMR == 100.00000 only seem to be in scotland rows
  clean_names()

rm(mal_cns_mort_data, mortality_clean)


# ----------- DATASET 3: SURVIVAL ---------

excel_sheets("data/estimates-of-survival-from-brain-and-other-cns-cancers.xlsx")

survival_all_data <- read_xlsx("data/estimates-of-survival-from-brain-and-other-cns-cancers.xlsx", sheet = 5) %>% 
  clean_names()

survival_notes <- read_xlsx("data/estimates-of-survival-from-brain-and-other-cns-cancers.xlsx", sheet = 6)
survival_notes2 <- read_xlsx("data/estimates-of-survival-from-brain-and-other-cns-cancers.xlsx", sheet = 7)

# survival @ 1, 3 5 & 10 years 1987 - 2017
# Brain and other CNS (ICD-9 191-192; ICD-10 C70-C72, C75.1-C75.3)
# NA's when less than 10 patients alive at 1, 5 or 10 years or when there is little change from previous time period
# NA's dropped 

survival_clean <- survival_all_data %>% 
  mutate(across(.cols = observed_survival_percent:upper_95_percent_ci_for_net_survival, 
                .fns = as.double)) %>% 
  select(-cancer_site_grouping) %>% 
  drop_na()
  
summary(survival_clean)
  
rm(survival_all_data, survival_notes, survival_notes2)
# ----------- DATASET 4: DEPRIVATION ---------


excel_sheets("data/deprivation-brain-and-cns.xls")

# Incidence per SIMD quintile 2014 - 2018

deprivation_incidence_data <- read_xls("data/deprivation-brain-and-cns.xls", sheet = 1, range = "A11:G18", col_names = T) %>% 
  clean_names() 


deprivation_incidence_clean <- deprivation_incidence_data %>% 
  select(-x2, -x4) %>% 
  filter(!is.na(easr)) %>% 
  rename(incidence_easr = easr,
         inc_lower_ci_95 = lower_95_percent_ci,
         inc_upper_ci_95 = upper_95_percent_ci)
      




excel_sheets("data/mal_cns_deprivation.xls")

# incidence per SIMD quintile 2013-2017, mortality rate 2014-2018 

deprivation_mortality_data <- read_xls("data/mal_cns_deprivation.xls", sheet = 1, range = "A11:M18", col_names = T) %>%
  clean_names()

deprivation_mortality_clean <- deprivation_mortality_data %>% 
  select(simd_2016_deprivation_quintile, 
         number_of_death_registrations, 
         easr_11, 
         lower_95_percent_ci_12, 
         upper_95_percent_ci_13) %>% 
  filter(!is.na(easr_11)) %>% 
  rename(mortality_easr = easr_11,
         mort_lower_ci_95 = lower_95_percent_ci_12,
         mort_upper_ci_95 = upper_95_percent_ci_13)

deprivation_complete <- deprivation_incidence_clean %>% 
  left_join(deprivation_mortality_clean, by = "simd_2016_deprivation_quintile")


# USED INCIDENCE FROM 'A' and mortality rate from 'B'

# Uses EASR 

# Incidence TREND TEST - poisson regression = 0.067500000000000004

# Mortality TREND TEST - poisson regression = 0.061100000000000002

# Incidence and mortality for Malignant brain cancer (including pituitary gland, craniopharyngeal duct and pineal gland) 
                              # (ICD-10 C70-C72, C75.1-C75.3)

# DATA FOR ALL CANCERS: excel_sheets("data/deprivation-estimates-of-survival-from-all-cancers.xlsx")

# deprivation_all_cancers <- read_xlsx("data/deprivation-estimates-of-survival-from-all-cancers.xlsx", sheet = 3)


rm(deprivation_incidence_data, deprivation_incidence_clean,
   deprivation_mortality_data, deprivation_mortality_clean)


# ------------------- DATASET 5: RISK ---------------------

# NOT A PRIORITY

#excel_sheets("data/mal_cns_risk.xls")

#risk_scotland <- read_xls("data/mal_cns_risk.xls", sheet = 1)
#risk_data <- read_xls("data/mal_cns_risk.xls", sheet = 4)




# ----------- DATASET 6: PREVALENCE ---------


excel_sheets("data/mal_cns_prev.xls")

prev_scotland <- read_xls("data/mal_cns_prev.xls", sheet = 1)
prev_data <- read_xls("data/mal_cns_prev.xls", sheet = 4)

prevalence_clean <- prev_data %>% 
  select(-id, -sitenew, -sexnew, -section) %>% 
  rename(number_cases = stat.1,
         rate_per_100k = stat.2,
         "%_prev_in_pop" = stat.3,
         "%_in_age_or_time_group" = stat.4) %>% 
  mutate(site_label = recode(site_label,
                             "Malignant brain cancer" = "mal_brain",
                             "Malignant brain cancer (incl pituitary gland, craniopharyngeal duct and pineal gland)" = "mal_brain_plus_glands"),
         sex_label = recode(sex_label,
                            "All Persons" = "all",
                            "Males" = "male",
                            "Females" = "female"))

age_groups_prev <- c("Under 45", "45-64", "65+", "All Ages")



prevalence_age_groups <- prevalence_clean %>% 
  filter(sectlab %in% age_groups_prev) %>% 
  rename(age_group = sectlab,
         cancer_site = site_label,
         sex = sex_label) %>% 
  mutate(age_group = recode(age_group,
                            "Under 45" = "< 45",
                            "All Ages" = "all"))

prevalence_diagnosis_groups <- prevalence_clean %>% 
  filter(!sectlab %in% age_groups_prev) %>% 
  rename(years_since_diagnosis = sectlab,
         cancer_site = site_label,
         sex = sex_label) %>% 
  mutate(years_since_diagnosis = recode(years_since_diagnosis,
                                        "Up to 1 year" = "< 1",
                                        "> 1 to 5 years" = "1 - 5",
                                        "> 5 to 10 years" = "6 - 10",
                                        "> 10 to 20 years" = "11 - 20",
                                        "Total up to 20 years" = "≤ 20"))


rm(prev_scotland, prev_data, prevalence_clean, age_groups_prev)


# ------------- END ---------------



