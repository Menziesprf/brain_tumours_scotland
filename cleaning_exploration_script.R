library(janitor)
library(tidyverse)
library(readxl)
library(stringi)

# --------- DATASET 1: Annual Incidence -----------

# all_ann_inc_scotland <- read_xls("data/all_ann_inc.xls", sheet = 1)
# all_ann_inc_region <- read_xls("data/all_ann_inc.xls", sheet = 2)
# all_ann_inc_board <- read_xls("data/all_ann_inc.xls", sheet = 3)
# all_ann_notes_area <- read_xls("data/all_ann_inc.xls", sheet = 4)
# all_ann_notes_sex <- read_xls("data/all_ann_inc.xls", sheet = 5)
# all_ann_notes_cancer_type <- read_xls("data/all_ann_inc.xls", sheet = 6)
 all_ann_inc_data <- read_xls("data/all_ann_inc.xls", sheet = 7)

#all_sum_inc_data <- read_xls("data/all_sum_inc.xls", sheet = 6)

#excel_sheets("data/all_ann_inc.xls")
#excel_sheets("data/all_sum_inc.xls")


all_ann_inc_data_clean <- all_ann_inc_data %>%
  select(-id, -hbnew, -sitenew, -sexnew, -label) %>% 
  mutate(trans1.1993 = as.double(trans1.1993),
         trans1.1994 = as.double(trans1.1994),
         trans1.1995 = as.double(trans1.1995),
         trans1.1996 = as.double(trans1.1996),
         trans1.1997 = as.double(trans1.1997),
         trans1.1998 = as.double(trans1.1998),
         trans1.1999 = as.double(trans1.1999)) %>%
  pivot_longer(cols = trans1.1993:trans1.2017,
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

all_ann_inc_data_ages <- all_ann_inc_data_clean %>% 
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

all_ann_inc_stats <- all_ann_inc_data_clean %>% 
  filter(str_detect(age, "Crude|EASR|WASR|Standardised|SIR")) %>% 
  mutate(age = recode(age,
                      "Standardised  Ratio" = "SIR")) %>% 
  pivot_wider(names_from = age,
              values_from = values) %>%   # CONSIDER DROPPING SIR == 100.00000 only seem to be in scotland rows
  clean_names()


  

rm(all_ann_inc_data, all_ann_inc_data_clean)

# ------------ DATASET 2: Mortality  ------------

excel_sheets("data/mal_ann_mort.xls")
excel_sheets("data/mal_cns_ann_mort.xls")

#mal_cns_mort_scotland <- read_xls("data/mal_cns_ann_mort.xls", sheet = 1)
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

excel_sheets("data/mal_cns_survival.xlsx")

survival 
  
  
  
  
  
  
  
  
  
  
  







# ------------- END ---------------
