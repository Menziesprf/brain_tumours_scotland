library(janitor)
library(tidyverse)
library(readxl)
library(stringi)

# --------- DATASET 1: Annual Incidence -----------

excel_sheets("data/cancer-incidence-brain-and-cns.xls")

inc_notes <- read_xls("data/cancer-incidence-brain-and-cns.xls", sheet = 2)

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
         year = as.double(year),
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
                             "All brain and CNS tumours (malignant and non-malignant)" = "all"),
         hb_label = recode(hb_label,
                           "highland and argyll" = "highland")) %>% 
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
               values_from = values) %>% 
  mutate(age = recode(age,
                      "5.." = "05-09"))

  
# 1B: Incidence + stats 

incidence_all_stats <- all_ann_inc_data_clean %>% 
  filter(str_detect(age, "Crude|EASR|WASR|Standardised|SIR")) %>% 
  mutate(age = recode(age,
                      "Standardised  Ratio" = "SIR")) %>% 
  pivot_wider(names_from = age,
              values_from = values) %>%   # CONSIDER DROPPING SIR == 100.00000 only seem to be in scotland rows
  clean_names() %>% 
  pivot_longer(cols = crude_rate:sir_upper_95_percent_ci,
              names_to = "value_type",
               values_to = "value")


# Regions 

north <- c("western isles", "highland", "grampian", "tayside", "orkney", "shetland", "north of scotland")
west <-  c("forth valley", "greater glasgow and clyde", "lanarkshire", "ayrshire and arran", "west of scotland")
southeast = c("fife", "lothian", "borders", "dumfries and galloway", "south east of scotland")

unique(incidence_all_ages$hb)



regional_incidence_map_prep <- incidence_all_ages %>% 
  filter(hb != "scotland") %>% 
  mutate(region_flag = case_when(
                          hb %in% north ~ "north",
                          hb %in% west ~ "west",
                          hb %in% southeast ~ "southeast",
                          T ~ "NA"))

regions_temp <- regional_incidence_map_prep %>% 
  filter(hb %in% c("north of scotland", "west of scotland", "south east of scotland")) %>% 
  select(-hb) %>% 
  rename(num_cases_region = num_cases,
         inc_region = incidence)

regional_incidence_map <- regional_incidence_map_prep %>% 
  filter(!hb  %in% c("north of scotland", "west of scotland", "south east of scotland")) %>% 
  left_join(regions_temp, by = c("cancer_type", "sex", "age", "year", "region_flag"))



rm(all_ann_inc_data, all_ann_inc_data_clean, regions_temp, regional_incidence_map_prep)

# ------------ DATASET 2: Mortality  ------------


excel_sheets("data/mal_cns_ann_mort.xls")

mal_cns_mort_data <- read_xls("data/mal_cns_ann_mort.xls", sheet = 7)

mortality_clean <- mal_cns_mort_data %>% 
  select(-id, -hbnew, -sitenew, -sexnew, -label) %>% 
  pivot_longer(cols = trans1.1993:trans1.2018,
               names_to = "year",
               values_to = "values") %>% 
  mutate(year = str_replace(year, "trans1.", ""),
         year = as.double(year),
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
              values_from = values) %>% 
  mutate(age = recode(age,
                      "5.." = "05-09"))

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

survival_notes <- read_xlsx("data/estimates-of-survival-from-brain-and-other-cns-cancers.xlsx", sheet = 6) %>% 
  pull()

survival_notes
survival_notes2 <- read_xlsx("data/estimates-of-survival-from-brain-and-other-cns-cancers.xlsx", sheet = 7)

# survival @ 1, 3 5 & 10 years 1987 - 2017
# Brain and other CNS (ICD-9 191-192; ICD-10 C70-C72, C75.1-C75.3) - only malignant neoplasms, includes glands and ducts
# NA's when less than 10 patients alive at 1, 5 or 10 years or when there is little change from previous time period
# NA's dropped 

unique(survival_all_data$cancer_site_grouping)

survival_clean <- survival_all_data %>% 
  mutate(across(.cols = observed_survival_percent:upper_95_percent_ci_for_net_survival, 
                .fns = as.double)) %>% 
  select(-cancer_site_grouping) %>% 
  drop_na()

survival_clean_nas <- survival_all_data %>% 
  mutate(across(.cols = observed_survival_percent:upper_95_percent_ci_for_net_survival, 
                .fns = as.double)) %>% 
  select(-cancer_site_grouping) 
  
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
      

excel_sheets("data/deprivation-all-cancers.xls")

dep_all <- read_xls("data/deprivation-all-cancers.xls", sheet = 1, range = "A11:G18", col_names = T) %>% 
  clean_names()

deprivation_all_incidence_clean <- dep_all %>% 
  select(-x2, -x4) %>% 
  filter(!is.na(easr)) %>% 
  rename(incidence_easr = easr,
         inc_lower_ci_95 = lower_95_percent_ci,
         inc_upper_ci_95 = upper_95_percent_ci)

dep_lung <- read_xls("data/deprivation-lung.xls", sheet = 1, range = "A11:G18", col_names = T) %>% 
  clean_names()

deprivation_lung_incidence_clean <- dep_lung %>% 
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

# risk_scotland <- read_xls("data/mal_cns_risk.xls", sheet = 1)
# risk_data <- read_xls("data/mal_cns_risk.xls", sheet = 4)




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


# ---------- CENSUS ----------

# years, all scotland

pop_scotland_male <- read_csv("data/census/mid-year-pop-est-19-time-series-7_Population estimates 1981-2019.csv",
                         skip = 44, col_names = TRUE, n_max = 39)

pop_scotland_female <- read_csv("data/census/mid-year-pop-est-19-time-series-7_Population estimates 1981-2019.csv",
                              skip = 86, col_names = TRUE, n_max = 39)

# selecting years 1994, 2004, 2014, 2019

years <- c(1994, 2004, 2014, 2019)

pop_scotland_male_clean <- pop_scotland_male %>% 
  rename(year = Males) %>% 
  select(-"All Ages", -X94) %>% 
  filter(year %in% years) %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(sex = "male")

pop_scotland_female_clean <- pop_scotland_female %>% 
  rename(year = Females) %>% 
  select(-"All Ages", -X94) %>% 
  filter(year %in% years) %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(sex = "female")

groups <- c(paste(seq(0, 85, by = 5), seq(0 + 5 - 1, 90 - 1, by = 5),
                sep = "-"), paste(90, "+", sep = ""))


pop_clean <- pop_scotland_male_clean %>% 
  bind_rows(pop_scotland_female_clean) %>% 
  mutate(age = recode(age,
                      "90+" = "90"),
         age = as.numeric(age),
         age_group = cut(age, 
                         breaks = c(seq(0, 90, by = 5), Inf), 
                         labels = groups, 
                         right = FALSE),
         age = as.character(age))  %>% 
  group_by(age_group, sex, year) %>% 
  mutate(count_group = sum(count)) %>% 
  select(-age, -count) %>% 
  distinct() %>%
  ungroup() %>% 
  group_by(sex, year) %>% 
  mutate(total_pop = sum(count_group)) %>% 
  group_by(age_group) %>% 
  mutate(percentage_pop = (count_group/total_pop*100))

rm(pop_scotland_female, pop_scotland_female_clean, pop_scotland_male, 
   pop_scotland_male_clean, years)

# 2019, healthboards

pop_2019_male <- read_csv("data/census/mid-year-pop-est-19-time-series-3/mid-year-pop-est-19-time-series-3_2019.csv",
                     skip = 21, col_names = TRUE, n_max = 17)

pop_2019_female <- read_csv("data/census/mid-year-pop-est-19-time-series-3/mid-year-pop-est-19-time-series-3_2019.csv",
                            skip = 40, col_names = TRUE, n_max = 17)

pop_2019_all <- read_csv("data/census/mid-year-pop-est-19-time-series-3/mid-year-pop-est-19-time-series-3_2019.csv",
                         skip = 2, col_names = TRUE, n_max = 17)

pop_2019_male_clean <- pop_2019_male %>% 
  drop_na() %>% 
  rename(hb = Males) %>% 
  select(-Code, -"All Ages") %>% 
  filter(hb != "Scotland") %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(sex = "male")

pop_2019_female_clean <- pop_2019_female %>% 
  drop_na() %>% 
  rename(hb = Females) %>% 
  select(-Code, -"All Ages") %>% 
  filter(hb != "Scotland") %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(sex = "female")

pop_2019_hb_clean <- pop_2019_female_clean %>% 
  bind_rows(pop_2019_male_clean) %>% 
  mutate(age = recode(age,
                      "90+" = "90"),
         age = as.numeric(age),
         age_group = cut(age, 
                         breaks = c(seq(0, 90, by = 5), Inf), 
                         labels = groups, 
                         right = FALSE),
         age = as.character(age)) %>% 
  group_by(age_group, sex, hb) %>% 
  mutate(count_group = sum(count)) %>% 
  select(-age, -count) %>% 
  distinct() %>%
  ungroup() %>% 
  group_by(sex, hb) %>% 
  mutate(total_pop = sum(count_group)) %>% 
  group_by(age_group) %>% 
  mutate(percentage_pop = (count_group/total_pop*100))

pop_2019_all_clean <- pop_2019_all %>% 
  drop_na() %>% 
  rename(hb = Persons) %>% 
  select(-"All Ages") %>% 
  filter(hb != "Scotland") %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(age = recode(age,
                      "90+" = "90"),
         age = as.numeric(age),
         over_65 = case_when(age < 65 ~ "under_65",
                             T ~ "over_65")) %>% 
  group_by(over_65, hb) %>% 
  mutate(count_over_65 = sum(count)) %>%
  select(-age, -count) %>% 
  distinct() %>%
  ungroup() %>% 
  group_by(hb) %>% 
  mutate(total_pop = sum(count_over_65)) %>% 
  group_by(over_65) %>% 
  mutate(percentage_pop = (count_over_65/total_pop*100))

pop_2017_all <- read_csv("data/census/mid-year-pop-est-19-time-series-3/mid-year-pop-est-19-time-series-3_2017.csv",
                         skip = 2, col_names = TRUE, n_max = 17)

# hb codes are out of date
pop_2017_all_clean <- pop_2017_all %>% 
  drop_na() %>% 
  rename(hb = Persons) %>% 
  select(-"All Ages", -Code) %>% 
  filter(hb != "Scotland") %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(age = recode(age,
                      "90+" = "90"),
         age = as.numeric(age),
         over_65 = case_when(age < 65 ~ "under_65",
                             T ~ "over_65")) %>% 
  group_by(over_65, hb) %>% 
  mutate(count_over_65 = sum(count)) %>%
  select(-age, -count) %>% 
  distinct() %>%
  ungroup() %>% 
  group_by(hb) %>% 
  mutate(total_pop = sum(count_over_65)) %>% 
  group_by(over_65) %>% 
  mutate(percentage_pop = (count_over_65/total_pop*100))

pop_1994_all <- read_csv("data/census/mid-year-pop-est-19-time-series-3/mid-year-pop-est-19-time-series-3_1994.csv",
                         skip = 2, col_names = TRUE, n_max = 17)

pop_1994_all_clean <- pop_1994_all %>% 
  drop_na() %>% 
  rename(hb = Persons) %>% 
  select(-"All ages", -Code) %>% 
  filter(hb != "Scotland") %>% 
  pivot_longer(cols = "0":"90+",
               names_to = "age",
               values_to = "count") %>% 
  mutate(age = recode(age,
                      "90+" = "90"),
         age = as.numeric(age),
         over_65 = case_when(age < 65 ~ "under_65",
                             T ~ "over_65")) %>% 
  group_by(over_65, hb) %>% 
  mutate(count_over_65 = sum(count)) %>%
  select(-age, -count) %>% 
  distinct() %>%
  ungroup() %>% 
  group_by(hb) %>% 
  mutate(total_pop = sum(count_over_65)) %>% 
  group_by(over_65) %>% 
  mutate(percentage_pop = (count_over_65/total_pop*100))

rm(pop_2019_female, pop_2019_female_clean, pop_2019_male, 
   pop_2019_male_clean, groups, pop_2019_all, pop_2017_all)

# midyear pop estimates from "national records of scotland" 


# -------------- Map prep -------------

hb_code_df<- pop_2019_all_clean %>%
  ungroup() %>% 
  select(hb, Code) %>% 
  distinct() %>% 
  mutate(hb = str_to_lower(hb))

health_boards <- c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife", 
                    "Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", 
                    "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles") %>% 
  str_to_lower()

#hb_codes <- c("S08000015", "S08000016", "S08000017", "S08000019", "S08000020", "S08000022", "S08000024",
              #"S08000025", "S08000026", "S08000028", "S08000029", "S08000030", "S08000031", "S08000032")

# Incidence

incidence_map_prep <- incidence_all_stats %>% 
  filter(hb %in% health_boards) %>% 
  left_join(hb_code_df, by = "hb")

# ------------ Obesity ----------

excel_sheets("data/obesity/tables.xls")

obesity_notes <- read_xls("data/obesity/tables.xls", sheet = 1)

obesity_data_raw <- read_xls("data/obesity/tables.xls", sheet = 4, skip = 3, col_names = TRUE, n_max = 10) %>% 
  clean_names()

obesity_data <- obesity_data_raw %>% 
  select(-x3, -x4, -x6, -x7, -x8, -x9, -x11, -x12, -x13, -x14) %>% 
  slice(-c(1:4))
  
obesity_bmi_25_plus <- obesity_data %>% 
  slice(c(2,3)) %>% 
  mutate(x1995f = as.double(x1995f)) %>% 
  pivot_longer(cols = x1995f:x2019, 
               names_to = "year", 
               values_to = "percent_pop") %>% 
  rename(age = body_mass_index_bmi_kg_m2) %>% 
  mutate(year = str_extract(year, "[0-9]+"),
         age = str_remove(age, "c"),
         year = as.double(year),
         bmi = "25+") %>% 
  filter(age == "16-64") 

obesity_bmi_30_plus <- obesity_data %>% 
  slice(c(5,6)) %>% 
  mutate(x1995f = as.double(x1995f)) %>% 
  pivot_longer(cols = x1995f:x2019, 
               names_to = "year", 
               values_to = "percent_pop") %>% 
  rename(age = body_mass_index_bmi_kg_m2) %>% 
  mutate(year = str_extract(year, "[0-9]+"),
         age = str_remove(age, "c"),
         year = as.double(year),
         bmi = "30+") %>% 
  filter(age == "16-64")

obesity_clean <- obesity_bmi_25_plus %>% 
  bind_rows(obesity_bmi_30_plus)

rm(obesity_bmi_25_plus, obesity_bmi_30_plus, obesity_data, obesity_data_raw, obesity_notes)

# source: https://www.gov.scot/publications/diet-healthy-weight-monitoring-report-2020/

# ----------- Leukaemia Survival ------------

excel_sheets("data/leukaemia/estimates-of-survival-from-leukaemias.xlsx")

leukaemia_raw <- read_xlsx("data/leukaemia/estimates-of-survival-from-leukaemias.xlsx", sheet = 5) %>% 
  clean_names()

leukaemia_survival_clean <- leukaemia_raw %>% 
  mutate(across(.cols = observed_survival_percent:upper_95_percent_ci_for_net_survival, 
              .fns = as.double)) %>% 
  select(-cancer_site_grouping) %>% 
  drop_na()

rm(leukaemia_raw)
# ------------- END ---------------



