library(janitor)
library(tidyverse)
library(readxl)
library(stringi)

all_ann_inc_scotland <- read_xls("data/all_ann_inc.xls", sheet = 1)
all_ann_inc_region <- read_xls("data/all_ann_inc.xls", sheet = 2)
all_ann_inc_board <- read_xls("data/all_ann_inc.xls", sheet = 3)
all_ann_notes_area <- read_xls("data/all_ann_inc.xls", sheet = 4)
all_ann_notes_sex <- read_xls("data/all_ann_inc.xls", sheet = 5)
all_ann_notes_cancer_type <- read_xls("data/all_ann_inc.xls", sheet = 6)
all_ann_inc_data <- read_xls("data/all_ann_inc.xls", sheet = 7)

all_sum_inc_data <- read_xls("data/all_sum_inc.xls", sheet = 6)

#excel_sheets("data/all_ann_inc.xls")
#excel_sheets("data/all_sum_inc.xls")

#unique(all_ann_inc_data$trans1.1993)

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
                                   "(Incidence)" = "")))

# Part A: AGES 

all_ann_inc_data_ages <- all_ann_inc_data_clean %>% 
    filter(str_detect(age_label, c("num_", "inc_"))) %>% 
  #mutate(age_label = str_replace(age_label, "Crude Rate ()", "inc_all"),
         #age_label = stri_replace_all_fixed(age_label, "()", "")) %>% 
  #mutate(inc_flag = case_when(str_detect(age_label, "num") ~ "num",
                             # T ~ "inc")) %>% 
  #mutate(age_label = str_replace_all(age_label, c("num_" = "", "inc_" = ""))) %>% 
  pivot_wider(names_from = inc_flag, 
               values_from = values)






#%>% 
  #pivot_longer(cols = "inc_5":"sir_upper_95_percent_ci",
              # names_to = "age_inc",
              # values_to = "incidence")
  
 
unique(all_ann_inc_data_clean$age_label)
unique(all_ann_inc_data$age_label)

names(all_ann_inc_data_ages)
unique(all_ann_inc_data_ages$age_label)
sum(is.na(all_ann_inc_data_ages$num))
sum(is.na(all_ann_inc_data_ages$num))

sum(is.na(all_ann_inc_data))
sum(is.na(all_ann_inc_data_clean))

sum(is.na(all_ann_inc_data_ages))

distinct(all_ann_inc_data_ages)

all_ann_inc_data_clean %>% 
  filter(!distinct(all_ann_inc_data_ages))

summary(all_ann_inc_data_ages)

filtered <- all_ann_inc_data_ages %>% 
  filter(sex_label == "Males",
         year == "1993",
         hb_label == "SCOTLAND")

filtered %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

all_ann_inc_data_ages %>% 
  summarise(across(.fns = ~sum(is.na(.x))))
