library(janitor)
library(tidyverse)
library(readxl)

all_ann_inc_scotland <- read_xls("data/all_ann_inc.xls", sheet = 1)
all_ann_inc_region <- read_xls("data/all_ann_inc.xls", sheet = 2)
all_ann_inc_board <- read_xls("data/all_ann_inc.xls", sheet = 3)
all_ann_notes_area <- read_xls("data/all_ann_inc.xls", sheet = 4)
all_ann_notes_sex <- read_xls("data/all_ann_inc.xls", sheet = 5)
all_ann_notes_cancer_type <- read_xls("data/all_ann_inc.xls", sheet = 6)
all_ann_inc_data <- read_xls("data/all_ann_inc.xls", sheet = 7)

all_sum_inc_data <- read_xls("data/all_sum_inc.xls", sheet = 6)

excel_sheets("data/all_ann_inc.xls")
excel_sheets("data/all_sum_inc.xls")
