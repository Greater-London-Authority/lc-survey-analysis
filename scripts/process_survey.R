#'[Script]#'*process_survey.R*
#'[Project]#'*tl_survey_heatplan_analysis (https://github.com/Greater-London-Authority/tl_survey_heatplan_analysis.git)*
#'[Author]#'*R. Dogan, M.Slade & M. Tibbles*
#'[Last Update]#'*18/11/2024*
#'[Description]#'*This script blah.*
#'[____________________________________________________________________________]

# Loading libraries
# ==============================================================================
library(readxl)
library(tidyverse) # allows you to do standard data manipulation / analysis, pandas equivalent
library(janitor) # this cleans names
`%ni%` <- Negate(`%in%`) # reverses in for not in. allows you to use the true / false later on

# loading data
# ==============================================================================
# change the string (excel files as per project files)
data_path <- "raw_data/export-1664008_100826_1557.xlsx"
demographics_path <- "raw_data/Talk London anon profiles - matched.xlsx"
#business_data_path <- "raw_data/Businesses Survey Data Talk London.xlsx"

raw_data <- readxl::read_xlsx(
  data_path, # as per line 19 above
  sheet = 1, #says select sheet 1
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0,
  .name_repair = "unique"
) 

raw_demographics <-read_xlsx(
  demographics_path, # as per line 19 above
  sheet = 1, #says select sheet 1
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0,
  .name_repair = "unique"
)

# Clean raw data:
# 1. keeping complete cases only and;
# 2. Removing duplicates, keeping earliest record
# TODO Note that there are 5 cases with NA uid (these are excluded)
# row 49 you're uploading a new dataframe which is clean. row 50 says keep columns: response ID, contact ID, drop those between the latter and URL Variable: uid
# row 51 renames the column names (the one to the right of the = sign is the original, left is the new name)
# row 57 says remove the uid's which are blank / NA
raw_data_clean <- raw_data %>%
  select(-c(`Response ID`, `Contact ID`:`URL Variable: uid`)) %>%
  rename(
    'time_started' = 'Time Started',
    'date_submitted' = 'Date Submitted',
    'status' = 'Status',
    'uid'= 'URL Variable: user-uid'
  ) %>%
  filter(!is.na(uid)) %>%
  group_by(uid) %>%
  # mutate(n_uid = n()) %>%
  # ungroup() %>%
  #mutate(dup = case_when(n_uid>1~1, .default=0)) %>% # Uncomment these three lines to check for duplicates
  mutate(time_started = as.POSIXlt(time_started, format="%e %B %Y %H:%M:%S")) %>%
  group_by(uid) %>%
  filter(time_started==min(time_started)) %>%
  ungroup() 

# Clean raw demographics, removing duplicates (oddly there are quite a lot)
raw_demographics_clean <- raw_demographics %>%
  select(`UID`:`Housing`) %>%
  clean_names() %>%
  distinct(uid, .keep_all=T)

# Merge
match_data <- raw_data_clean %>% 
  dplyr::left_join(raw_demographics_clean, by = join_by(uid)) #%>%
  #select(-uid) %>%
  #mutate(nuid = 1000+row_number())

# QA to check that N for master merge is same as N for clean respondent data (both expected to be different than from demog clean)
nrow(raw_data)
nrow(raw_data_clean)
nrow(raw_demographics_clean)
nrow(match_data)

# Export QUAL responses
# TODO Other please specificy qual mc1 repeated twice (awareness)
qual_data <- match_data  %>%
  select(uid, imptec__open1, ends_with(paste0("...",0:1000)), starts_with('Other')) %>% # Other columns end with ...X, due to duplicate names
  select(!ends_with(paste0("cx",0:1000))) %>%
  select(!ends_with(paste0('...', c(27,163,235)))) %>% # Only necessary bc of presence of three other columns - otherwise comment out
  select(
    uid, imptec__open1, seq_len(ncol(.[!grepl('__sc', names(.))]))[seq_len(ncol(.[!grepl('__sc', names(.))]))%%2==1], contains('__sc') # Select EVEN Other columns (qual other is always after data other), and then sc other columns
  )
write.csv(qual_data, 'processed_data/qual_data-260827.csv')

# Remove QUAL responses from main dataset
match_data_noqual <- match_data %>%
  select(-names(qual_data)[names(qual_data)!='uid']) %>%
  select(!ends_with(paste0('...', c(27,163,235)))) # Only necessary bc of presence of three other columns - otherwise comment out
  

# Save matched data (with NA demographic rows removed, effectively a full dataset)
readr::write_csv(match_data_noqual, "processed_data/processed_survey-260819.csv")
