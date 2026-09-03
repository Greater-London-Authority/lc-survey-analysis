#'[Script]#'*working_test.R*
#'[Project]#'*nye25_fireworks (https://github.com/Greater-London-Authority/nye25_fireworks.git)*
#'[Author]#'* M. Tibbles & R.Dogan*
#'[Last Update]#'*24/02/2026*
#'[Description]#'*This script blah.*
#'[Libraries]
library(tidyverse)
library(openxlsx)
library(readxl)
#'[Globals]
#'
# SUrvey questions (label=long)
SURVEY_QUESTIONS <-
  c(
    'awareness__mc1'='Awareness of public services',
    'access__mat1'='Frequency of public services use',
    'satisfaction__sc1'='Satisfaction of public services',
    'diffsame__mat2' = 'Use of public services for different or same reasons',
    'reason__mc2'='Reason for use of public services',
    'resolve__sc2'='Whether services resolved issues',
    'satisfaction__mat3'='Satisfaction with support',
    'dissatisfaction__cx1'='Reasons for dissatisfaction with support',
    'influence__sc3'='Influence on services',
    'interestimp__sc4'='Interest in improving services',
    'difficult__mc3'='Barriers to accessing services',
    'important__mat4'='Important factors in services',
    'community__mc4'='Community strengths',
    'commstrengths__sc5'='Recognition of community strengths in services',
    'commeasier__mc5'='Actions to make access easier',
    'tec__mc6'='Use of digital platforms',
    'confidence__sc6'='Confidence in digital platforms',
    'tecbarriers__mc7'='Barriers to using digital platforms',
    'access__mc8'='Alternative actions taken against digital access',
    'tecwell__sc7'='Perceptions of digital platforms usability',
    'whydifficult__mc9'='Reasons for difficulties when using digital platforms',
    'tecdata__sc8'='Confidence in technology within public services',
    'imptec__open1'= 'Use of technology in improving',
    'AIsupp__sc9'='AI in public services',
    'happierhealthier__mc10'='What would make Londoners happier and healthier',
    'disability__sc10'='Disability Status',
    'involve__sc11'='Involvement in future research'
  )

# Single choice value levels
SC_VALUE_LEVELS <- list(
  c("Very satisfied", "Somewhat satisfied", "Somewhat dissatisfied", "Very dissatisfied", "Don't know"),
  c("Strongly agree", "Tend to agree", "Tend to disagree", "Strongly disagree"),
  c("A lot", "A little", "Not a lot", "Not at all", "Don't know"),
  c("Very interested", "Fairly interested", "Not very interested", "Not at all interested", "Don't know"),
  c("A lot", "A little", "Not a lot", "Not at all", "Don't know"),
  c("Very confident", "Somewhat confident", "Not very confident", "Not confident at all", "Not applicable – I do not use technology/digital channels to access public services"),
  c("Very well", "Quite well", "Not very well", "Not at all well", "Don't know"),
  c("Very confident", "Somewhat confident", "Not very confident", "Not at all confident", "Don't know"),
  c("Strongly support", "Somewhat support", "Somewhat oppose", "Strongly oppose", "Don't know"),
  c("Yes", "No", "Prefer not to say"),
  c("Yes", "No")
)

# Mat value levels (left)
MAT_VALUE_LEVELS_LEFT <- list(
  c('Seeing a doctor or getting healthcare GP surgeries, hospitals, ambulances, pharmacies, dentists, mental health support', 'Getting help to stay safe and well', 'Finding or keeping a home', 'Education, childcare and support for children and young people', 'Getting around London', 'Emergency help and community safety', 'Keeping neighbourhoods clean and working well', 'Help to live healthier lives', 'Services that shape the places people live'),
  c('For the same reason', 'For different reasons'),
  c('Seeing a doctor or getting healthcare', 'Getting help to stay safe and well', 'Finding or keeping a home', 'Education, childcare and support', 'Getting around London', 'Emergency help and community safety', 'Keeping neighbourhoods clean and working well', 'Help to live healthier lives', 'Services that shape the places people live'),
  c('Speed of support', 'Clear information', 'Feeling listened to / supported')
)

# Matrix value levels (right)
MAT_VALUE_LEVELS_RIGHT <- list(
  c('A few times a week', 'Once a week', 'A few times a month', 'Once a month', 'Every 1-3 months', 'Every 3-6 months', 'Every six months', 'Less often than once every six months', 'Never'),
  c('Yes', 'No'),
  c('Very satisfied', 'Somewhat satisfied', 'Somewhat dissatisfied', 'Very dissatisfied', 'Don\'t know', 'Not applicable - I did not use this service at the same time as others'),
  c('Very important', 'Quite important', 'Not very important', 'Not at all important', 'Don\'t know') #'Don\'t know / Not applicable')#, # Reminder the apostrophe used for the don't know is different to the other two above due to differences in Alchemer export
)

# Checkbox grid value levels
CX_VALUE_LEVELS <- list(
  c()
)

# Single choice NET levels
SC_NET_LEVELS <- list(
  # list(
  #   'Very satisfied / Somewhat satisfied' = c('Very satisfied', 'Somewhat satisfied'),
  #   'Not very satisfied / Not at all satisfied' = c('Not very satisfied', 'Not at all satisfied'), # TODO
  #   'Don\'t know' = c('Don\'t know')
  # ),
  list(
    'Very satisfied / Somewhat satisfied' = c('Very satisfied', 'Somewhat satisfied'),
    'Somewhat dissatisfied / Very dissatisfied' = c('Somewhat dissatisfied', 'Very dissatisfied'), 
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'Strongly agree / Tend to agree' = c('Strongly agree', 'Tend to agree'),
    'Strongly disagree / Tend to disagree' = c('Strongly disagree', 'Tend to disagree')
  ),
  list(
    'A lot / A little' = c('A lot', 'A little'),
    'Not a lot / Not at all' = c('Not a lot', 'Not at all'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'Very interested / Fairly interested' = c('Very interested', 'Fairly interested'),
    'Not very interested / Not at all interested' = c('Not very interested', 'Not at all interested'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'A lot / A little' = c('A lot', 'A little'),
    'Not a lot / Not at all' = c('Not a lot',  'Not at all'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'Very confident / Somewhat confident' = c('Very confident', 'Somewhat confident'),
    'Not very confident / Not confident at all' = c('Not very confident', 'Not confident at all'),
    'Not applicable – I do not use technology/digital channels to access public services' = c ('Not applicable – I do not use technology/digital channels to access public services')
  ),
  list(
    'Very well / Quite well' = c('Very well', 'Quite well'),
    'Not very well / Not at all well' = c('Not very well', 'Not at all well'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'Very confident / Somewhat confident' = c('Very confident', 'Somewhat confident'),
    'Not very confident / Not at all confident' = c('Not very confident', 'Not at all confident'),
    'Don\'t know' = c('Don\'t know')
  ),
  list(
    'Strongly support / Somewhat support' = c('Strongly support', 'Somewhat support'),
    'Strongly oppose / Somewhat oppose' = c('Somewhat oppose', 'Strongly oppose'),
    'Don\'t know' = c('Dont\'t know')
  ) #,
  # add the other questions, even tho they are not actual nets
)
# TODO old names!!!!
names(SC_NET_LEVELS) <- c(
  'satisfaction__sc1', 'resolve__sc2', 'influence__sc3', 'interestimp__sc4',
  'commstrengths__sc5', 'confidence__sc6', 'tecwell__sc7', 'tecdata__sc8','AIsupp__sc9'
)
                          
# Matrix NET levels
MAT_NET_LEVELS <- list(
  list(
    'Access at all' =c('A few times a week', 'Once a week', 'A few times a month', 'Once a month', 'Every 1-3 months', 'Every 3-6 months', 'Every six months', 'Less often than once every six months'),
    'Never' = c('Never')
  ),
  list('Yes' = c('Yes'),
       'No' = c('No')
  ),
  list('Very satisfied / Somewhat satisfied' = c('Very satisfied', 'Somewhat satisfied'),
       'Very dissatisfied / Somewhat dissatisfied' = c('Very dissatisfied', 'Somewhat dissatisfied'),
       'Not applicable - I did not use this service at the same time as others' = c('Not applicable - I did not use this service at the same time as others')
  ),
  list('Very important / Quite important' = c('Very important', 'Quite important'),
       'Not very important / Not at all important' = c('Not very important', 'Not at all important'),
       'Don\'t know' = c('Don\'t know'))
)
names(MAT_NET_LEVELS) <- c('access__mat1', 'diffsame__mat2', 'satisfaction__mat3', 'important__mat4')

# Demographic NET levels
DEMOG_NET_LEVELS <- list(
  list(
    'White' = c('English / Welsh / Scottish / Northern Irish / British', 'Irish', 'Gypsy or Irish Traveller', 'Any other White background'),
    'Mixed / Multiple ethnic groups' = c('White and Black Caribbean', 'White and Black African', 'White and Asian', 'Any other Mixed or Multiple background'),
    'Asian' = c('Indian', 'Pakistani', 'Bangladeshi', 'Chinese', 'Any other Asian background'),
    'Black' = c('African', 'Caribbean', 'Any other Black / African / Caribbean background'),
    'Other' = c('Arab', 'Latin American', 'Any other ethnic group')
  ),
  list(
    'Working' = c('Working - Full time (30+ hours)', 'Working - Part time (9-29 hours)'),
    'Not working' = c('Unemployed', 'Not working - disabled', 'Not working - looking after house/children'),
    'Retired' = c('Not working - retired'),
    'Student' = c('Student, with some part time work', 'Student, without any part time work'),
    'Other' = c('Other')
  )
)
names(DEMOG_NET_LEVELS) <- c('ethnicity', 'employment')

# Grouping columns
GROUP_VARS <- c(
  "age_group",  "gender", "ethnicity",  "employment",
  "inner_outer_borough", "disability__sc10"
)
# can't group by an mc or mat variable - possible but will take a lot of work ti implement
GROUP_LEVELS <- list(
  c("16-24", "25-34","35-44","45-54","55-64","65+"),
  c("Man", "Woman", "Other"),
  c("White", "Black", "Asian", "Mixed / Multiple ethnic groups", "Other"),
  c("Working", "Not working", "Retired", "Student", "Other"),
  c("Inner borough", "Outer borough", "Outside of London"),
  c('Yes', 'No', 'Prefer not to say')
)
names(GROUP_LEVELS) <- GROUP_VARS

# Date for suffix on exports (ymd)
DATE <- 260827

#'[Source files (functions)]
source('scripts/funs.R')
#'[____________________________________________________________________________]
#

# Loading survey
survey_df <- read_csv("processed_data/processed_survey-260819.csv") 


# Process single choice (SC) questions
#===============================================================================

# Main summaries
df_sc <- survey_df %>%
  dplyr::select(ends_with(paste0('__sc', 0:1000))) 
df_sc_list <- singleCode_processing(df=df_sc, value_levs=SC_VALUE_LEVELS)
# Net summaries
df_sc_net_list <- singleCode_processing(df=df_sc, net_levs=SC_NET_LEVELS) # TODO need to give actual variable names
# Grouped analysis (for non-netted variables)
df_sc_group_values_list <- lapply(
  GROUP_VARS, function(group) {
    df_sc_group <- survey_df %>% dplyr::select(ends_with(paste0('__sc', 0:1000)), group) #%>% select(-names(SC_NET_LEVELS)) # TODO uncomment select bit when SC_NET_LEVELS is specificed correct
    singleCode_processing(df=df_sc_group, value_levs=SC_VALUE_LEVELS,  group=group, net_levs_group=DEMOG_NET_LEVELS, group_levs=GROUP_LEVELS)
  }
)
# Grouped analysis (for netted variables)
df_sc_group_net_list <- lapply(
  GROUP_VARS, function(group) {
    df_sc_group <- survey_df %>% dplyr::select(ends_with(paste0('__sc', 0:1000)), group) 
    singleCode_processing(df=df_sc_group,  net_levs=SC_NET_LEVELS, group=group, net_levs_group=DEMOG_NET_LEVELS, group_levs=GROUP_LEVELS)
  }
)


# Process multiple choice (MC) questions
#===============================================================================

# Main summaries
df_mc <- survey_df %>%
  dplyr::select(ends_with(paste0('__mc', 0:1000))) 
df_mc_list <- multiChoice_processing(df=df_mc)
# Grouped analysis (all MC variables are non-nets by desing, since are distinct categories) 
# Note too that SC grouping vars are removed from grouping analysis. Grouping MC by SC variables and vice versa is not currently possible given the design of the codebase (it works on each question structure in isolation)
df_mc_group_list <- lapply(
  GROUP_VARS, function(group) {
    df_mc_group <- survey_df %>% dplyr::select(ends_with(paste0('__mc', 0:1000)), group) 
    multiChoice_processing(df=df_mc_group, group=group, net_levs_group=DEMOG_NET_LEVELS, group_levs=GROUP_LEVELS)
  }
)
# add additional argument group_net_levs


# Process matrix (MAT) questions
#===============================================================================
# Main summaries
df_mat <- survey_df %>%
  dplyr::select(ends_with(paste0('__mat', 0:1000))) 
df_mat_list <- matrix_processing(df=df_mat, value_levs_left=MAT_VALUE_LEVELS_LEFT, value_levs_right=MAT_VALUE_LEVELS_RIGHT)
df_mat_net_list <- matrix_processing(df=df_mat, value_levs_left=MAT_VALUE_LEVELS_LEFT, net_levs=MAT_NET_LEVELS) # some new issue here
# TODO Grouped by summary (with recoding of grouping variable)


# Process demographic frequencies
#===============================================================================
df_demog_list <- demographics_processing(survey_df, group_vars=GROUP_VARS, net_levs=DEMOG_NET_LEVELS) #DEMOG_NET_LEVELS


# Export - ordered by question structure
#===============================================================================

# Non-grouped
#----------------------
 
# Detailed
wb <- createWorkbook()
exportToExcel(df_sc_list, sheet_name='sc_values_frequencies', grouped=FALSE, tab_colour='#0000FF', order='structure', publication_ready=FALSE)
exportToExcel(df_sc_net_list, sheet_name='sc_net_frequencies', grouped=FALSE, tab_colour='darkblue', order='structure', publication_ready=FALSE)
exportToExcel(df_mc_list, sheet_name='mc_frequencies', grouped=FALSE, tab_colour='red',order='structure', publication_ready=FALSE)
exportToExcel(df_mat_list, sheet_name='mat_values_frequencies', grouped=FALSE, tab_colour='lightgreen', order='structure', publication_ready=FALSE)
exportToExcel(df_mat_net_list, sheet_name='mat_net_frequencies', grouped=FALSE, tab_colour='darkgreen', order='structure', publication_ready=FALSE)
exportToExcel(df_demog_list, sheet_name='demog_frequencies', grouped=FALSE, tab_colour='pink', order='structure', publication_ready=FALSE)
saveWorkbook(wb, paste0("processed_data/lc_survey_frequencies_detail_",DATE,".xlsx"), overwrite = TRUE)

#  Accessible 
wb <- createWorkbook()
exportToExcel(df_sc_list, sheet_name='sc_values_frequencies', grouped=FALSE, tab_colour='lightblue', order='structure', publication_ready=TRUE)
exportToExcel(df_sc_net_list, sheet_name='sc_net_frequencies', grouped=FALSE, tab_colour='darkblue', order='structure', publication_ready=TRUE)
exportToExcel(df_mc_list, sheet_name='mc_frequencies', grouped=FALSE, tab_colour='red', order='structure', publication_ready=TRUE)
exportToExcel(df_mat_list, sheet_name='mat_values_frequencies', grouped=FALSE, tab_colour='lightgreen', order='structure', publication_ready=TRUE)
exportToExcel(df_mat_net_list, sheet_name='mat_net_frequencies', grouped=FALSE, tab_colour='darkgreen', order='structure', publication_ready=TRUE)
exportToExcel(df_demog_list, sheet_name='demog_frequencies', grouped=FALSE, tab_colour='pink', order='structure', publication_ready=TRUE)
saveWorkbook(wb, paste0("processed_data/lc_survey_frequencies_accessible__",DATE,".xlsx"), overwrite = TRUE)

# Grouped
#----------------------

# Detailed 
wb <- createWorkbook()
exportToExcel(df_sc_group_values_list, sheet_name='sc_values_frequencies', grouped=TRUE, tab_colour='lightblue', order='structure', publication_ready=FALSE)
exportToExcel(df_sc_group_net_list, sheet_name='sc_net_frequencies', grouped=TRUE, tab_colour='darkblue', order='structure', publication_ready=FALSE)
exportToExcel(df_mc_group_list, sheet_name='mc_frequencies', grouped=TRUE, tab_colour='red', order='structure', publication_ready=FALSE)
saveWorkbook(wb, paste0("processed_data/lc_survey_grouped_frequencies_detailed__",DATE,".xlsx"), overwrite = T)

#  Accessible 
wb <- createWorkbook()
exportToExcel(df_sc_group_values_list, sheet_name='sc_values_frequencies', grouped=TRUE, tab_colour='lightblue', order='structure', publication_ready=TRUE)
exportToExcel(df_sc_group_net_list, sheet_name='sc_net_frequencies', grouped=TRUE, tab_colour='darkblue', publication_ready=TRUE)
exportToExcel(df_mc_group_list, sheet_name='mc_frequencies', grouped=TRUE, tab_colour='red', order='structure', publication_ready=TRUE)
saveWorkbook(wb, paste0("processed_data/lc_survey_grouped_frequencies_accessible__",DATE,".xlsx"), overwrite = TRUE)




# # Export - ordered by survey question
#===============================================================================

# Detailed
#----------------
wb <- createWorkbook()
exportToExcel(ls(pattern='_list'), sheet_name='sc_values_frequencies', grouped=FALSE, tab_colour='#0000FF', order='structure', publication_ready=FALSE)



for (q in 1:length(df_export_list)) {
  exportToExcel(df_export_list, sheet_name='sc_values_frequencies', grouped=FALSE, tab_colour='#0000FF', order='structure', publication_ready=FALSE)
  
}

# 
# 
# exportToExcel(df_export_list, sheet_name='sc_values_frequencies', grouped=FALSE, tab_colour='#0000FF', order='structure', publication_ready=FALSE)
# exportToExcel(df_sc_net_list, sheet_name='sc_net_frequencies', grouped=FALSE, tab_colour='darkblue', order='structure', publication_ready=FALSE)
# exportToExcel(df_mc_list, sheet_name='mc_frequencies', grouped=FALSE, tab_colour='red',order='structure', publication_ready=FALSE)
# exportToExcel(df_mat_list, sheet_name='mat_values_frequencies', grouped=FALSE, tab_colour='lightgreen', order='structure', publication_ready=FALSE)
# exportToExcel(df_mat_net_list, sheet_name='mat_net_frequencies', grouped=FALSE, tab_colour='darkgreen', order='structure', publication_ready=FALSE)
# exportToExcel(df_demog_list, sheet_name='demog_frequencies', grouped=FALSE, tab_colour='pink', order='structure', publication_ready=FALSE)
# saveWorkbook(wb, paste0("processed_data/lc_survey_frequencies_detail_",DATE,".xlsx"), overwrite = TRUE)



###############################################################################



df_export_list <- lapply(
  1:length(SURVEY_QUESTIONS), function(q) {
    svyq_short <- names(SURVEY_QUESTIONS[q])
    
    # Find SC frequencies
    #===========================================================================
    if (grepl('__sc', svyq_short)==T) {
      
      # Basic SC
      tempdf_sc <- bind_rows(
        lapply(
          df_sc_list, function(i) {
            #browser()
            if (grepl(svyq_short, unique(i$survey_question_short))==T) {
              return(i)
            }
          }
        )
      )
      
      # Net SC
      tempdf_sc_net <- bind_rows(
        lapply(
          df_sc_net_list, function(i) {
            #browser()
            if (grepl(svyq_short, unique(i$survey_question_short))==T) {
              return(i)
            }
          }
        )
      )
      
      # Grouped non-netted SC
      templist_sc_group_values <- lapply(
        df_sc_group_values_list, function(i) {
          #browser()
          tempdf <- bind_rows(
            lapply(
              i, function(j) {
                #browser()
                if (grepl(svyq_short, unique(j$survey_question_short))==T) {
                  return(j)
                }
              }
            )
          )
        }
      )
      templist_sc_group_values <- templist_sc_group_values[lapply(templist_sc_group_values, length)>0]

      # Grouped netted SC
      templist_sc_group_net <- lapply(
        df_sc_group_values_list, function(i) {
          tempdf <- bind_rows(
            lapply(
              i, function(j) {
                if (grepl(svyq_short, unique(j$survey_question_short))==T) {
                  return(j)
                }
              }
            )
          )
        }
      )
      templist_sc_group_net <- templist_sc_group_net[lapply(templist_sc_group_net, length)>0]
      
      
      return(
        list(
          tempdf_sc, 
          tempdf_sc_net, 
          lapply(templist_sc_group_values, "[", -1), 
          lapply(templist_sc_group_net, "[", -1)
        )
      )
    }
    
    # Find MC frequencies
    #===========================================================================
    if (grepl('__mc', svyq_short)==T) {
      # Basic MC
      tempdf_mc <- bind_rows(
        lapply(
          df_mc_list, function(i) {
            #browser()
            if (grepl(svyq_short, unique(i$survey_question_short))==T) {
              return(i)
            }
          }
        )
      )
      # Grouped MC
      templist_mc_group <- lapply(
        df_mc_group_list, function(i) {
          tempdf <- bind_rows(
            lapply(
              i, function(j) {
                if (grepl(svyq_short, unique(j$survey_question_short))==T) {
                  return(j)
                }
              }
            )
          )
        }
      )
      templist_mc_group <- templist_mc_group[lapply(templist_mc_group, length)>0]
      
      # Return
      return(
        list(
          tempdf_mc, 
          lapply(templist_mc_group, "[", -1)
        )
      )
    } 
    
    # Find MAT frequencies
    #===========================================================================
    if (grepl('__mat', svyq_short)==T) {
      # Basic MAT
      tempdf_mat <- bind_rows(
        lapply(
          df_mat_list, function(i) {
            #browser()
            if (grepl(svyq_short, unique(i$survey_question_short))==T) {
              return(i)
            }
          }
        )
      )
      
      # Net MAT
      tempdf_mat_net <- bind_rows(
        lapply(
          df_mat_net_list, function(i) {
            #browser()
            if (grepl(svyq_short, unique(i$survey_question_short))==T) {
              return(i)
            }
          }
        )
      )
      
      # Return
      return(
        list(
          tempdf_mat, 
          tempdf_mat_net
        )
      )
      
    }
    
  }
)
      
# Integrate above with Excel writing functionality, and then finally into its own function
# Add group!=q condition to mc
# Some issue with ordering


# Show detailed tables
# Potentially add routed n column (ask Roz to speak with Yvette about this)
# matrix order now correct, explain why, this was me correcting both mc and asusming mat would be same, so happens to everyone see!
# Satisfied issue, go through that, good exmaple of how getting these wrong results in wrong outputs, as with the above on my part
# every 1-3 months for keep for finding a home - count is zero, can try to add that in
# Then also retsurcture by question, rathe than question type - explain importanc eof latter for QA, but former to get sense of data


