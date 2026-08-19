
# Single Code
#===============================================================================
singleCode_processing <- 
  function(df, value_levs=NULL, net_levs=NULL, group=NULL, net_levs_group=NULL, group_levs=NULL) {
    
    if (!is.null(net_levs)) {
      if (is.null(group)) {
        df <- df[names(net_levs)]
      }
      else{
        df <- df[c(names(net_levs), group)]
      }
      for (x in 1:length(names(net_levs))) {
        if (length(net_levs[[x]])==2) {
          df[names(net_levs[x])] <- 
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
            NA_character_
             ))
        }
        
        if (length(net_levs[[x]])==3) {
          df[names(net_levs[x])] <- 
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
            NA_character_
            )))
        }
        if (length(net_levs[[x]])==4) {
          df[names(net_levs[x])] <- 
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[4]], names(net_levs[[x]][4]),
            NA_character_
            ))))
        }
        if (length(net_levs[[x]])==5) {
          df[names(net_levs[x])] <- 
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[4]], names(net_levs[[x]][4]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[5]], names(net_levs[[x]][5]),       
            NA_character_
            )))))
        }
        if (length(net_levs[[x]])==6) {
          df[names(net_levs[x])] <- 
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[4]], names(net_levs[[x]][4]),
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[5]], names(net_levs[[x]][5]),   
            ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[6]], names(net_levs[[x]][6]),     
            NA_character_
            ))))))
        }
      }
    }
    
    if (!is.null(net_levs_group)) {
      if (group %in% names(net_levs_group)) {
        if (length(net_levs_group[[group]])==2) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            NA_character_
          ))
        }
        if (length(net_levs_group[[group]])==3) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            NA_character_
          )))
        }
        if (length(net_levs_group[[group]])==4) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[4]], names(net_levs_group[[group]][4]),
            NA_character_
          ))))
        }
        if (length(net_levs_group[[group]])==5) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[4]], names(net_levs_group[[group]][4]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[5]], names(net_levs_group[[group]][5]),
            NA_character_
          )))))
        }
        if (length(net_levs_group[[group]])==6) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[4]], names(net_levs_group[[group]][4]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[5]], names(net_levs_group[[group]][5]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[6]], names(net_levs_group[[group]][6]),
            NA_character_
          ))))))
        }
      }
    }
      
    
    if (is.null(group)) {
      df_summary_list <- lapply(
        names(df), function(q) {
          q_long <-  SURVEY_QUESTIONS[q]
          tempdf <- df %>%
            dplyr::select(
              "response"=all_of(q)
            ) %>%
            mutate(response = as.character(response)) %>%
            dplyr::group_by(response) %>%
            dplyr::summarise(response_count=n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(response_N = sum(response_count, na.rm=T)) %>%
            dplyr::mutate(response_perc = response_count/response_N) %>%
            dplyr::mutate(survey_question_short ={{q}}) %>%
            dplyr::mutate(survey_question_long = {{q_long}}) %>%
            arrange(-response_count)
          
          # Rebased summary for routed questions (which are distinguishable by presence of NA values)
          if (NA %in% unique(tempdf$response)==T) {
            tempdf_rtd <- df %>%
              dplyr::select(
                "response"=all_of(q)
              ) %>%
              mutate(response = as.character(response)) %>%
              filter(!is.na(response)) %>%
              dplyr::group_by(response) %>%
              dplyr::summarise(rtd_response_count=as.double(n())) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(rtd_response_N = sum(rtd_response_count, na.rm=T)) %>%
              dplyr::mutate(rtd_response_perc = rtd_response_count/rtd_response_N) %>%
              arrange(-rtd_response_count)
            
            tempdf <- left_join(tempdf, tempdf_rtd) %>%
              relocate(all_of(contains('survey_question')), .after = last_col()) %>%
              mutate(response = case_when(is.na(response)~'Not answered', .default=response))
          }
          
          # Order by question (response or net) levels via conversion to factor
          if (!is.null(value_levs)) {
            levs <- value_levs[[parse_number(q)]]
            if ('Not answered' %in% unique(tempdf$response)==T) {
              levs <- c(levs, 'Not answered')
            }
            tempdf <- tempdf %>% 
              mutate(response = factor(response, levels=levs, ordered=T)) %>%
              arrange(response)
          }
          if (!is.null(net_levs)) {
            levs <- names(net_levs[[q]])
            if ('Not answered' %in% unique(tempdf$response)==T) {
              levs <- c(levs, 'Not answered')
            }
            tempdf <- tempdf %>% 
              mutate(response = factor(response, levels=levs, ordered=T)) %>%
              arrange(response)
          }
          
          return(tempdf)
        }
      )
      
    }
    
    else {
      #browser()
      df_summary_list <- lapply(
        names(df)[names(df)!=group], function(q) {
          q_long <-  SURVEY_QUESTIONS[q]
          tempdf <- df %>%
            dplyr::select(
              "response"=all_of(q),
              "group"=all_of(group),
            ) %>%
            mutate(response = as.character(response)) %>%
            mutate(group = as.character(group)) %>%
            dplyr::group_by(response, group) %>%
            dplyr::summarise(response_count=n()) %>%
            # sample N as denominator
            dplyr::ungroup() %>%
            dplyr::mutate(response_N = sum(response_count, na.rm=T)) %>%
            dplyr::mutate(response_perc = response_count/response_N) %>%
            # group N as denominator
            group_by(group) %>%
            dplyr::mutate(grouped_response_N = sum(response_count, na.rm=T)) %>%
            dplyr::mutate(grouped_response_perc = response_count/grouped_response_N) %>%
            dplyr::mutate(survey_question_short ={{q}}) %>%
            dplyr::mutate(survey_question_long = {{q_long}}) %>%
            mutate(survey_question_group ={{group}}) 
          
          # Rebased summary for routed questions (which are distinguishable by presence of NA values)
          if (NA %in% unique(tempdf$response)==T) {
            tempdf_rtd <- df %>%
              dplyr::select(
                "response"=all_of(q),
                "group"=all_of(group)
              ) %>%
              mutate(response = as.character(response)) %>%
              mutate(group = as.character(group)) %>%
              filter(!is.na(response)) %>%
              dplyr::group_by(response, group) %>%
              dplyr::summarise(rtd_response_count=as.double(n())) %>%
              # sample N as denominator
              dplyr::ungroup() %>%
              dplyr::mutate(rtd_response_N = sum(rtd_response_count, na.rm=T)) %>%
              dplyr::mutate(rtd_response_perc = rtd_response_count/rtd_response_N) %>%
              # group N as denominator
              group_by(group) %>%
              dplyr::mutate(rtd_grouped_response_N = sum(rtd_response_count, na.rm=T)) %>%
              dplyr::mutate(rtd_grouped_response_perc = rtd_response_count/rtd_grouped_response_N) 
            
            
            tempdf <- left_join(tempdf, tempdf_rtd) %>%
              relocate(all_of(contains('survey_question')), .after = last_col()) %>%
              mutate(response = case_when(is.na(response)~'Not answered', .default=response)) %>%
              arrange(group)
          } 
          # Order by question (response or net) levels via conversion to factor
          if (!is.null(value_levs)) {
            levs <- value_levs[[parse_number(q)]]
            if ('Not answered' %in% unique(tempdf$response)==T) {
              levs <- c(levs, 'Not answered')
            }
            tempdf <- tempdf %>% 
              mutate(response = factor(response, levels=levs, ordered=T)) %>%
              arrange(group, response)
          }
          if (!is.null(net_levs)) {
            levs <- names(net_levs[[q]])
            if ('Not answered' %in% unique(tempdf$response)==T) {
              levs <- c(levs, 'Not answered')
            }
            tempdf <- tempdf %>% 
              mutate(response = factor(response, levels=levs, ordered=T)) %>%
              arrange
          }
          if (!is.null(group_levs)) {
            levs <- group_levs[[group]]
            if (NA_character_ %in% unique(tempdf$group)) {
              tempdf$group <- ifelse(is.na(tempdf$group), 'NA', tempdf$group)
              levs <- c(levs, 'NA')
            }
            tempdf <- tempdf %>% 
              mutate(group = factor(group, levels=levs, ordered=T)) %>%
              arrange(group, response)
          }
          
          return(tempdf)
        }
      )
    }
    
    return(df_summary_list)
    
  }


# Multichoice
#===============================================================================
multiChoice_processing <- 
  function(df, group=NULL, net_levs_group=NULL, group_levs=NULL) {

    if (!is.null(net_levs_group)) {
      if (group %in% names(net_levs_group)) {
        if (length(net_levs_group[[group]])==2) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            NA_character_
          ))
        }
        if (length(net_levs_group[[group]])==3) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
           NA_character_
         )))
        }
        if (length(net_levs_group[[group]])==4) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[4]], names(net_levs_group[[group]][4]),
            NA_character_
          ))))
        }
        if (length(net_levs_group[[group]])==5) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[4]], names(net_levs_group[[group]][4]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[5]], names(net_levs_group[[group]][5]),
            NA_character_
          )))))
        }
        if (length(net_levs_group[[group]])==6) {
          df[group] <- 
            ifelse(df[[group]] %in% net_levs_group[[group]][[1]], names(net_levs_group[[group]][1]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[2]], names(net_levs_group[[group]][2]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[3]], names(net_levs_group[[group]][3]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[4]], names(net_levs_group[[group]][4]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[5]], names(net_levs_group[[group]][5]),
            ifelse(df[[group]] %in% net_levs_group[[group]][[6]], names(net_levs_group[[group]][6]),
            NA_character_
          ))))))
        }
      }
    }
    
    
    if (is.null(group)) {
      
      df_summary_list <- lapply(
        unique(sub(".*:", "", names(df))), function(q) {
          q_long <-  SURVEY_QUESTIONS[grepl(q, names(SURVEY_QUESTIONS))]
          tempdf <- df %>%
            select(ends_with(sub(".*__", "", {{q}}))) %>%
            mutate(not_routed = case_when(rowSums(is.na(.))-ncol(.)==0~'Not answered', .default=NA_character_)) %>%
            pivot_longer(cols=everything(), names_to='response', values_to='value') %>%
            mutate(response = as.character(response)) %>%
            group_by(response, value) %>%
            summarise(response_count=n()) %>%
            ungroup() %>%
            group_by(response) %>%
            mutate(response_N=sum(response_count)) %>%
            ungroup() %>%
            mutate(value=case_when(response_count==response_N~sub(":.*", "", response)  , .default=value)) %>% # Not sure if this does anything at all (possibly redundant line - maybe it captures rows where non answers are qual to total sample, not quite sure lol)
            mutate(response_count=case_when(response_count==response_N~0, .default=response_count)) %>%
            drop_na() %>%
            mutate(response_perc = response_count/response_N) %>%
            select(-response, response=value) %>%
            mutate(response = case_when(response=='not_routed'~'Not answered', .default=response)) # Recode not_routed (n=0) to 'Not answered
            # Routed stats (where applicable)
          if (as.numeric(tempdf[tempdf$response=='Not answered', 'response_count'])>0) {
            tempdf <- tempdf %>%
              mutate(rtd_response_count = response_count) %>%
              mutate(rtd_response_count = case_when(response=='Not answered'~NA_real_, .default=response_count)) %>%
              mutate(rtd_response_N = case_when(response=='Not answered'~response_N-response_count, .default=NA_real_)) %>%
              fill(rtd_response_N, .direction='up') %>%
              mutate(rtd_response_N = case_when(response=='Not answered'~NA_real_, .default=rtd_response_N)) %>%
              mutate(rtd_response_perc = case_when(response!='Not answered'~rtd_response_count/rtd_response_N)) 
          }
          response_levs <- tempdf$response[rev(order(tempdf$response_count))]
          if ('rtd_response_count' %in% names(tempdf)) {
            response_levs <- c(response_levs[!grepl('Not answered', response_levs)], 'Not answered')
          }
          else {
            response_levs <- response_levs[!grepl('Not answered', response_levs)]
            tempdf <- tempdf[tempdf$response!='Not answered',]
          }
          tempdf <- tempdf %>%
           mutate(survey_question_short = {{q}}) %>% # %>% TODO remove mc8-based if condition when mc8 variables are added to lookup, connecting this line to the next mutate(survey_question_)
           mutate(survey_question_long = {{q_long}}) %>%
           mutate(response = factor(response, levels=response_levs)) %>%
           arrange(response)
          
          return(tempdf)
        }
      )
    }
    
    else {
      #browser()
      df_summary_list <- lapply(
        unique(sub(".*:", "", names(df)))[!grepl(group, unique(sub(".*:", "", names(df))))], function(q) {
          #browser()
          q_long <-  SURVEY_QUESTIONS[grepl(q, names(SURVEY_QUESTIONS))]
          tempdf <- df %>%
            select(
              ends_with(sub(".*__", "", {{q}})),
              "group"=all_of(group)
              )  %>%
            mutate(not_routed = case_when(rowSums(is.na(.[1:ncol(.)-1]))-(ncol(.)-1)==0~'Not answered', .default=NA_character_)) %>% # Identify not-routed rows on the non-group column (ie all columns but last)
            pivot_longer(cols=!group, names_to='response', values_to='value') %>%
            mutate(response = as.character(response)) %>%
            group_by(response, value, group) %>%
            dplyr::summarise(response_count=n()) %>%
            group_by(response) %>%
            mutate(response_N=sum(response_count)) %>%
            ungroup() %>%
            mutate(value=case_when(response_count==response_N~sub(":.*", "", response)  , .default=value)) %>% # Not sure if this does anything at all (possibly redundant line - maybe it captures rows where non answers are qual to total sample, not quite sure lol)
            mutate(response_count=case_when(response_count==response_N~0, .default=response_count)) %>%
            filter(!is.na(value)) %>%
            # sample N as denominator
            mutate(response_perc = response_count/response_N) %>%
            select(-response, response=value) %>%
            mutate(response = case_when(response=='not_routed'~'Not answered', .default=response)) # Recode not_routed (n=0) to 'Not answered
          # Create separate dataframe for grouped N's (CANNOT CALC FROM resonse_count DUE TO DOUBLE COUNTING AS A RESULT OF MULTICHOICE STRUCTURE)
          tempdf_groupn <- df %>%
            group_by(.data[[group]]) %>%
            summarise(grouped_response_N = n()) %>%
            ungroup() %>%
            rename('group' = .data[[group]])
          tempdf <- tempdf %>%
            left_join(tempdf_groupn, by='group') %>%
            # group N as denominator
            dplyr::mutate(grouped_response_perc = response_count/grouped_response_N) %>%
            dplyr::mutate(survey_question_short ={{q}}) %>%
            dplyr::mutate(survey_question_long = {{q_long}}) %>%
            mutate(survey_question_group ={{group}}) 

          # Rebased summary for routed questions (which are distinguishable by presence of 'Not answered' values)
          if (sum(as.vector(unlist(tempdf[tempdf$response=='Not answered', 'response_count'])))>0) {
            tempdf_rtd <- tempdf %>%
              mutate(rtd_response_count = response_count) %>%
              mutate(rtd_response_count = case_when(response=='Not answered'~NA_real_, .default=response_count)) %>%
              group_by(response) %>%
              mutate(not_routed_n = case_when(response=='Not answered'~sum(response_count), .default=NA_real_)) %>%
              ungroup() %>%
              mutate(rtd_response_N = case_when(response=='Not answered'~response_N-not_routed_n, .default=NA_real_)) %>%
              fill(rtd_response_N, .direction='up') %>%
              mutate(rtd_response_N = case_when(response=='Not answered'~NA_real_, .default=rtd_response_N)) %>%
              mutate(rtd_response_perc = case_when(response!='Not answered'~rtd_response_count/rtd_response_N)) %>%
              mutate(rtd_grouped_response_N = case_when(response=='Not answered'~grouped_response_N-response_count, .default=NA_real_)) %>%
              group_by(group) %>%
              fill(rtd_grouped_response_N, .direction='up') %>%
              mutate(rtd_grouped_response_N = case_when(response=='Not answered'~NA_real_, .default=rtd_grouped_response_N)) %>%
              mutate(rtd_grouped_response_perc = case_when(response!='Not answered'~rtd_response_count/rtd_grouped_response_N))
     
            tempdf <- left_join(tempdf, tempdf_rtd) %>%
              relocate(all_of(contains('survey_question')), .after = last_col()) %>%
              select(-not_routed_n)  
          }
          
          if (!is.null(group_levs)) {
            levs <- group_levs[[group]]
            if (NA_character_ %in% unique(tempdf$group)) {
              tempdf$group <- ifelse(is.na(tempdf$group), 'NA', tempdf$group)
              levs <- c(levs, 'NA')
            }
            tempdf <- tempdf %>% 
              mutate(group = factor(group, levels=levs, ordered=T)) 
          }
          tempdf <- tempdf %>%
            arrange(group, response)
          return(tempdf)
        }
      )
    }
    
    return(df_summary_list)
    
  }


# Matrix
#===============================================================================
matrix_processing <- 
  function(df, value_levs=NULL, net_levs=NULL, group=NULL) {
    
    # Hard-coded for now, since only one maatrix net required for AI data wand was working under time contraints!
    if (!is.null(net_levs)) {
      df <- df %>% 
        #select(contains(names(net_levs))) %>%
        mutate(across(everything(),~
          case_when(
            .x %in% c('Very confident', 'Fairly confident')~'Confident',
            .x %in% c('Not very confident', 'Not confident at all')~'Not confident',
            .default='Don\'t know/NA'
           )
        ))
    }

    if (is.null(group)) {
      
      #browser()
      df_summary_list <- lapply(
        unique(sub(".*:", "", names(df))), function(q) {
          
          q_long <-  SURVEY_QUESTIONS[grepl(q, names(SURVEY_QUESTIONS))]
          #browser()
          tempdf <- df %>%
            select(ends_with(sub(".*__", "", {{q}}))) %>%
            pivot_longer(cols=everything(), names_to='response', values_to='value') %>%
            mutate(response = as.character(response)) %>%
            group_by(response, value) %>%
            summarise(response_count=n()) %>%
            ungroup() %>%
            group_by(response) %>%
            mutate(response_N=sum(response_count)) %>%
            ungroup() %>%
            mutate(response_perc = response_count/response_N) %>%
            mutate(value = case_when(is.na(value)~'Not answered', .default=value)) %>%
            mutate(response = sub(":.*", "", response)) %>%
            # Maybe not applicable instead of Not answerwed, not clear rn!
            mutate(rtd_response_count = response_count) %>%
            mutate(rtd_response_count = case_when(value=='Not answered'~NA_real_, .default=response_count)) %>%
            group_by(response) %>%
            mutate(rtd_response_N = case_when(response!='Not answered'~sum(rtd_response_count, na.rm=T), .default=NA_real_)) %>%
            ungroup() %>%
            mutate(rtd_response_perc = case_when(response!='Not answered'~rtd_response_count/rtd_response_N)) %>%
            mutate(survey_question_short = {{q}}) %>%
            mutate(survey_question_long = {{q_long}})
          
          # Order by question (response or net) levels via conversion to factor
          if (!is.null(value_levs)) {
            levs <- value_levs[[parse_number(q)]]
            tempdf <- tempdf %>%
              mutate(value = factor(value, levels=levs, ordered=T)) %>%
              arrange(response, value)
          }
          if (!is.null(net_levs)) {
            levs <- names(net_levs[[parse_number(q)]])
            tempdf <- tempdf %>% 
              mutate(value = factor(value, levels=levs, ordered=T)) %>%
              arrange(response, value)
          }
           
          return(tempdf)
        }
      )
    }
    
    else {
      #browser()
      # df_summary_list <- lapply(
      #   names(df), function(q) {
      #     q_long <-  SURVEY_QUESTIONS[q]
      #     tempdf <- df_sc %>%
      #       dplyr::select(
      #         "response"=all_of(q),
      #         "group"=all_of(group),
      #       ) %>%
      #       mutate(response = as.character(response)) %>%
      #       mutate(group = as.character(group)) %>%
      #       dplyr::group_by(response, group) %>%
      #       dplyr::summarise(response_count=n()) %>%
      #       # sample N as denominator
      #       dplyr::ungroup() %>%
      #       dplyr::mutate(response_N = sum(response_count, na.rm=T)) %>%
      #       dplyr::mutate(response_perc = (response_count/response_N*100)) %>%
      #       # group N as denominator
      #       group_by(group) %>%
      #       dplyr::mutate(filtered_response_N = sum(response_count, na.rm=T)) %>%
      #       dplyr::mutate(filtered_response_perc = (response_count/response_N*100)) %>%
      #       dplyr::mutate(survey_question_short ={{q}}) %>%
      #       dplyr::mutate(survey_question_long = {{q_long}})
      #     
      #     # Rebased summary for routed questions (which are distinguishable by presence of NA values)
      #     if (NA %in% unique(tempdf$response)==T) {
      #       tempdf_rtd <- df_sc %>%
      #         dplyr::select(
      #           "response"=all_of(q),
      #           "group"=all_of(group)
      #         ) %>%
      #         mutate(response = as.character(response)) %>%
      #         mutate(group = as.character(group)) %>%
      #         filter(!is.na(response)) %>%
      #         dplyr::group_by(response, group) %>%
      #         dplyr::summarise(rtd_response_count=n()) %>%
      #         # sample N as denominator
      #         dplyr::ungroup() %>%
      #         dplyr::mutate(rtd_response_N = sum(rtd_response_count, na.rm=T)) %>%
      #         dplyr::mutate(rtd_response_perc = (rtd_response_count/rtd_response_N*100)) %>%
      #         # group N as denominator
      #         group_by(group) %>%
      #         dplyr::mutate(rtd_filtered_response_N = sum(rtd_response_count, na.rm=T)) %>%
      #         dplyr::mutate(rtd_filtered_response_perc = (rtd_response_count/rtd_filtered_response_N*100)) 
      #       
      #       
      #       tempdf <- left_join(tempdf, tempdf_rtd) %>%
      #         relocate(all_of(contains('survey_question')), .after = last_col()) %>%
      #         mutate(response = case_when(is.na(response)~'Not answered', .default=response))
      #     } 
      #     return(tempdf)
      #   }
      # )
    }
    
    return(df_summary_list)
    
  }


# Demographic frequencies
#===============================================================================
demographics_processing <- function(df, group_vars=NULL, net_levs=NULL) {

  if (!is.null(net_levs)) {
    for (x in 1:length(names(net_levs))) {
      
      if (length(net_levs[[x]])==2) {
        df[names(net_levs[x])] <- 
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
          NA_character_
         ))
      }
      if (length(net_levs[[x]])==3) {
        df[names(net_levs[x])] <- 
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
          NA_character_
        )))
      }
      if (length(net_levs[[x]])==4) {
        df[names(net_levs[x])] <- 
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[4]], names(net_levs[[x]][4]),
          NA_character_
        ))))
      }
      if (length(net_levs[[x]])==5) {
        df[names(net_levs[x])] <- 
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[4]], names(net_levs[[x]][4]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[5]], names(net_levs[[x]][5]),
          NA_character_
        )))))
      }
      if (length(net_levs[[x]])==6) {
        df[names(net_levs[x])] <- 
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[1]], names(net_levs[[x]][1]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[2]], names(net_levs[[x]][2]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[3]], names(net_levs[[x]][3]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[4]], names(net_levs[[x]][4]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[5]], names(net_levs[[x]][5]),
          ifelse(df[[names(net_levs[x])]] %in% net_levs[[x]][[6]], names(net_levs[[x]][6]),
          NA_character_
        ))))))
      }
      
    }
  }
  #browser()    
  tempdf <- df %>%
    select(!contains('__'), -c(time_started:uid)) %>%
    select(any_of(group_vars))
  tempdf_list <- lapply(
    names(tempdf), function(x) {
      tempdf %>%
        select(.data[[x]]) %>%
        rename('response'= .data[[x]]) %>%
        group_by(response) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        mutate(N = sum(n)) %>%
        mutate(prop = n/N) %>%
        select(-N) %>%
        mutate(response = case_when(is.na(response)~'Not answered', .default=as.character(response))) %>%
        mutate(demog = x) 
    }
  )
  return(tempdf_list)
}


# Export to Excel
#===============================================================================

exportToExcel <- 
  function(df_list, sheet_name, grouped=FALSE, tab_colour=NULL, publication_ready=NULL) {
    
    # Add worksheet
    addWorksheet(wb, sheetName=sheet_name, tabColour = tab_colour)
    `%ni%` <- Negate(`%in%`)
    

    # Non-grouped output
    if (grouped==F) {
      row_num <- 1-nrow(df_list[[1]])-3
      for (q in 1:length(df_list)) {
        if (q==1) {
          row_num <- row_num + nrow(df_list[[q]]) + 4
          #browser()
        }
        else {
          qm1 <- q-1
          row_num <- row_num + nrow(df_list[[qm1]]) + 4
        }
        if (publication_ready==F) {
          writeData(wb, sheet_name, df_list[[q]], startRow=row_num)
        }
      
        else {
          #browser()
          if ('rtd_response_perc' %in% names(df_list[[q]]) & 'value' %ni% names(df_list[[q]])) {
            tempdf <- df_list[[q]] %>%
              select(
                'Response' = 'response',
                'Base size (n)' = 'response_count',
                'Percent\n(of routed sample)' = 'rtd_response_perc',
                'Label' = 'survey_question_short'
              ) 
          }
          else if ('rtd_response_perc' %in% names(df_list[[q]]) & 'value' %in% names(df_list[[q]])) {
            tempdf <- df_list[[q]] %>%
              select(
                'Response' = 'response',
                'Value' = 'value',
                'Base size (n)' = 'response_count',
                'Percent\n(of routed sample)' = 'rtd_response_perc',
                'Label' = 'survey_question_short'
              ) 
          }
          else if ('rtd_response_perc' %ni% names(df_list[[q]]) & 'value' %in% names(df_list[[q]])) {
            tempdf <- df_list[[q]] %>%
              select(
                'Response' = 'response',
                'Value' = 'value',
                'Base size (n)' = 'response_count',
                'Percent' = 'response_perc',
                'Label' = 'survey_question_short'
              ) 
          }
          else if ('prop' %in% names(df_list[[q]])) {
            tempdf <- df_list[[q]] %>%
              select(
                'Response' = 'response',
                'Base size (n)' = 'n',
                'Percent' = 'prop',
                'Label' = 'demog'
              ) %>%
              mutate(`Label` = gsub("_", " ",str_to_title(Label)))
          }
          else {
            tempdf <- df_list[[q]] %>%
              select(
                'Response' = 'response',
                'Base size (n)' = 'response_count',
                'Percent' = 'response_perc',
                'Label' = 'survey_question_short'
              ) 
          }
          writeData(wb, sheet_name, tempdf, startRow=row_num)
          showGridLines(wb, sheet_name, showGridLines = FALSE)
          if ('Value' %in% names(tempdf)) {
            setColWidths(wb, sheet_name, cols = c(1:5), widths = c(30, 25, 20, 20, 26))
          }
          else {
            setColWidths(wb, sheet_name, cols = c(1:4), widths = c(30, 20, 20, 25))
          }
          headerStyle <- createStyle(
            fontColour = "#ffffff", fontSize='12', halign = "center", valign = "center",
            fgFill = "#007acc", border = "TopBottom", borderColour = "#007acc",  wrapText = TRUE,
            textDecoration = "Bold"
          )
          addStyle(wb, sheet = sheet_name, headerStyle, rows = row_num, cols = 1:ncol(tempdf), gridExpand = TRUE)
          bodyStyle <- createStyle(halign = "center", valign = "center", wrapText = TRUE)
          bodyPercStyle <- createStyle(numFmt="0%", halign = "center", valign = "center", wrapText = TRUE)
          bodyStart <- row_num+1
          bodyEnd <- row_num + nrow(tempdf)
          if ('Value' %in% names(tempdf)) {
            addStyle(wb, sheet = sheet_name, bodyStyle, rows = bodyStart:bodyEnd, cols = c(1:3,5), gridExpand = TRUE)
            addStyle(wb, sheet = sheet_name, bodyPercStyle, rows = bodyStart:bodyEnd, cols = 4, gridExpand = TRUE)
          }
          else {
            addStyle(wb, sheet = sheet_name, bodyStyle, rows = bodyStart:bodyEnd, cols = c(1:2,4), gridExpand = TRUE)
            addStyle(wb, sheet = sheet_name, bodyPercStyle, rows = bodyStart:bodyEnd, cols = 3, gridExpand = TRUE)
          }
          maxResponseLength <- max(nchar(as.character(tempdf$Response)))
          if (maxResponseLength <= 30) {
            height <- 20
          }
          else if (between(maxResponseLength, 30, 50 )) {
            height <- 30
          }
          else if (between(maxResponseLength, 50, 70)) {
            height <- 35
          }
          else {
            height <- 45
          }
          setRowHeights(wb, sheet_name, rows = row_num:bodyEnd, heights = c(35, rep(height, length(bodyStart:bodyEnd))))
        }
      }
    }
      
    # Grouped output
    else {
      group_row_num <- 1
      max_length_group <- 0
      for (d in 1:length(df_list)) {
        if (length(unique(df_list[[d]][[1]]$group))>max_length_group) {
          max_length_group <- length(unique(df_list[[d]][[1]]$group))
        }
        for (q in 1:length(df_list[[d]])) {
          
          if (publication_ready==F) {
            if (d==1&q==1) {
              group_row_num <- group_row_num
            }
            else if (q==1) {
              dm1 <- d-1
              qm1 <- length(df_list[[dm1]])
              group_row_num <- group_row_num + nrow(df_list[[dm1]][[qm1]]) + 3
            }
            else {
              browser()
              qm1 <- q-1
              group_row_num <- group_row_num + nrow(df_list[[d]][[qm1]]) + 3
            }
            writeData(wb, sheet_name, df_list[[d]][[q]], startRow=group_row_num)
          }
          
          else {
            if ('rtd_response_perc' %in% names(df_list[[d]][[q]]) & 'value' %ni% names(df_list[[d]][[q]])) {
              tempdf_perc <- df_list[[d]][[q]] %>%
                select(
                  'Response' = 'response',
                  'Group' = 'group',
                  'Percent' = 'rtd_grouped_response_perc',
                  'Question label' = 'survey_question_short',
                  'Group label' = 'survey_question_group'
                ) %>%
                pivot_wider(
                  id_cols=c('Response', 'Question label', 'Group label'),
                  names_from='Group',
                  values_from='Percent'
                ) %>%
                relocate(c('Question label', 'Group label'), .after = last_col()) %>%
                mutate(across(-c('Response', 'Question label', 'Group label'),~case_when(is.na(.x)~0, .default=.x)))
              tempdf_base <- df_list[[d]][[q]] %>%
                pivot_wider(
                  id_cols='response',
                  names_from='group',
                  values_from='rtd_grouped_response_N'
                ) %>%
                summarise(across(!response,~mean(.x, na.rm=T))) %>%
                mutate(
                  'Response'='Base size (n) - routed',
                  'Question label'='',
                  'Group label'=''
                ) %>%
                relocate(Response)
              tempdf <- bind_rows(tempdf_perc, tempdf_base) %>%
                filter(Response!='Not answered')
            }
            # else if ('rtd_response_perc' %in% names(df_list[[q]]) & 'value' %in% names(df_list[[q]])) {
            #   tempdf <- df_list[[q]] %>%
            #     select(
            #       'Response' = 'response',
            #       'Value' = 'value',
            #       'Base size' = 'response_count',
            #       'Percent\n(of routed sample)' = 'rtd_response_perc',
            #       'Label' = 'survey_question_short'
            #     ) 
            # }
            # else if ('rtd_response_perc' %ni% names(df_list[[q]]) & 'value' %in% names(df_list[[q]])) {
            #   tempdf <- df_list[[q]] %>%
            #     select(
            #       'Response' = 'response',
            #       'Value' = 'value',
            #       'Base size' = 'response_count',
            #       'Percent' = 'response_perc',
            #       'Label' = 'survey_question_short'
            #     ) 
            # }
            else {
              tempdf_perc <- df_list[[d]][[q]] %>%
                select(
                  'Response' = 'response',
                  'Group' = 'group',
                  'Percent' = 'grouped_response_perc',
                  'Question label' = 'survey_question_short',
                  'Group label' = 'survey_question_group'
                ) %>%
                pivot_wider(
                  id_cols=c('Response', 'Question label', 'Group label'),
                  names_from='Group',
                  values_from='Percent'
                ) %>%
                relocate(c('Question label', 'Group label'), .after = last_col()) %>%
                mutate(across(-c('Response', 'Question label', 'Group label'),~case_when(is.na(.x)~0, .default=.x)))
              tempdf_base <- df_list[[d]][[q]] %>%
                pivot_wider(
                  id_cols='response',
                  names_from='group',
                  values_from='grouped_response_N'
                ) %>%
                summarise(across(!response,~mean(.x, na.rm=T))) %>%
                mutate(
                  'Response'='Base size (n)',
                  'Question label'='',
                  'Group label'=''
                ) %>%
                relocate(Response)
              tempdf <- bind_rows(tempdf_perc, tempdf_base) 
            }
            #browser()
            if (d==1&q==1) {
              group_row_num <- group_row_num
            }
            else if (q==1) {
              dm1 <- d-1
              qm1 <- length(df_list[[dm1]])
              group_row_num <- group_row_num + length(unique(df_list[[dm1]][[qm1]]$response)) + 5
            }
            else {
              qm1 <- q-1
              group_row_num <- group_row_num + length(unique(df_list[[d]][[qm1]]$response)) + 5
            }
            writeData(wb, sheet_name, tempdf, startRow=group_row_num)
            showGridLines(wb, sheet_name, showGridLines = FALSE)
            #browser()
            #("openxlsx.maxWidth" = 20)
            max_length_group_p2 <- max_length_group + 2 
            max_length_group_p3 <- max_length_group + 3 
            setColWidths(wb, sheet_name, cols = c(1:max_length_group_p3), widths = c(30, rep(20, max_length_group_p2 )))
            #browser()
            headerStyle <- createStyle(
              fontColour = "#ffffff", fontSize='12', halign = "center", valign = "center",
              fgFill = "#007acc", border = "TopBottom", borderColour = "#007acc",  wrapText = TRUE,
              textDecoration = "Bold"
            )
            addStyle(wb, sheet = sheet_name, headerStyle, rows = group_row_num, cols = 1:ncol(tempdf), gridExpand = TRUE)
            bodyStyle <- createStyle(halign = "center", valign = "center", wrapText = TRUE)
            bodyPercStyle <- createStyle(numFmt="0%", halign = "center", valign = "center", wrapText = TRUE)
            bodyStart <- group_row_num+1
            bodyEnd <- group_row_num + nrow(tempdf) -1
            baseStart <- bodyEnd+1
            bodyCols <- which(colnames(tempdf) %in% c('Response', 'Question label', 'Group label'))
            percCols <- which(colnames(tempdf) %ni% c('Response', 'Question label', 'Group label'))
            #browser()
            addStyle(wb, sheet = sheet_name, bodyStyle, rows = bodyStart:bodyEnd, cols = bodyCols, gridExpand = TRUE)
            addStyle(wb, sheet = sheet_name, bodyPercStyle, rows = bodyStart:bodyEnd, cols = percCols, gridExpand = TRUE)
            baseStyle <- createStyle(fgFill = "#F2F2F2",  textDecoration = "Bold",halign = "center", valign = "center")
            addStyle(wb, sheet = sheet_name, baseStyle, rows = baseStart, cols = 1:ncol(tempdf), gridExpand = TRUE)
            maxResponseLength <- max(nchar(as.character(tempdf$Response)))
            if (maxResponseLength <= 30) {
              height <- 20
            }
            else if (between(maxResponseLength, 30, 50 )) {
              height <- 30
            }
            else if (between(maxResponseLength, 50, 70)) {
              height <- 35
            }
            else {
              height <- 45
            }
            setRowHeights(wb, sheet_name, rows = group_row_num:baseStart, heights = c(35, rep(height, length(bodyStart:bodyEnd))))

            
            
            
            
          }
        }
      }
      
      
    }
  }
    

    

   