function(df) {
  
  df %>% 
    group_by(SA1_7DIGITCODE_2016) %>% 
    pivot_longer(cols = -c(SA1_7DIGITCODE_2016,Elect_div,geom,geopackage),
                 names_to = "category",
                 values_to = "count") %>% 
    filter(str_detect(category, 
                      "(Age_)(\\d+)_(\\d+)(_yr_)([MFP])|(Age_85ov_)([MFP])|(Australian_citizen_)[MFP]|(Age_yr_)(\\d+)_(\\d+)_([MFP])|([MF])_Tot_Tot|([MF])(_Australia_)(\\d+)_(\\d+)|([MF])(_Australia_)(\\d+)(ov)|([A-Za-z])+(_)(([A-Za-z]){3,}(_))?(BP|FO|MO|)(_B_)(OS|Aus|NS)|([A-Za-z])+(_Both_parents_born_Aust)|([A-Za-z])+(_Birthplace_not_stated)")) %>%
    unglue_unnest(category, 
                  c("Age_{age_min=\\d+}_{age_max=\\d+}_yr_{sex=[MFP]}",
                    "Age_{age_min=\\d+}{age_max=(\\d+|ov)}_{sex=[MFP]}", 
                    "{citizenship=([A-Za-z]){10}}_citizen_{sex=[MFP]}", 
                    "Age_yr_{age_min=\\d+}_{age_max=\\d+}_{sex=[MFP]}",
                    "{sex=[MF]}_Tot_Tot",
                    "{sex=[MF]}_Australia_{age_min=\\d+}_{age_max=\\d+}",
                    "{sex=[MF]}_Australia_{age_min=\\d+}ov", 
                    "{ancestry1=([A-Za-z])+}_{parent=(BP|FO|MO)}(_B_){birthofparent=(OS|Aus|NS)}", 
                    "{ancestry1=([A-Za-z])+}_{parent=Birthplace}_{birthofparent=not_stated}",
                    "{ancestry1=([A-Za-z])+}_{parent=Both}_parents_born_{birthofparent=not_stated}",
                    #"{ancestry1=([A-Za-z])+}_{ancestry2=([A-Za-z])+}_{parent=(BP|FO|MO)}(_B_){birthofparent=(OS|Aus|NS)}" 
                  ),
                  remove = FALSE) #%>% 
  #mutate(across(starts_with("age"), as.numeric))
}


clean_G01_G04_G09 <- function(df) {
  
  df %>% 
    group_by(SA1_7DIGITCODE_2016) %>% 
    pivot_longer(cols = -c(SA1_7DIGITCODE_2016,Elect_div,geom,geopackage,centroid),
                 names_to = "category",
                 values_to = "count") %>% 
    filter(str_detect(category, 
                      "(Age_)(\\d+)_(\\d+)(_yr_)([MFP])|(Age_85ov_)([MFP])|(Australian_citizen_)[MFP]|(Age_yr_)(\\d+)_(\\d+)_([MFP])|([MF])_Tot_Tot|([MF])(_Australia_)(\\d+)_(\\d+)|([MF])(_Australia_)(\\d+)(ov)")) %>%
    unglue_unnest(category, 
                  c("Age_{age_min=\\d+}_{age_max=\\d+}_yr_{sex=P}",
                    "Age_{age_min=\\d+}{age_max=(\\d+|ov)}_{sex=P}", 
                    "{citizenship=([A-Za-z]){10}}_citizen_{sex=P}", 
                    "Age_yr_{age_min=\\d+}_{age_max=\\d+}_{sex=[MFP]}",
                    "{sex=[MF]}_Tot_Tot",
                    "{sex=[MF]}_Australia_{age_min=\\d+}_{age_max=\\d+}",
                    "{sex=[MF]}_Australia_{age_min=\\d+}ov", 
                  ),
                  remove = FALSE) %>% 
    mutate(across(starts_with("age"), as.numeric))
}
```
