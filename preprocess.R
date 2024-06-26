library(MatchIt)
library(tidyverse)
##https://datadryad.org/stash/dataset/doi:10.5061/dryad.ft8750v
x=readxl::read_xlsx('./dataset/RC Health Care Data-20180820.xlsx')
filtered_dat=x %>% 
  janitor::clean_names() %>% 
  dplyr::select(-drinking_status_1_current_drinker_2_ever_drinker_3_never_drinker, 
         -smoking_status_1_current_smoker_2_ever_smoker_3_never_smoker, 
         -ast_u_l,
         -family_histroy_of_diabetes_1_yes_0_no,
         -diabetes_diagnosed_during_followup_1_yes)  %>% 
  filter(!is.na(hdl_c_mmol_l))  %>% 
  mutate(diabetes=censor_of_diabetes_at_followup_1_yes_0_no) %>% 
  dplyr::select(-fpg_of_final_visit_mmol_l,
                -censor_of_diabetes_at_followup_1_yes_0_no) %>% 
      filter(complete.cases(.))

# Find matched group
matchit.out=MatchIt::matchit(diabetes ~ age_y +
                     gender_1_male_2_female + 
                     site, data=filtered_dat,
                   method = "nearest", distance ="glm",ratio = 2,replace = FALSE)

matched_filtered_dat = filtered_dat[c(as.numeric(matchit.out$match.matrix),
                            which(filtered_dat$diabetes==1)), ] %>%
  filter(site %in% c(6,8,5))
  group_by(diabetes) %>% 
  slice_head(n=2000) %>% 
  ungroup() 

dat<-matched_filtered_dat %>% 
  filter(!is.na(id))

#dat

write.csv(dat, file = './RC_health_data_n3266.csv', row.names = F)
