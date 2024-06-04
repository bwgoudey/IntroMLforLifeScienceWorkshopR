library(MatchIt)
library(tidyverse)
##https://datadryad.org/stash/dataset/doi:10.5061/dryad.ft8750v
x=readxl::read_xlsx('./dataset/RC Health Care Data-20180820.xlsx')
filtered_dat=x %>% 
  janitor::clean_names() %>% 
  select(-drinking_status_1_current_drinker_2_ever_drinker_3_never_drinker, 
         -smoking_status_1_current_smoker_2_ever_smoker_3_never_smoker, 
         -ast_u_l,
         -family_histroy_of_diabetes_1_yes_0_no,
         -diabetes_diagnosed_during_followup_1_yes)  %>% 
  filter(!is.na(hdl_c_mmol_l))  %>% 
  mutate(diabetes=censor_of_diabetes_at_followup_1_yes_0_no)

# Find matched group
matchit.out=MatchIt::matchit(censor_of_diabetes_at_followup_1_yes_0_no ~ age_y +
                     gender_1_male_2_female + 
                     site, data=filtered_dat,
                   method = "nearest", distance ="glm",ratio = 1,replace = FALSE)

matched_filtered_dat = filtered_dat[c(as.numeric(I$match.matrix),
                            which(filtered_dat$diabetes==1)), ] %>%
  group_by(diabetes) %>% 
  slice_head(n=1000) %>% 
  ungroup() 

dat<-matched_filtered_dat %>% 
  select(-site) 

write.csv(dat, file = './RC_health_data_n2000.csv')