#> Capstone Project
#> Brendan Phillips

library(tidyverse)
library(haven)

# Reading data files
setwd("S:/HMG-Capstone-Phillips/Data")
d_sas <- read_sas("dedup_pt_encounters_eci_analysis.sas7bdat")

setwd("C:/Users/brend/OneDrive/Documents")
write.csv(d_sas, file = "capstone_data.csv", row.names = FALSE)

d_csv <- read.csv("capstone_data.csv")

d_tables <- d_csv %>% 
  mutate(across(c(ed_30days,
                  readmission_30days),
                ~recode(.,"N"="No","Y"="Yes",.default=NULL),
                .names="{.col}_recode"),
         sex_recode = ifelse(sex == "", "Unknown", sex),
         ethnicity_recode = case_when(
           ethnicity == "Hispanic, Latino/a, or Spanish Origin" ~ "Hispanic",
           ethnicity == "Non-Hispanic, Latino/a, or Spanish Origin" ~ "Hispanic",
           ethnicity == "Patient Unable to Answer" ~ "Unknown",
           ethnicity == "*Unspecified" ~ "Unknown",
           ethnicity == "" ~ "Unknown",
           TRUE ~ ethnicity),
         ICU_days_recode = ifelse(ICU_days == "","No", "Yes")) %>% 
  
  select(readmission_30days_recode,
         ed_30days_recode,
         death_30_days,
         payor_category,
         sex_recode,
         race_category,
         ethnicity_recode,
         language_category,
         ICU_days_recode,
         dc_disp_category,
         payor_category,
         age_at_encounter,
         admission_HOSPITAL_score,
         admission_news_score,
         day_minus_2_HOSPITAL_score,
         day_minus_1_HOSPITAL_score,
         day_minus_1_news_score,
         day_minus_2_news_score,
         discharge_HOSPITAL_score,
         discharge_news_score)

# Creating descriptive statistics tables
# First, setting up the sorting of the tables
table_variable_order <- c("readmission_30days_recode",
                          "ed_30days_recode",
                          "death_30_days",
                          
                          "age_at_encounter",
                          "sex_recode",
                          "race_category",
                          "ethnicity_recode",
                          "language_category",
                          "ICU_days_recode",
                          "dc_disp_category",
                          
                          "discharge_HOSPITAL_score",
                          "day_minus_1_HOSPITAL_score",
                          "day_minus_2_HOSPITAL_score",
                          "admission_HOSPITAL_score",
                          "discharge_news_score",
                          "day_minus_1_news_score",
                          "day_minus_2_news_score",
                          "admission_news_score")

table_value_order <- c("Yes",
                       "No",
                       
                       "Home",
                       "Post-acute care",
                       "AMA",
                       "Hospice",
                       
                       "Hispanic",     
                       "Non-Hispanic",
                       
                       "English", 
                       "Spanish",                                  
                       
                       "White or Caucasian",                        
                       "Black or African American",                 
                       "Asian",                                     
                       
                       "American Indian or Alaska Native",  
                       "Native Hawaiian and Other Pacific Islander",
                       "More than one race",
                       
                       "Male",                                    
                       "Female",
                       
                       "Other",
                       "Unknown",
                       "Missing")

categorical_table <- d_tables %>% 
  select(readmission_30days_recode,
         ed_30days_recode,
         death_30_days,
         payor_category,
         sex_recode,
         race_category,
         ethnicity_recode,
         language_category,
         ICU_days_recode,
         dc_disp_category) %>%
  
  pivot_longer(-payor_category, names_to = "variable", values_to = "value") %>% 
  group_by(payor_category, variable, value) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = payor_category, values_from = count, values_fill = 0) %>%
#  mutate(Overall = rowSums(select(., c(-variable, -value)))) %>%
  arrange(match(variable, table_variable_order), 
          match(value, table_value_order))
view(categorical_table)

numeric_table <- d_tables %>% 
  select(payor_category,
         age_at_encounter,
         admission_HOSPITAL_score,
         admission_news_score,
         day_minus_2_HOSPITAL_score,
         day_minus_1_HOSPITAL_score,
         day_minus_1_news_score,
         day_minus_2_news_score,
         discharge_HOSPITAL_score,
         discharge_news_score) %>% 
  pivot_longer(-payor_category, names_to = "variable", values_to = "value") %>% 
  group_by(payor_category, variable) %>% 
  summarise(mean=mean(value, na.rm=TRUE)) %>% 
  pivot_wider(names_from = payor_category, values_from = mean, values_fill = 0)


d_analysis <- d_tables %>% 
  mutate(across(c(readmission_30days_recode,
                  ed_30days_recode,
                  death_30_days),
                ~recode(.,"No"=0,"Yes"=1,.default=NULL),
                .names="{.col}_analysis"),
         (across(c(payor_category,
                   sex_recode,
                   race_category,
                   ethnicity_recode,
                   language_category,
                   ICU_days_recode,
                   dc_disp_category),
                 ~factor(.),
                 .names="{.col}_analysis"))) %>% 
  
  select(readmission_30days_recode_analysis,
         ed_30days_recode_analysis,
         death_30_days_analysis,
         payor_category_analysis,
         sex_recode_analysis,
         race_category_analysis,
         ethnicity_recode_analysis,
         language_category_analysis,
         ICU_days_recode_analysis,
         dc_disp_category_analysis,
         age_at_encounter,
         admission_HOSPITAL_score,
         admission_news_score,
         day_minus_2_HOSPITAL_score,
         day_minus_1_HOSPITAL_score,
         day_minus_1_news_score,
         day_minus_2_news_score,
         discharge_HOSPITAL_score,
         discharge_news_score)

summary(HOSPITAL_read_fit_1 <- glm(readmission_30days_recode_analysis~
                                   discharge_HOSPITAL_score+
                                   payor_category_analysis+
                                   sex_recode_analysis+
                                   race_category_analysis+
                                   ethnicity_recode_analysis+
                                   language_category_analysis+
                                   ICU_days_recode_analysis+
                                   dc_disp_category_analysis+
                                   age_at_encounter+
                                   admission_HOSPITAL_score+
                                   day_minus_2_HOSPITAL_score+
                                   day_minus_1_HOSPITAL_score,
                                   data=d_analysis,
                                   family = binomial))

summary(HOSPITAL_ed_fit_1 <- glm(ed_30days_recode_analysis~
                                     discharge_HOSPITAL_score+
                                     payor_category_analysis+
                                     sex_recode_analysis+
                                     race_category_analysis+
                                     ethnicity_recode_analysis+
                                     language_category_analysis+
                                     ICU_days_recode_analysis+
                                     dc_disp_category_analysis+
                                     age_at_encounter+
                                     admission_HOSPITAL_score+
                                     day_minus_2_HOSPITAL_score+
                                     day_minus_1_HOSPITAL_score,
                                   data=d_analysis,
                                   family = binomial))

summary(HOSPITAL_death_fit_1 <- glm(death_30_days_analysis~
                                     discharge_HOSPITAL_score+
                                     payor_category_analysis+
                                     sex_recode_analysis+
                                     race_category_analysis+
                                     ethnicity_recode_analysis+
                                     language_category_analysis+
                                     ICU_days_recode_analysis+
                                     dc_disp_category_analysis+
                                     age_at_encounter+
                                     admission_HOSPITAL_score+
                                     day_minus_2_HOSPITAL_score+
                                     day_minus_1_HOSPITAL_score,
                                   data=d_analysis,
                                   family = binomial))

summary(NEWS_read_fit_1 <- glm(readmission_30days_recode_analysis~
                                     discharge_news_score+
                                     payor_category_analysis+
                                     sex_recode_analysis+
                                     race_category_analysis+
                                     ethnicity_recode_analysis+
                                     language_category_analysis+
                                     ICU_days_recode_analysis+
                                     dc_disp_category_analysis+
                                     age_at_encounter+
                                     admission_news_score+
                                     day_minus_2_news_score+
                                     day_minus_1_news_score,
                                   data=d_analysis,
                                   family = binomial))

summary(NEWS_ed_fit_1 <- glm(ed_30days_recode_analysis~
                                   discharge_news_score+
                                   payor_category_analysis+
                                   sex_recode_analysis+
                                   race_category_analysis+
                                   ethnicity_recode_analysis+
                                   language_category_analysis+
                                   ICU_days_recode_analysis+
                                   dc_disp_category_analysis+
                                   age_at_encounter+
                                   admission_news_score+
                                   day_minus_2_news_score+
                                   day_minus_1_news_score,
                                 data=d_analysis,
                                 family = binomial))

summary(NEWS_death_fit_1 <- glm(death_30_days_analysis~
                                      discharge_HOSPITAL_score+
                                      payor_category_analysis+
                                      sex_recode_analysis+
                                      race_category_analysis+
                                      ethnicity_recode_analysis+
                                      language_category_analysis+
                                      ICU_days_recode_analysis+
                                      dc_disp_category_analysis+
                                      age_at_encounter+
                                      admission_HOSPITAL_score+
                                      day_minus_2_HOSPITAL_score+
                                      day_minus_1_HOSPITAL_score,
                                    data=d_analysis,
                                    family = binomial))
fits_1 <- c(HOSPITAL_read_fit_1,HOSPITAL_ed_fit_1,HOSPITAL_death_fit_1,
            NEWS_read_fit_1,NEWS_ed_fit_1,NEWS_death_fit_1)

HOSPITAL_read_fit_1_drop <- drop1(HOSPITAL_read_fit_1,test="LRT")
HOSPITAL_ed_fit_1_drop <- drop1(HOSPITAL_ed_fit_1,test="LRT")
HOSPITAL_death_fit_1_drop <- drop1(HOSPITAL_death_fit_1,test="LRT")
NEWS_read_fit_1_drop <- drop1(NEWS_read_fit_1,test="LRT")
NEWS_ed_fit_1_drop <- drop1(NEWS_ed_fit_1,test="LRT")
NEWS_death_fit_1_drop <- drop1(NEWS_death_fit_1,test="LRT")

drop1_table <- cbind(HOSPITAL_read_fit_1_drop$`Pr(>Chi)`,
                       HOSPITAL_ed_fit_1_drop$`Pr(>Chi)`,
                       HOSPITAL_death_fit_1_drop$`Pr(>Chi)`,
                       NEWS_read_fit_1_drop$`Pr(>Chi)`,
                       NEWS_ed_fit_1_drop$`Pr(>Chi)`,
                       NEWS_death_fit_1_drop$`Pr(>Chi)`)
