library(tidyverse)
library(zoo)
library(data.table)
library(lfe)
library(aweek)
library(glmnet)
library(MLmetrics)
library(lubridate)
library(stringr)

setwd('/Users/benjaminmanning/Desktop/zearn')
z = read_csv('TestDataUncleaned.csv')

#backup df in case I mess up
z_hold = z

############################################################
############### KEY FOR THE CONTROL VARIABLES ####################
############################################################
#for categorical variables, wewill use the largest subgroup as the reference group:
# 1. Percentage of the teacher’s students at each grade level (omitting one grade level as 
#the reference group)
# 2. An indicator for whether the teacher’s account is a free account
# 3. The number of times the teacher logged into Zearn between August 1, 2021 and
#September 14, 2021 (inclusive) - what about how much they logged in during?
# 4. The number of the teacher’s students
# 5. The number of the teacher’s classrooms
# 6. The number of days the teacher has had a Zearn account before September 14, 2021
# 7. The number of days between the start of the school year and September 14, 2021
# 8. Baseline data (i.e., from the start of the 2021-2022 school year to September 14, 2021)
#on the dependent variable
# 9. Fixed effects for schools, omitting the category “unknown school”
# 10. An indicator for whether a teacher is in a school with just one teacher who is a Zearn user
# 11. An indicator for whether the teacher opened the first of two study-wide emails sent
#immediately prior to the intervention period
# 12. An indicator for whether the teacher opened the second of two study-wide emails sent 
#immediately prior to the intervention period
# 13.To handle missing values of predictor variables, we will introduce indicator variables in 
#our regression for any variable that’s missing values, and these indicators will take on a
#value of hen the variable in question is missing and 0 otherwise

############################################################
############### KEY FOR THE DEPENDENT VARIABLE ####################
############################################################
# 1. The primary dependent variable will be student math performance as indicated by the
#average number of lessons that teachers’ students successfully complete on the Zearn 
#platform during the 4 week intervention period beginning Wednesday, September 15, 2021 
#and ending Tuesday, October 12, 2021.

# 2. As secondary dependent variables, we will assess student engagement as indicated by the
#average time that teachers’ students spend on the Zearn platform during the 4 week
#intervention period and 

# 3. teacher engagement as indicated by the number of times teachers log in
#to Zearn Math during the 4week intervention period.
# 4. For the purpose of assessing the enduring impact of treatment effects, all three of these
#dependent variables will be re-created for a 4 week follow up period beginning Wednesday,
#October 13, 2021, as well as a 8 week follow up period beginning on Wednesday, October 13,

#instructions on DV from angela
#Create a separate row for each teacher for each week, 
#and include an indicator for weeks during the intervention. 

#########################################
############# DATA CLEANING #############
#########################################

# 1. Percentage of the teacher’s students at each grade level (omitting one grade level as 
#the reference group)

#teachers with more than one school

#NO NA's
table(is.na(z_hold$classroom_grade))

z = z %>%
  filter(classroom_grade == 'G1' |
           classroom_grade == 'G2'|
           classroom_grade == 'G3'|
           classroom_grade == 'G4'|
           classroom_grade == "G5"|
           classroom_grade == "G6"|
           classroom_grade == "G7"|
           classroom_grade == "G8")

#grouping number of students per classroom
stud_per_class = z %>% 
  group_by(teacher_id, classroom_id,classroom_grade) %>% 
  summarise(n_students = sum(n_students))

#grouping number of students per teacher
stud_per_teacher = z %>% 
  group_by(teacher_id) %>% 
  summarise(n_students_per_teach = sum(n_students))

#combining per teacher and per class and getting prop of class of all teachers students
stud_per =  left_join(stud_per_class, stud_per_teacher, by = 'teacher_id') %>% 
  mutate(prop_per_class = n_students/n_students_per_teach)

#making sure all props = 1 across teachers
teacher_check = stud_per %>% 
  group_by(teacher_id) %>% 
  summarise(teacher_full = sum(prop_per_class))

#all sum to 1 -YAY
table(teacher_check$teacher_full)

#getting grade props
stud_per_wide <- stud_per %>%
  pivot_wider(names_from = classroom_grade, values_from = prop_per_class) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  group_by(teacher_id) %>% 
  summarise(G1 = sum(G1),
            G2 = sum(G2),
            G3 = sum(G3),
            G4 = sum(G4),
            G5 = sum(G5),
            G6 = sum(G6),
            G7 = sum(G7),
            G8 = sum(G8),
            )
#alt way to dump 0s
#myDataframe[is.na(myDataframe)] = 0

#check to make sure all sum to 1
stud_per_wide = stud_per_wide %>% 
  mutate(prop_total = G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8)
#still good!
table(stud_per_wide$prop_total)
stud_per_wide$prop_total = NULL

#rejoing with original - NEED TO DROP ONE FOR FINAL ANALYSIS
z =  left_join(z, stud_per_wide, by = 'teacher_id') 

# 2. An indicator for whether the teacher’s account is a free account
table(is.na(z_hold$is_paid))

table(z_hold$is_paid)

###NA means free :)

z = z %>% 
  mutate(is_paid = case_when(
    is_paid == TRUE ~ 1,
    is_paid == FALSE ~ 0,
    is.na(is_paid) == TRUE ~ 0,
  ))

table(is.na(z$is_paid))
# 3. The number of times the teacher logged into Zearn between August 1, 2021 and
#September 14, 2021 (inclusive) - per week!

##### See end for long to wide stuff
#JUST IMPUTING MEANS HERE! - CURRENTLY NO NAs in Fake data

z_log_impute = z %>% 
  select(contains('_teacher_logins'))


# 4. The number of the teacher’s students
# acquired when getting prop grades (in #1)

z =  left_join(z, stud_per_teacher, by = 'teacher_id') 

# 5. The number of the teacher’s classrooms
#dropping classes with teacher Id == NA
z = z %>% 
  filter(is.na(teacher_id) == FALSE)

class_per_teacher = z %>% 
  group_by(teacher_id) %>% 
  summarise(classrooms = n())

z =  left_join(z, class_per_teacher, by = 'teacher_id') 

# 6. The number of days the teacher has had a Zearn account before September 14, 2021

table(is.na(z$acct_created))

days_since_created = z %>% 
  select(teacher_id, acct_created) %>% 
  mutate(days_since_created = as.numeric(today() - as.Date(acct_created))) %>% 
  select(teacher_id, days_since_created) %>% 
  unique()

z =  left_join(z, days_since_created, by = 'teacher_id')

#indicator for missing
table(is.na(z$days_since_created))
table(z$days_since_created)

z$days_since_created_missing = ifelse(is.na(z$days_since_created)==TRUE, 1,0)
table(z$days_since_created_missing)

#imputing meaen
z$days_since_created = ifelse(is.na(z$days_since_created)==TRUE, mean(z$days_since_created, na.rm=TRUE),z$days_since_created)

summary(z$days_since_created)

# 7. The number of days between the start of the school year and September 14, 2021

table(is.na(z$school_start))

days_since_school_start = z %>% 
  select(teacher_id, school_start) %>% 
  mutate(days_since_school_start = as.numeric(today() - as.Date(school_start))) %>% 
  select(teacher_id, days_since_school_start) %>% 
  unique()

z =  left_join(z, days_since_school_start, by = 'teacher_id')

#indicator for missing
table(is.na(z$days_since_school_start))
table(z$days_since_school_start)

z$days_since_school_start_missing = ifelse(is.na(z$days_since_school_start)==TRUE, 1,0)
table(z$days_since_school_start_missing)

#imputing meaen
z$days_since_school_start = ifelse(is.na(z$days_since_school_start)==TRUE, mean(z$days_since_school_start, na.rm=TRUE),z$days_since_school_start)

summary(z$days_since_school_start)

# 8. Baseline data (i.e., from the start of the 2021-2022 school year to September 14, 2021)
#on the dependent variable

##### See end for long to wide stuff

# 9. Fixed effects for schools, omitting the category “unknown school”

#making unknown school 0
z$school_id[is.na(z$school_id)] <- 0

#WILL DO IN FINAL REGRESSION

# 10. An indicator for whether a teacher is in a school with just one teacher who is a Zearn user
# solo_zearn_at_school = z %>% 
#   group_by(school_id,teacher_id) %>% 
#   summarise(num_classes_pr = n()) %>% 
#   group_by(school_id) %>% 
#   summarise(num_teachers_per_school = n())
# 
# z =  left_join(z,solo_zearn_at_school, by = 'school_id')
# 
# z = z %>% 
#   rename(only_zearn_teacher_at_school = num_teachers_per_school) %>% 
#   mutate(only_zearn_teacher_at_school = case_when(
#     only_zearn_teacher_at_school == 1 ~ 1,
#     only_zearn_teacher_at_school != 1 ~ 0,
#   ))

# 11. An indicator for whether the teacher opened the first of two study-wide emails sent
#immediately prior to the intervention period
table(is.na(z$opened_weekneg1))

table(z$opened_weekneg1)

# 12. An indicator for whether the teacher opened the second of two study-wide emails sent 
#immediately prior to the intervention period

table(z$opened_week0)

table(is.na(z$opened_week0))

# 13.To handle missing values of predictor variables, we will introduce indicator variables in 
#our regression for any variable that’s missing values, and these indicators will take on a
#value of 1 when the variable in question is missing and 0 otherwise

######### IS THIS A VARIABLE FOR EVERY SINGLE VARIABLE? ALSO, WE HAVE TO IMPUTE SOMETHING - YA?

#############################################
################ EXCLUSIONS KEY################
#############################################

# 1. there is reason to believe teachers do not receive study emails (e.g., invalid email addresses, delivery error),
# 2. there is evidence of technical problems (e.g., Zearn Math platform data with implausible statistics),
# 3. teachers have no current students associated with their account,
# 4. there is no record of either a teacher login or associated student login to Zearn Math from 
#March 1, 2021 through September 14, 2021 (inclusive),
# 5. the Zearn classroom is associated with more than one Zearn teacher (in which case we
#will omit that classroom from analysis), or 
# 6. teachers have more than 150 students associated with their account
# 7. teachers have 7 or more classrooms
# 8. Zearn classroom is in high school or post high school

# 1. there is reason to believe teachers do not receive study emails (e.g., invalid email addresses, delivery error),

##### CAN'T TEST BEFOREHAND #############

# 2. there is evidence of technical problems (e.g., Zearn Math platform data with implausible statistics),

##### CAN'T TEST BEFOREHAND #############

# 3. teachers have no current students associated with their account,

###$# CAN'T TEST BEFOREHAND - ZEARN ONLY GIVING US IF HAS #############

# 4. there is no record of either a teacher login or associated student login to Zearn Math from 
#March 1, 2021 through September 14, 2021 (inclusive),

############  we don't have back to march ############ 

# 5. the Zearn classroom is associated with more than one Zearn teacher (in which case we
#will omit that classroom from analysis), or 

teachers_per_classroom = z %>% 
  group_by(classroom_id) %>% 
  summarise(teach_per_class = n())

z =  left_join(z,teachers_per_classroom, by = 'classroom_id')

z = z %>% 
  filter(teach_per_class== 1)

# 6. teachers have more than 150 students associated with their account

z = z %>% 
  filter(n_students_per_teach <= 150)

# 7. teachers have 7 or more classrooms

z = z %>% 
  filter(classrooms <= 7)

# 8. Zearn classroom is in high school or post high school

##### FIXED IN THE VREY BEGINNING SO THAT I COULD GET THE CORRECT GRADE PROPS ####


################################################################################
############# Going to per week data wide to long to wide ##############
######################################## ########################################

# doing at the end because computaitonally intensive and saves time after exclusions
z_to_long = z %>% 
  select(teacher_id, classroom_id, contains('w')) %>% 
  pivot_longer(cols = starts_with("w"),
               names_to = "week",
               values_to = "value") %>% 
  mutate(week_num = case_when(
    str_detect(week, 'wneg5') ~ -5,
    str_detect(week, 'wneg4') ~ -4,
    str_detect(week, 'wneg3') ~ -3,
    str_detect(week, 'wneg2') ~ -2,
    str_detect(week, 'wneg1') ~ -1,
    str_detect(week, 'w0') ~ 0,
    str_detect(week, 'w1') ~ 1,
    str_detect(week, 'w2') ~ 2,
    str_detect(week, 'w3') ~ 3,
    str_detect(week, 'w4') ~ 4,
  ))

z_long_str_edit = z_to_long %>% 
  mutate(week = str_replace_all(week, 'wneg5_', '')) %>% 
  mutate(week = str_replace_all(week, 'wneg4_', '')) %>% 
  mutate(week = str_replace_all(week, 'wneg3_', '')) %>% 
  mutate(week = str_replace_all(week, 'wneg2_', '')) %>% 
  mutate(week = str_replace_all(week, 'wneg1_', '')) %>% 
  mutate(week = str_replace_all(week, 'w0_', '')) %>% 
  mutate(week = str_replace_all(week, 'w1_', '')) %>% 
  mutate(week = str_replace_all(week, 'w2_', '')) %>% 
  mutate(week = str_replace_all(week, 'w3_', '')) %>% 
  mutate(week = str_replace_all(week, 'w4_', ''))

z_to_wide = z_long_str_edit %>% 
  select(-contains('open')) %>% 
  pivot_wider(names_from = week, values_from = value) %>% 
  rename(teacher_logins_per_week = teacher_logins)

#getting rid of extra columns z 
z = z %>% 
  select(-starts_with('w'))


z =  left_join(z_to_wide , z, by = c('teacher_id','classroom_id') )

z = z %>% 
  mutate(during_treatment = case_when(
    week_num < 0 ~ 0,
    week_num >= 0 ~ 1
  ))

# 3. The number of times the teacher logged into Zearn between August 1, 2021 and
#September 14, 2021 (inclusive) - per week!

# NO NA'S!
table(is.na(z$teacher_logins_per_week))

z %>% 
  select(teacher_logins_per_week, week_num) %>% 
  filter(week_num < 0)

# 8. Baseline data (i.e., from the start of the 2021-2022 school year to September 14, 2021)
#on the dependent variable

#CREATING INDICATOR FOR MISSING BASELINE
z = z %>% 
  mutate(baseline_mins_mising = case_when(
    during_treatment == 0 & is.na(mins_per_active) == TRUE ~ 1,
    during_treatment == 1 | is.na(mins_per_active) == FALSE ~0
  )) %>% 
  mutate(baseline_badges_mising = case_when(
    during_treatment == 0 & is.na(badges_per_active) == TRUE ~ 1,
    during_treatment == 1 | is.na(badges_per_active) == FALSE ~ 0
  ))

#Imputing Mean
z$mins_per_active = ifelse(z$baseline_mins_mising == 1, 0, z$mins_per_active)
z$badges_per_active = ifelse(z$baseline_badges_mising == 1,0, z$badges_per_active)

####################################################
############# Fixing the DV for number active ##############
######################################## #########

#badges_per_active * total active that week / n_students per classroom total for each classroom
#weighed average based on the number of classroom and students per classroom

#numerator: badges all students did that week / total number of students
#WEIGHTS is N_STUDENTS
#DROP LARGEST SUBGROUP - MOST FOR THAT GRADE

#making students per active 0 instead of NA
z$students_active[is.na(z$students_active)] <- 0

z = z %>% 
  mutate(badges_avg = badges_per_active*students_active/n_students)


save.image('test_zearn_analysis_progress.Rdata')
load('test_zearn_analysis_progress.Rdata')
###############################
###### dependent Variable ############
###############################

# 1. The primary dependent variable will be student math performance as indicated by the
#average number of lessons that teachers’ students successfully complete on the Zearn 
#platform during the 4 week intervention period beginning Wednesday, September 15, 2021 
#and ending Tuesday, October 12, 2021.

table(z$classroom_grade)

z = z %>% 
  mutate(condition = case_when(
    condition >= 16 ~ 16,
    condition < 16 ~ condition
  )) %>% 
  mutate(condition = as.factor(condition))


class(z$condition)
#what is the biggest grade class? - G2
table(z$classroom_grade)

fit1 = felm(badges_avg ~ G1 + G3 + G4 + G5 + G6 + G7 + G8 + is_paid +
              teacher_logins_per_week + n_students_per_teach + classrooms +
              days_since_created + days_since_school_start + 
              opened_weekneg1 + opened_week0 +
              condition*during_treatment +
              # missing indicators
              days_since_school_start_missing
              #drop days since created missing
              #baseline_mins_mising + baseline_badges_mising
              #fixed effects
              | school_id,  data = z, weights = z$n_students_per_teach)

summary(fit1)

# 2. As secondary dependent variables, we will assess student engagement as indicated by the
#average time that teachers’ students spend on the Zearn platform during the 4 week
#intervention period and 

#minutes_per_active * total active that week / n_students total for each classrooms

fit2 = felm(mins_per_active ~ G1 + G2 + G3 + G4 + G5 + G6 + G7 + is_paid +
              teacher_logins_per_week + n_students_per_teach + classrooms +
              days_since_created + days_since_school_start + 
              opened_weekneg1 + opened_week0 +
              condition*during_treatment + 
              condition + during_treatment +
              # missing indicators
              days_since_created_missing + days_since_school_start_missing +
              baseline_mins_mising + baseline_badges_mising
              #fixed effects
              | school_id,data = z)

summary(fit2)


# 3. teacher engagement as indicated by the number of times teachers log in
#to Zearn Math during the 4week intervention period.

# 4. For the purpose of assessing the enduring impact of treatment effects, all three of these
#dependent variables will be re-created for a 4 week follow up period beginning Wednesday,
#October 13, 2021, as well as a 8 week follow up period beginning on Wednesday, October 13,






