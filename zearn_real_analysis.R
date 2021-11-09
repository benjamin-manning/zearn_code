library(tidyverse)
library(lfe)
library(readxl)
library(readr)

setwd("C:/Users/beman/Dropbox/BCFG Zearn Mega-Study/2021 data/BCFG Main File 2021-10-18")

###############################################
########### DATA FRAMES AND DESCRIPTIONS ######
###############################################

# # all studywide emails, what the contents was, teacher it was sent to, whether it was opened, whether it was clicked on
# email_opens = read_csv('BCFG - Email Opens 2021-10-18T1323.csv', col_types = list(`User ID (Pseudonymized)` = "c"))
# 
# #every day that a student logged in for each classroom, no zeros, must impute, keep teachers even without this data
# student_usage = read_csv('BCFG Main File - Student Usage 2021-10-18T1140.csv')

# classroom_info = read_csv('BCFG Main File - Teacher Classroom Info 2021-10-18T0940.csv', col_types = list(`User ID (Pseudonymized)` = "c"))
# teacher_sessions = read_csv('BCFG Main File - Teacher Sessions 2021-10-18T0940.csv', col_types = list(`User ID (Pseudonymized)` = "c"))
# teachers_active_sessions <- read_csv("BCFG Main File - Teachers Active Since March 2021-10-26T1056.csv", col_types = list(`User ID (Pseudonymized)` = "c"))
# teachers_with_active_student = read_csv('BCFG Main File - Teachers with Students Active Since March 2021-10-18T0941.csv', col_types = list(`User ID (Pseudonymized)` = "c"))
# mindset_survey = read_excel('BCFG Mindset survey responses.xlsx')
# report_views = read_csv('BCFG - Report Views 2021-10-18T1541.csv')
# qualtrics_link = read_csv('BCFG - Qualtrics Link by Teacher Pseudonymized 2021-10-18T0751.csv')
# std_dev_usage = read_csv('BCFG Main File - Standard Deviations 2021-10-18T0943.csv')

#LINK TO CLEANING ORDER OF OPERATIONS: https://docs.google.com/document/d/1ffOyMtP3TiNtYMq4oAQ3rfQBuc4GN93l7AmJD2EwiwM/edit
#SEE LINK TO REFERENCE OF ALL THE OPERATIONS (IN ORDER)

classroom_info = read_csv('BCFG Main File - Teacher Classroom Info 2021-10-18T0940.csv', col_types = list(`User ID (Pseudonymized)` = "c"))
#renaming variables
colnames(classroom_info) = c('classroom_id', 'teacher_id', 'classroom_grade', 'primary_grade',
                             'condition', 'school_id1', 'school_id2', 
                             'usage_prediction', "school_start", "n_students",
                             "acct_created", "school_type", "is_charter")

###########################################
################SECTION 1 ##################
###########################################

#1. Remove teachers with study condition NA
classroom_info = classroom_info %>% 
  filter(!is.na(condition))

#2. Remove classrooms with duplicate classroom AND teacher IDs
classroom_info = classroom_info %>% 
  group_by(teacher_id, classroom_id) %>% 
  mutate(n_dups = n()) %>% 
  filter(n_dups == 1) %>% 
  select(-n_dups)

#3. Create indicator for teachers who 1. Did not log in to Zearn since March 
#AND 2. had no students log in to Zearn since March 
students_active <- read_csv("BCFG Main File - Teachers with Students Active Since March 2021-10-18T0941.csv", col_types = list(`User ID (Pseudonymized)` = "c", `Active Students - Total` = "c"))
colnames(students_active) <- c("teacher_id", "students_active")
students_active <- students_active %>%
  mutate(students_active = gsub(",", '',students_active)) %>%
  mutate(students_active = as.numeric(students_active))

teachers_active <- read_csv("BCFG Main File - Teachers Active Since March 2021-10-26T1056.csv" , col_types = list(`User ID (Pseudonymized)` = "c"))
colnames(teachers_active) <- c("teacher_id", "teachers_active")

classroom_info <- left_join(classroom_info, teachers_active, by = "teacher_id") %>%
 left_join(students_active, by = "teacher_id")

classroom_info <- classroom_info %>%
  mutate(students_active =  ifelse(is.na(students_active), 1, 0),
         teachers_active = ifelse(is.na(teachers_active), 1, 0)) %>%
  mutate(no_log_or_student_since_march = ifelse((students_active == 1 & teachers_active ==1), 1, 0))

table(classroom_info$no_log_or_student_since_march)

#4. Create indicator for teachers who received 0 emails (invalid email address, delivery error)
email_opens = read_csv('BCFG - Email Opens 2021-10-18T1323.csv', col_types = list(`Lead Zearn User ID (Pseudonymized)` = "c"))
colnames(email_opens) = c("teacher_id", "date", "folder", "email", "subject", "opened", "clicked")

email_error = email_opens %>% 
  group_by(teacher_id) %>% 
  summarise(count = n())

classroom_info = left_join(classroom_info,email_error , by = 'teacher_id') 

classroom_info = classroom_info %>% 
  rename(no_receive_email = count) %>% 
  mutate(no_receive_email = ifelse(
    is.na(no_receive_email),1,0 )
  )

table(classroom_info$no_receive_email)

#5. Remove classes with no students
classroom_info <- classroom_info[classroom_info$n_students > 0,]

#6. Calculate number of students per teacher, create indicator for teachers with > 150 students
classroom_info = classroom_info %>% 
  group_by(teacher_id) %>% 
  mutate(students_per_teacher = sum(n_students)) %>% 
  mutate(more_150_students = ifelse(
    students_per_teacher > 150,1,0
  ))

table(classroom_info$more_150_students)
#7. Calculate number of classes per teacher, create indicator for teachers with > 6 classes
classroom_info = classroom_info %>% 
  group_by(teacher_id) %>% 
  mutate(class_per_teacher = n()) %>% 
  mutate(more_6_classes= ifelse(
    class_per_teacher >= 7, 1, 0
  ))

table(classroom_info$more_6_classes)
#8. Remove teachers with any of the following: 
#indicator for did not log in and had no student log in since March (c), 
#>150 students (e), > 6 classes f) no email delivery g) remove classes in HS+
classroom_info = classroom_info%>% 
  filter(no_log_or_student_since_march != 1) %>% 
  filter(no_receive_email!=1) %>% 
  filter(more_150_students != 1) %>% 
  filter(more_6_classes != 1) %>% 
  filter(classroom_grade == 'G1' |
           classroom_grade == 'G2'|
           classroom_grade == 'G3'|
           classroom_grade == 'G4'|
           classroom_grade == "G5"|
           classroom_grade == "G6"|
           classroom_grade == "G7"|
           classroom_grade == "G8"|
           classroom_grade == 'K' |
           classroom_grade == 'PK')


#9. Calculate number of teachers per classroom, create an indicator for shared classroom - remove
classroom_info = classroom_info %>% 
  group_by(classroom_id) %>% 
  mutate(teachers_per_class = n()) %>% 
  mutate(shared_class = ifelse(
    teachers_per_class > 1, 1, 0
  ))

classroom_info = classroom_info[classroom_info$shared_class==0,]

#10. Remove teachers with K/preK primary grade
classroom_info = classroom_info %>% 
  filter(primary_grade != 'K' &
           primary_grade != 'PK')

#11. Remove preK/K classrooms
classroom_info = classroom_info %>% 
  filter(classroom_grade != 'K' &
           classroom_grade != 'PK')

#Remove teachers with emails from multiple arms
multi_arm_email = email_opens %>% 
  group_by(teacher_id, folder) %>% 
  summarise(count = n()) %>% 
  filter(folder != 'BCFG_StudyWideMessage') %>% 
  group_by(teacher_id) %>% 
  summarise(count = n()) %>% 
  rename(multi_arm_email = count) %>% 
  mutate(multi_arm_email = multi_arm_email -1)

classroom_info = left_join(classroom_info, multi_arm_email, by = 'teacher_id')

classroom_info = classroom_info[classroom_info$multi_arm_email == 0,]

###########################################
################SECTION 2 ##################
###########################################

#1. Create week in usage data (based on actual date)
student_usage= read_csv('BCFG Main File - Student Usage 2021-10-18T1140.csv', col_types = list(`Classroom ID` = "c"))
colnames(student_usage) = c('classroom_id', 'usage_date', 'total_active_students', 'sessions_per_active_student', 'mins_per_active_students',
                            'lessons_comp_per_active_student','tower_alerts_per_active_student')

student_usage = student_usage %>% 
  mutate(week = case_when(
    usage_date >=  "2021-07-14" & usage_date <= "2021-07-20" ~ -8,
    usage_date >=  "2021-07-21" & usage_date <= "2021-07-27" ~ -7,
    usage_date >=  "2021-07-28" & usage_date <= "2021-08-03" ~ -6,
    usage_date >=  "2021-08-04" & usage_date <= "2021-08-10" ~ -5,
    usage_date >=  "2021-08-11" & usage_date <= "2021-08-17" ~ -4,
    usage_date >=  "2021-08-18" & usage_date <= "2021-08-24" ~ -3,
    usage_date >=  "2021-08-25" & usage_date <= "2021-08-31" ~ -2,
    usage_date >=  "2021-09-01" & usage_date <= "2021-09-07" ~ -1,
    usage_date >=  "2021-09-08" & usage_date <= "2021-09-14" ~ 0,
    usage_date >=  "2021-09-15" & usage_date <= "2021-09-21" ~ 1,
    usage_date >=  "2021-09-22" & usage_date <= "2021-09-28" ~ 2,
    usage_date >=  "2021-09-29" & usage_date <= "2021-10-05" ~ 3,
    usage_date >=  "2021-10-06" & usage_date <= "2021-10-12" ~ 4
    
  )
  )

table(student_usage$week)

### --------------- GOOD THROUGH HERE!!

#2. Create week in teacher data (-8 to 4 for each teacher)
teacher_sessions = read_csv('BCFG Main File - Teacher Sessions 2021-10-18T0940.csv', col_types = list(`User ID (Pseudonymized)` = "c"))

colnames(teacher_sessions) = c("teacher_id", "usage_date", "logins")

teacher_sessions = teacher_sessions %>% 
  mutate(usage_date = as.Date(usage_date)) %>% 
  mutate(week = case_when(
    usage_date >=  "2021-07-14" & usage_date <= "2021-07-20" ~ -8,
    usage_date >=  "2021-07-21" & usage_date <= "2021-07-27" ~ -7,
    usage_date >=  "2021-07-28" & usage_date <= "2021-08-03" ~ -6,
    usage_date >=  "2021-08-04" & usage_date <= "2021-08-10" ~ -5,
    usage_date >=  "2021-08-11" & usage_date <= "2021-08-17" ~ -4,
    usage_date >=  "2021-08-18" & usage_date <= "2021-08-24" ~ -3,
    usage_date >=  "2021-08-25" & usage_date <= "2021-08-31" ~ -2,
    usage_date >=  "2021-09-01" & usage_date <= "2021-09-07" ~ -1,
    usage_date >=  "2021-09-08" & usage_date <= "2021-09-14" ~ 0,
    usage_date >=  "2021-09-15" & usage_date <= "2021-09-21" ~ 1,
    usage_date >=  "2021-09-22" & usage_date <= "2021-09-28" ~ 2,
    usage_date >=  "2021-09-29" & usage_date <= "2021-10-05" ~ 3,
    usage_date >=  "2021-10-06" & usage_date <= "2021-10-12" ~ 4
  )
  ) %>% 
  filter(!is.na(week))

classroom_info = left_join(teacher_sessions, classroom_info, by = 'teacher_id')


#3. Merge usage data into teacher data by class ID and week (Data now at the CLASSROOM-DAY Level)
#merging student usage and classroom info

classroom_info = classroom_info %>% 
  mutate(classroom_id = as.character(classroom_id))

usage_classroom = left_join(classroom_info, student_usage, by = c('classroom_id','usage_date'))


length(unique(classroom_info$teacher_id))









