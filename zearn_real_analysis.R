library(tidyverse)
library(lfe)
library(readxl)

setwd("~/Desktop/zearn/BCFG Main File 2021-10-18")

###############################################
########### DATA FRAMES AND DESCRIPTIONS ######
###############################################

# all studywide emails, what the contents was, teacher it was sent to, whether it was opened, whether it was clicked on
email_opens = read_csv('BCFG - Email Opens 2021-10-18T1323.csv')

#every day that a student logged in for each classroom, no zeros, must impute, keep teachers even without this data
student_usage = read_csv('BCFG Main File - Student Usage 2021-10-18T1140.csv')

classroom_info = read_csv('BCFG Main File - Teacher Classroom Info 2021-10-18T0940.csv')
teacher_sessions = read_csv('BCFG Main File - Teacher Sessions 2021-10-18T0940.csv')
teachers_active_sessions = read_csv('BCFG Main File - Teachers Active Since March 2021-10-18T0940.csv')
teachers_with_active_student = read_csv('BCFG Main File - Teachers with Students Active Since March 2021-10-18T0941.csv')
#mindset_survey = read_excel('BCFG Mindset survey responses.xlsx')
#report_views = read_csv('BCFG - Report Views 2021-10-18T1541.csv')
#qualtrics_link = read_csv('BCFG - Qualtrics Link by Teacher Pseudonymized 2021-10-18T0751.csv')
#std_dev_usage = read_csv('BCFG Main File - Standard Deviations 2021-10-18T0943.csv')

#LINK TO CLEANING ORDER OF OPERATIONS: https://docs.google.com/document/d/1ffOyMtP3TiNtYMq4oAQ3rfQBuc4GN93l7AmJD2EwiwM/edit
#SEE LINK TO REFERENCE OF ALL THE OPERATIONS (IN ORDER)


###########################################
################SECTION 1 ##################
###########################################

#renaming variables
classroom_info = classroom_info %>% 
  rename(
    teacher_id = `User ID (Pseudonymized)`,
    classroom_id = `Classroom ID`,
    n_student_per_class = `Classroom Student Count`,
    school_id = `Zearn School ID`,
    condition = `BCFG Test Group`,
    grade_level = `Grade Level`
  )


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

classroom_info = classroom_info %>% 
  mutate(no_log_or_student_since_march = ifelse(
    !teacher_id %in% teachers_active_sessions$`User ID (Pseudonymized)` &
      !teacher_id %in% teachers_with_active_student$`User ID (Pseudonymized)`, 1,0)
  )
table(classroom_info)

3546591 - 1
824627 - 0

#4. Create indicator for teachers who received 0 emails (invalid email address, delivery error)
classroom_info = classroom_info %>% 
  mutate(no_emails_received = ifelse(
    !teacher_id %in% email_opens$`Lead Zearn User ID (Pseudonymized)`,1,0)
  )

table(classroom_info$no_emails_received)

#5. Remove classes with no students
classroom_info = classroom_info %>% 
  filter(n_student_per_class > 0)

#6. Calculate number of students per teacher, create indicator for teachers with > 150 students
students_per_teacher = classroom_info %>% 
  group_by(teacher_id) %>% 
  summarise(students_per_teacher = sum(n_student_per_class)) 

classroom_info = left_join(classroom_info, students_per_teacher, by = 'teacher_id')

classroom_info = classroom_info %>% 
  mutate(more_150_students = ifelse(
    classroom_info$students_per_teacher > 150,1,0
  ))

#7. Calculate number of classes per teacher, create indicator for teachers with > 6 classes
class_per_teacher = classroom_info %>% 
  group_by(teacher_id) %>% 
  summarise(class_per_teacher = n())

classroom_info = left_join(classroom_info, class_per_teacher, by = 'teacher_id')

classroom_info = classroom_info %>% 
  mutate(more_6_classes= ifelse(
    classroom_info$class_per_teacher > 6,1,0
  ))

#8. Remove teachers with any of the following: 
#indicator for did not log in and had no student log in since March (c), 
#>150 students (e), > 6 classes f) no email delivery g) remove classes in HS+
classroom_info = classroom_info %>% 
  filter(no_log_or_student_since_march != 1) %>% 
  filter(no_emails_received != 1) %>% 
  filter(more_150_students != 1) %>% 
  filter(more_6_classes != 1) %>% 
  filter(grade_level == 'G1' |
           grade_level == 'G2'|
           grade_level == 'G3'|
           grade_level == 'G4'|
           grade_level == "G5"|
           grade_level == "G6"|
           grade_level == "G7"|
           grade_level == "G8"|
           grade_level == 'K' |
           grade_level == 'PK')


#9. Calculate number of teachers per classroom, create an indicator for shared classroom
teachers_per_class = classroom_info %>% 
  group_by(classroom_id) %>% 
  summarise(teachers_per_class = n())

classroom_info = left_join(classroom_info, teachers_per_class, by ='classroom_id')

classroom_info = classroom_info %>% 
  mutate(shared_class = ifelse(
    classroom_info$teachers_per_class > 1, 1, 0
  ))

#10. Remove teachers with K/preK primary grade
classroom_info = classroom_info %>% 
  filter(`Teacher Main Grade Level` != 'K' &
           `Teacher Main Grade Level` != 'PK')

#11. Remove preK/K classrooms
classroom_info = classroom_info %>% 
  filter(grade_level != 'K' &
           grade_level != 'PK')


###########################################
################SECTION 2 ##################
###########################################

#1. Create week in usage data (based on actual date)
student_usage = student_usage %>% 
  rename(usage_date = `Usage Date`) %>% 
  rename(classroom_id = `Classroom ID`) %>% 
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

#2. Create week in teacher data (-8 to 4 for each teacher)
#joining teacher sessions with classroom
teacher_sessions = teacher_sessions %>% 
  rename(usage_date = `Usage Date`) %>% 
  rename(teacher_id = `User ID (Pseudonymized)`)

classroom_info = left_join(teacher_sessions, classroom_info, by = 'teacher_id')

classroom_info = classroom_info %>% 
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

#getting rid of useless rows
classroom_info = classroom_info %>% 
  filter(!is.na(classroom_id))

#3. Merge usage data into teacher data by class ID and week (Data now at the CLASSROOM-DAY Level)
#merging student usage and classroom info

usage_classroom = left_join(classroom_info, student_usage, by = c('classroom_id','week'))


length(unique(classroom_info$teacher_id))









