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
################SECTION 1##################
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
teacher_class_dup = classroom_info %>%  
  group_by(teacher_id, classroom_id) %>% 
  summarise(count = n()) %>% 
  filter(count >1)

classroom_info = classroom_info %>% 
  filter(!classroom_id %in% teacher_class_dup$classroom_id)

#3. Create indicator for teachers who 1. Did not log in to Zearn since March 
#AND 2. had no students log in to Zearn since March  
classroom_info = classroom_info %>% 
  mutate(no_log_or_student_since_march = ifelse(
    !teacher_id %in% teachers_active_sessions$`User ID (Pseudonymized)` &
    !teacher_id %in% teachers_with_active_student$`User ID (Pseudonymized)`, 1,0)
    )

#4. Create indicator for teachers who received 0 emails (invalid email address, delivery error)
classroom_info = classroom_info %>% 
  mutate(no_emails_received = ifelse(
    !teacher_id %in% email_opens$`Lead Zearn User ID (Pseudonymized)`,1,0)
    )

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
hold = classroom_info
classroom_info = hold %>% 
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






