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

#
