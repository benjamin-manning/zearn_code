library(tidyverse)
library(zoo)
library(data.table)
library(aweek)
library(glmnet)
library(MLmetrics)
library(lubridate)
library(stringr)

#################################################################

## Importing files:
setwd("~/Desktop/zearn")
teacher_usage <- read.csv("teacher_usage_time_series_2020-07-28T1651.csv")
classroom_student_usage <- read.csv("Classroom Student Usage - Time Series 2020-10-09T1616.csv")
classroom_teacher_lookup <- read.csv("Classroom-Teacher Lookup 2020-10-09T1618.csv")


################################################################
###### CONDITIONAL PROBABLITIES OF LOGGING IN THE NEXT DAY ######
################################################################

#ADJUSTING DATES
teacher_usage$Usage.Time <- as.Date(teacher_usage$Usage.Time)

#UNIQUE TEACHER ID'S TO ITERATE
unique_ids <- unique(teacher_usage$Adult.User.ID)
total_distribution <- c()

#FILTERING BY DATE RANGE FOR BCFG EXPERIMENT (EXCEPT THIS WILL BE 2021)
teacher_usage <- teacher_usage %>% 
  filter(Usage.Time >='2019-09-10') %>% 
  filter(Usage.Time <='2019-10-20')

##GETTING LAG TIMES PER TEACHER
for (id in unique_ids){
  #FILTERING BY ID
  test <- teacher_usage %>% 
    filter(Adult.User.ID == id) %>% 
    arrange(Usage.Time)
  #GETTING RID OF MULTIPLE LOG INS PER DAY
  days_between <- unique(test$Usage.Time)
  unique_days <- length(days_between)
  
  #DIFFERENCES BETWEEN ORDERED LOG IN DAYS
  day_distribution <- c()
  for (i in 1:(length(days_between)-1)){
    day_distribution <- append(day_distribution ,as.numeric((days_between[i+1]) - days_between[i]))
  }
  
  #ADDING TO OVERALL LIST OF DIFFERENCES IN DAYS
  total_distribution <- append(total_distribution, day_distribution)
}


#GETTING PROBABILITIES FOR EACH TLENGTH BETWEEN LOG INS
x <- c()
for (i in 1:(length(total_distribution))){
 x<-append(x,table(total_distribution)[[i]]/length(total_distribution))
}

#PLOTTING CONDITIONAL PROBABILITIES
ggplot() +
  geom_point(aes(y= x[2:34], x = as.numeric(names(table(total_distribution)))))+
  labs(
    title='P(logging in on given day | Days since last log in)  2019-09-10 : 2019-10-20
    n_teachers = _______; n_logins = ________',
          y = 'P(logging in',
          x = "Days since Last Log in")

#ZOOOMING IN DEPENDING ON SAMPEL DATE RANGE
ggplot() +
  geom_point(aes(y= x[2:34], x = as.numeric(names(table(total_distribution)))))+
  labs(
    title='P(logging in on given day | Days since last log in) 2019-09-10 : 2019-10-20',
    y = 'P(logging in)',
    x = "Days since Last Log in") +
  coord_cartesian(xlim = c(0,50))

############## TEST CASE FOR INDIVIDUAL TEACHERS TO MAKE SURE WORK WAS CORRECT ############

#filtering by teacher
# test <- teacher_usage %>% 
#   filter(Adult.User.ID == unique_ids[3]) %>% 
#   arrange(Usage.Time)
# 
# 
# day_distribution <- c()
# 
# for (i in 1:(length(days_between)-1)){
#   day_distribution <- append(day_distribution ,as.numeric((days_between[i+1]) - days_between[i]))
# }
# 
# table(total_distribution)
# 
# length
# 
# histogram(day_distribution)


########################################################
################ ## DV CORRELATIONS ################### 

#READING IN DATA
teacher_usage <- read.csv("raw/teacher_usage_time_series_2020-07-28T1651.csv")
classroom_student_usage <- read.csv("raw/classroom_student_usage_time_series_2020-07-28T1641.csv")
classroom_info <- read.csv("raw/classroom_info_2020-07-28T1640.csv")
classroom_teacher_lookup <- read.csv("raw/classroom_teacher_lookup_2020-07-28T1647.csv")
teacher_info <- read.csv("raw/teacher_info_2020-07-28T1650.csv")
school_info <- read.csv("raw/school_info_2020-07-28T1648.csv")


unique(classroom_student_usage$Classroom.ID)

##### teacher log ins per week
set_week_start("Monday") # set the default start of the week

#MAKING SURE THE DATES ARE IN THE RIGHT FORMAT
teacher_usage$Usage.Time <- as.Date(teacher_usage$Usage.Time)
classroom_student_usage$Usage.Week <- as.Date(classroom_student_usage$Usage.Week)


ggplot(classroom_student_usage) +
  geom_point(aes(x =as.numeric(Minutes.per.Active.User), y = Badges.per.Active.User)) +
  labs(title = "full Year",
       x = 'Average Minutes per student',
       y = 'Average Badges per student') +
  geom_abline(slope = .07, color = 'red')

#FILTERING FOR PREFERRED DATE RANGE
classroom_student_usage <- classroom_student_usage %>% 
  filter(Usage.Week >='2019-09-09') %>%  
  filter(Usage.Week <='2019-10-20') %>% 
  arrange(Usage.Week)

teacher_usage <- teacher_usage  %>% 
  filter(Usage.Time >='2019-09-09') %>%  
  filter(Usage.Time <='2019-10-20') %>% 
  arrange(Usage.Time)

length(unique(classroom_teacher_lookup$Classroom.ID))

length(unique(classroom_teacher_lookup$Teacher.User.ID))

##CREATING THE NEW MATCHING COLUMN FOR THE JOIN
classroom_student_usage$weeks <- as.character(date2week(classroom_student_usage$Usage.Week, floor_day = TRUE))
classroom_student_usage$week_TEST <- classroom_student_usage$Usage.Week
teacher_usage$weeks <- as.character(date2week(teacher_usage$Usage.Time,floor_day = TRUE) )
teacher_usage$week_TEST <- teacher_usage$Usage.Time

weekly_log_count <- teacher_usage %>% 
  group_by(weeks, Adult.User.ID) %>% 
  summarise(n = n())

classroom_teacher_lookup$Adult.User.ID <- classroom_teacher_lookup$Teacher.User.ID 
join1 <- full_join(weekly_log_count, classroom_teacher_lookup, by = 'Adult.User.ID')

length(unique(join1$Classroom.ID))


join2 <- left_join(join1,classroom_student_usage, by = c('weeks', "Classroom.ID") )

join2_edit <- join2 %>% 
  group_by(Classroom.ID) %>% 
  summarise(n = sum(n), 
            badges = sum(Badges.per.Active.User),
            mins = sum(as.numeric(Minutes.per.Active.User)))


ggplot(join2_edit)+
  geom_histogram(aes(x = mins), bins =50)+
  coord_cartesian(xlim=c(0,1500))
  
ggplot(join2_edit)+
  geom_histogram(aes(x = n),bins=200)+
  coord_cartesian(xlim=c(0,500))

ggplot(join2_edit)+
  geom_histogram(aes(x =badges), bins =50)+
  coord_cartesian(xlim=c(0,50))


table(join2_edit$n)

#many teachers are associated with many classrooms! - WHAT????
ggplot(join2_edit)+
  geom_point(aes(x = n, y = as.numeric(badges))) +
  coord_cartesian((xlim = c(0,200))) +
  labs(x = 'Log ins over experimental period',
       y = 'Average Badges per student \nover experimental period')

ggplot(join2_edit)+
  geom_point(aes(x = n, y = as.numeric(mins)))  +
  coord_cartesian((xlim = c(0,200))) +
  labs(x = 'Log ins over experimental period',
       y = 'Average Minutes per student \nover experimental period')

ggplot(join2_edit)+
  geom_point(aes(x = as.numeric(mins), y = as.numeric(badges))) +
  labs(x = 'Average Minutes per student \nover experimental period',
       y = 'Badges per student per week')

ggplot(classroom_student_usage)+
  geom_point(aes(x =as.numeric(Minutes.per.Active.User), y = Badges.per.Active.User)) +
  labs(title = "comprehensive sample",
       x = 'Average Minutes per student \nover experimental period',
       y = 'Average Badges per student \nover experimental period') +
  geom_abline(slope = .07, color = 'red')


#######################################################
######### CROSS TABS  ##########
#######################################################
setwd("~/Desktop/zearn")
total_data <- read.csv('2020-09_teacher_classroom_activity_with_stddevs.csv')

total_data <- total_data %>%
  filter(classroom_grade == 'G1' |
           classroom_grade == 'G2'|
           classroom_grade == 'G3'|
           classroom_grade == 'G4'|
           classroom_grade == "G5"|
           classroom_grade == "G6"|
           classroom_grade == "G7"|
           classroom_grade == "G8")

print(paste('Total starting teachers:', length(unique(total_data$teacher_id))))

total_data <- total_data %>%
  
  dplyr::select(-school_id)

table(total_data$classroom_grade)

total_data <- unique(total_data)

print(paste('Total starting teachers:', length(unique(total_data$teacher_id))))

total_data_classes <- total_data %>% 
  group_by(teacher_id,classroom_id) %>% 
  summarise(n=n())

length(unique(totaldata_classes$classroom_id))

data.frame()

class_table <- data.frame(table(total_data_classes$classroom_id))
class_table <- class_table %>% 
  filter(Freq == 1)

teacher_classes2_
total_data_case_1 <- total_data  
teacher_classes2_ <- class_table$Var1

total_data_case_1 <- total_data %>% 
  filter((classroom_id %in% teacher_classes2_))

print(paste('Case 1 teachers', length(unique(total_data_case_1$teacher_id))))

case2_copy <- total_data %>% 
  filter(!(classroom_id %in% teacher_classes2_)) %>% 
  select(teacher_id)

total_data_case_2 <- total_data

total_data_case_2 <- total_data_case_2 %>% 
  filter(!(teacher_id %in% case2_copy$teacher_id))

print(paste('Case 2 teachers',length(unique(total_data_case_2$teacher_id)) ))

teacher_table <- data.frame(table(total_data_classes$teacher_id))
teacher_table <- teacher_table %>% 
  filter(Freq == 1)

total_data_case_3 <- total_data  
teacher_classes3_ <- teacher_table$Var1
teacher_classes3_

total_data_case_3 <- total_data %>% 
  filter((classroom_id %in% teacher_classes2_))

total_data_case_3 <- total_data_case_3 %>% 
  filter((teacher_id %in% teacher_classes3_))
         
print(paste('Case 3 teachers', length(unique(total_data_copy$teacher_id))))

sum(teacher_classes2_ %in% teacher_classes3_)

# 
# class_table<- table(total_data_classes$classroom_id)
# teacher_classes2_ <- c()
# 
# for (i in 1:length(table(total_data_classes$classroom_id))){
#   if (class_table[[i]] > 1){
#     teacher_classes2_ <- append(teacher_classes2_, names(class_table[i]))
#     print(class_table[i])
#     print(i)
#   }
# }
# 
# teacher_table<- table(total_data_classes$teacher_id)
# teacher_table
# teachers2 <- c()
# for (i in 1:length(table(total_data_classes$teacher_id))){
#   if (teacher_table[[i]] > 1){
#     teachers2 <- append(teachers2, names(teacher_table[i]))
#   }
#   print(i)
# }
# 
# teachers2
# total_data_copy <- total_data  
# teacher_classes2_ <- as.numeric(teacher_classes2_)
# 
# total_data_copy <- total_data_copy %>% 
#   filter(!(classroom_id %in% teacher_classes2_))
# 
# length(unique(total_data_copy$teacher_id))
# 
# print(paste('Case 1 teachers', length(unique(total_data_copy$teacher_id))))
# 
# 
# indecies <- c()
# 
# length(total_data$teacher_id)
# 
# indecies <- which(total_data$classroom_id %in%teacher_classes2_)
# indecies
# 
# drop_teachers <- total_data$teacher_id[indecies]
# 
# total_data_copy2 <- total_data
# 
# total_data_copy2 <- total_data_copy2 %>% 
#   filter(!(teacher_id %in% drop_teachers))
# 
# 
# print(paste('Case 2 teachers',length(unique(total_data_copy2$teacher_id)) ))
# 
# ######
# 
# teachers2
# 
# indecies2 <- which(total_data$teacher_id %in%teachers2)
# indecies2 
# 
# 
# drop_teachers2 <- total_data$teacher_id[indecies2]
# 
# total_data_copy3 <- total_data
# 
# total_data_copy3 <- total_data_copy3 %>% 
#   filter(!(teacher_id %in% drop_teachers2)) %>% 
#   filter(!(teacher_id %in% drop_teachers))
# 
# print(paste('All teachers with > 1 class',length(unique(total_data_copy3$teacher_id)) ))
# 
# drop_teachers2
# 
########################################################################
################## CROSS TABS FOR EXTRA TEACHERS #######################
########################################################################

total_data_classes <- total_data %>% 
  dplyr::group_by(teacher_id,classroom_id) %>% 
  dplyr::summarise(n=n())

class_table <- data.frame(table(total_data_classes$classroom_id))
class_table <- class_table %>% 
  filter(Freq > 1)

total_data_copy <- total_data  
teacher_classes2_ <- class_table$Var1

#students_active_2020.09.14
total_data_copy <- total_data %>% 
  dplyr::filter((classroom_id %in% teacher_classes2_)) %>% 
  dplyr::group_by(teacher_id, classroom_id) %>% 
  dplyr::summarise(student_group_sum = sum(students_active_2020.09.14))



#HISTOGRAM FOR NUMBER OF STUDENTS PER CLASSROOM :)
ggplot(total_data_copy) +
  geom_histogram(aes(x= student_group_sum),bins = 100)+
  coord_cartesian(xlim=c(0,40))

total_data_copy <- total_data_copy %>% 
  mutate(student_groups = case_when(
    student_group_sum >= 0 & student_group_sum <= 4 ~'(0-4)',
    student_group_sum > 5 & student_group_sum <= 35 ~'(5-35)',
    student_group_sum > 36 & student_group_sum <= 150 ~'(36-150)',
    student_group_sum > 150 ~'(150+)')) %>% 
  drop_na()

table(total_data_copy$student_groups)


p <- function(v) {
  Reduce(f=paste0, x = v)
}

x<-total_data_copy %>% 
  group_by(classroom_id) %>% 
  summarise(student_groups_final = p(as.character(student_groups))) %>%
  merge(., total_data_copy, by = 'classroom_id')

y<-x[!duplicated(x$classroom_id), ]

z <- data.frame(table(y$student_groups_final))

names(teacher_info)[1] <- "Teacher.User.ID"
names(z)[1] <- "Num_kids_per_teacher_in_class"

write_csv(z,'cross_tabs.csv')
length(unique(total_data_copy$teacher_id))



#######################################################
######### CASE 1 VERSUS CASE 2 #########################
#######################################################

total_data_classes <- total_data %>% 
  group_by(teacher_id,classroom_id) %>% 
  summarise(n=n())

length(unique(total_data_classes$classroom_id))


class_table <- data.frame(table(total_data_classes$classroom_id))
class_table <- class_table %>% 
  filter(Freq == 1)

total_data_copy <- total_data  
teacher_classes2_ <- class_table$Var1

total_data_copy <- total_data %>% 
  filter((classroom_id %in% teacher_classes2_))

print(paste('Case 1 teachers', length(unique(total_data_copy$teacher_id))))

case2_copy <- total_data %>% 
  filter(!(classroom_id %in% teacher_classes2_)) %>% 
  select(teacher_id)

total_data_copy2 <- total_data

total_data_copy2 <- total_data_copy2 %>% 
  filter(!(teacher_id %in% case2_copy$teacher_id))

#differencee
case1_2_difference <- total_data_copy %>% 
  filter(!(total_data_copy$classroom_id %in% total_data_copy2$classroom_id))

case1_2_difference<- case1_2_difference%>% 
  filter((classroom_id %in% teacher_classes2_)) %>% 
  group_by(teacher_id, classroom_id) %>% 
  summarise(student_group_sum = sum(students_active_2020.09.14))

case1_2_difference <- case1_2_difference %>% 
  mutate(student_groups = case_when(
    student_group_sum >= 0 & student_group_sum <= 4 ~'(0-4)',
    student_group_sum > 5 & student_group_sum <= 35 ~'(5-35)',
    student_group_sum > 36 & student_group_sum <= 150 ~'(36-150)',
    student_group_sum > 150 ~'(150+)')) %>% drop_na()

length(unique(case1_2_difference$classroom_id))


p <- function(v) {
  Reduce(f=paste0, x = v)
}

x<-case1_2_difference %>% 
  group_by(classroom_id) %>% 
  summarise(student_groups_final = p(as.character(student_groups))) %>%
  merge(., total_data_copy, by = 'classroom_id')

y<-x[!duplicated(x$classroom_id), ]

z <- data.frame(table(y$student_groups_final))

names(teacher_info)[1] <- "Teacher.User.ID"
names(z)[1] <- "Num_kids_per_teacher_in_class"

table(z)

write_csv(z,'cross_tabs.csv')

z_final
length(unique(total_data_copy$teacher_id))


#######################################################
######### TEACHERS IN CASE CASE 1 2 DIFF, FROM CASE BASELINE TO 1##########
#######################################################

case_1_2_diff_1 <- total_data %>% 
  filter(teacher_id %in% case1_2_difference$teacher_id)


case_1_2_diff_1<-case_1_2_diff_1%>% 
  group_by(teacher_id, classroom_id) %>% 
  summarise(student_group_sum = sum(students_active_2020.09.14))

case_1_2_diff_1 <- case_1_2_diff_1 %>% 
  mutate(student_groups = case_when(
    student_group_sum >= 0 & student_group_sum <= 4 ~'(0-4)',
    student_group_sum > 5 & student_group_sum <= 35 ~'(5-35)',
    student_group_sum > 36 & student_group_sum <= 150 ~'(36-150)',
    student_group_sum > 150 ~'(150+)')) %>% drop_na()

length(unique(case_1_2_diff_1$classroom_id))

p <- function(v) {
  Reduce(f=paste0, x = v)
}

xx <- case_1_2_diff_1  %>% 
  group_by(classroom_id) %>% 
  summarise(student_groups_final = p(as.character(student_groups))) %>%
  merge(., total_data, by = 'classroom_id')

yy<-xx[!duplicated(xx$classroom_id), ]

zz <- data.frame(table(yy$student_groups_final))

zz$sum <- sum(zz$Freq)

names(zz)[1] <- "Num_kids_per_teacher_in_class"



#######################################################
######### TEST ANALYSIS WITH PREREGISTRATION 8/16/2021 ##########
#######################################################
  
setwd("~/Desktop/zearn")
total_data <- read.csv('2020-09_teacher_classroom_activity_with_stddevs.csv')

total_data <- total_data %>%
  filter(classroom_grade == 'G1' |
           classroom_grade == 'G2'|
           classroom_grade == 'G3'|
           classroom_grade == 'G4'|
           classroom_grade == "G5"|
           classroom_grade == "G6"|
           classroom_grade == "G7"|
           classroom_grade == "G8")

print(paste('Total starting teachers:', length(unique(total_data$teacher_id))))


teach_num <-total_data %>%
  group_by(classroom_id) %>%
  summarize(teachers = n_distinct(teacher_id))

total_data_case1 <- total_data %>%
  filter(classroom_id %in% subset(teach_num, teachers == 1)$classroom_id)

print(paste('case1:', length(unique(total_data_case1$teacher_id))))

#filtering columns that I need <3
total_data_case1_test <- total_data_case1 %>% 
  select(classroom_id, teacher_id, classroom_grade, total_associated_students,
         badges_per_active_2020.09.14, minutes_per_active_2020.09.14,
         badges_per_active_2020.09.21, minutes_per_active_2020.09.21,
         badges_per_active_2020.09.28, minutes_per_active_2020.09.28,
         badges_per_active_2020.10.05, minutes_per_active_2020.10.05,
         account_created_at, is_in_school_account, school_id)

total_data_case1_test$classroom_grade <- str_replace_all(total_data_case1_test$classroom_grade, "G","")

#putting 0s in to get badges
total_data_case1_test$badges_per_active_2020.09.14[is.na(total_data_case1_test$badges_per_active_2020.09.14)] <- 0
total_data_case1_test$badges_per_active_2020.09.21[is.na(total_data_case1_test$badges_per_active_2020.09.21)] <- 0
total_data_case1_test$badges_per_active_2020.09.28[is.na(total_data_case1_test$badges_per_active_2020.09.28)] <- 0
total_data_case1_test$badges_per_active_2020.10.05[is.na(total_data_case1_test$badges_per_active_2020.10.05)] <- 0

total_data_case1_test$total_badges <- as.numeric(total_data_case1_test$badges_per_active_2020.09.14) + as.numeric(total_data_case1_test$badges_per_active_2020.09.21) +
  as.numeric(total_data_case1_test$badges_per_active_2020.09.28) + as.numeric(total_data_case1_test$badges_per_active_2020.10.05)

#putting 0s in to get minutes
total_data_case1_test$minutes_per_active_2020.09.14[is.na(total_data_case1_test$minutes_per_active_2020.09.14)] <- 0
total_data_case1_test$minutes_per_active_2020.09.21[is.na(total_data_case1_test$minutes_per_active_2020.09.21)] <- 0
total_data_case1_test$minutes_per_active_2020.09.28[is.na(total_data_case1_test$minutes_per_active_2020.09.28)] <- 0
total_data_case1_test$minutes_per_active_2020.10.05[is.na(total_data_case1_test$minutes_per_active_2020.10.05)] <- 0

total_data_case1_test$total_minutes <- as.numeric(total_data_case1_test$minutes_per_active_2020.09.14) + as.numeric(total_data_case1_test$minutes_per_active_2020.09.21) +
  as.numeric(total_data_case1_test$minutes_per_active_2020.09.28) + as.numeric(total_data_case1_test$minutes_per_active_2020.10.05)



#days column
total_data_case1_test <- total_data_case1_test %>% 
  mutate(days_active = as.numeric((as.Date("2021-09-14") - as.Date(account_created_at)))) %>% 
  mutate(classroom_grade = as.numeric(classroom_grade)) 

#number of classrooms for each teacher/badges per teacher
num_class <- total_data_case1_test %>% 
  group_by(teacher_id) %>% 
  summarise(num_classes = n(),
            total_sum_badges = sum(total_badges),
            total_students = sum(total_associated_students),
            total_sum_minutes = sum(total_minutes))

total_data_case1_test <- left_join(total_data_case1_test, num_class, by = 'teacher_id')

#droppingmore columns
total_data_case1_test <- total_data_case1_test %>% 
  select(-c(badges_per_active_2020.09.14, minutes_per_active_2020.09.14,
            badges_per_active_2020.09.21, minutes_per_active_2020.09.21,
            badges_per_active_2020.09.28, minutes_per_active_2020.09.28,
            badges_per_active_2020.10.05, minutes_per_active_2020.10.05,
            account_created_at))

#gettinggrade eprcentage breakdowns
total_data_case1_test <- total_data_case1_test %>% 
  mutate(student_prop = total_associated_students/total_students)

grade_props <- total_data_case1_test %>% 
  select(teacher_id, classroom_grade, total_students,total_associated_students,student_prop)


grade_props <- grade_props %>%
  pivot_wider(names_from = classroom_grade, values_from = student_prop, values_fn = mean)
#fixing columns
grade_props$G1 <- grade_props$`1`
grade_props$G2 <- grade_props$`2`
grade_props$G3 <- grade_props$`3`
grade_props$G4 <- grade_props$`4`
grade_props$G5 <- grade_props$`5`
grade_props$G6 <- grade_props$`6`
grade_props$G7 <- grade_props$`7`
grade_props$G8 <- grade_props$`8`

#putting 0 in for NA
grade_props$G1[is.na(grade_props$G1)] <- 0
grade_props$G2[is.na(grade_props$G2)] <- 0
grade_props$G3[is.na(grade_props$G3)] <- 0
grade_props$G4[is.na(grade_props$G4)] <- 0
grade_props$G5[is.na(grade_props$G5)] <- 0
grade_props$G6[is.na(grade_props$G6)] <- 0
grade_props$G7[is.na(grade_props$G7)] <- 0
grade_props$G8[is.na(grade_props$G8)] <- 0


#grouping to get breakdown
grade_props <- grade_props %>% 
  group_by(teacher_id) %>% 
  summarise(G8prop = sum(G8),G7prop = sum(G7),G6prop = sum(G6),
            G5prop = sum(G5),G4prop = sum(G4),G3prop = sum(G3),G2prop = sum(G2),G1prop = sum(G1))

total_data_case1_test <- left_join(total_data_case1_test, grade_props, by = 'teacher_id')

#getting treatment randoms
treatment <- round(runif(length(total_data_case1_test$total_sum_badges), min=0, max=1))
total_data_case1_test$treatment <- treatment

#dropping more than 150?
total_data_case1_test <- total_data_case1_test %>% 
  filter(total_students <150)


#getting rid of repeat teacher ID - DO AT LAST MINUTE
total_data_case1_test <- total_data_case1_test[!duplicated(total_data_case1_test$teacher_id), ]

#ALSO DOING LAST MINUTE TO HELP!

#unique school ID - fewer than 2?
school_id_table <- data.frame(table(total_data_case1_test$school_id))
school_id_table <- school_id_table %>% 
  filter(Freq == 1)

names(school_id_table)[1] = 'school_id'

#unknown or less than school
total_data_case1_test <- total_data_case1_test %>% 
  mutate(school_id = ifelse(
    school_id %in% school_id_table$school_id, 3,school_id))

#regression
# DONE Percentage of the teacher’s students at each grade level
# DONEA n indicator for whether the teacher’s account is a free account
# CANT An indicator for whether the teacher logged into Zearn at least once between August 1, 2021 and September 14, 2021 (inclusive)
# DONE The number of the teacher’s students 
# DONE The number of the teacher’s classrooms 
# CANT The number of days the teacher has had a Zearn account before September 14, 2021
# CANT The number of days between the start of the school year and September 14, 2021
# CANT Baseline data (i.e., from the start of the 2021-2022 school year to September 14, 2021) on the dependent variable
# CANT An indicator for missing baseline data 
# DONE Fixed effect for school omitting the category of “unknown school” (and if a school has fewer than two teachers in the study, that school will be put into the “unknown school” category)
# CANT Indicator for whether the teacher opened the first of two studywide emails sent immediately prior to the intervention period 
# CANT Indicator for whether the teacher opened the second of two studywide emails sent immediately prior to the intervention period 



#regression
fit_badges <- felm(total_sum_badges ~ total_students + is_in_school_account + days_active +
               num_classes + total_students + treatment + 
               G1prop + G2prop+ G3prop + G4prop + G5prop +G6prop + G7prop + G8prop | school_id ,weights = total_data_case1_test$total_students, data = total_data_case1_test)

summary(fit_badges)

fit_minutes <- felm(total_sum_minutes ~ total_students + is_in_school_account + days_active +
                     num_classes + total_students + treatment + 
                     G1prop + G2prop+ G3prop + G4prop + G5prop +G6prop + G7prop + G8prop | school_id ,weights = total_data_case1_test$total_students, data = total_data_case1_test)

summary(fit_minutes)
             
#fit_noreg <- felm(turnout_total ~ treatment | town_id | 0 , data = test_data)

library(psych)

cor(total_data_case1_test$total_sum_minutes,total_data_case1_test$total_sum_badges)

#dv plots
ggplot(total_data_case1_test)+ 
  geom_point(aes(x = total_sum_minutes, y = total_sum_badges))

#histogram of total student
ggplot(total_data_case1_test_cut)+
 geom_histogram(aes(total_students),bins=1000)+
  coord_cartesian(xlim=c(0,75))+
  labs(x='students per teacher')


#######################################################
######### LOUISIANA REGRESSION ##########
#######################################################


setwd("~/Desktop/zearn")
teacher_usage <- read.csv("raw2/Teacher Usage - Time Series 2020-10-09T1635.csv")
classroom_student_usage <- read.csv("raw2/Classroom Student Usage - Time Series 2020-10-09T1616.csv")
classroom_info <- read.csv("raw2/Classroom Info 2020-10-09T1617.csv")
classroom_teacher_lookup <- read.csv("raw2/Classroom-Teacher Lookup 2020-10-09T1618.csv")
school_info <- read.csv("raw2/School Info 2020-10-09T1619.csv")
la_usage_types <- read_csv("raw2/la_usage_types.csv")


### TEACHERS
# Merging with the classroom info for further merge with teacher info
classroom_student_usage_subset <- merge(classroom_student_usage, classroom_teacher_lookup,
                                        by = c("Classroom.ID"))
classroom_student_usage_subset$week = lubridate::week(classroom_student_usage_subset$Usage.Week)
classroom_student_usage_subset$year = lubridate::year(classroom_student_usage_subset$Usage.Week)

# Noting week, month, and year to summarize teacher behavior: 
teacher_usage$week = lubridate::week(teacher_usage$Usage.Time)
teacher_usage$year = lubridate::year(teacher_usage$Usage.Time)
teacher_usage$wday = lubridate::wday(teacher_usage$Usage.Time)
teacher_usage$hour = lubridate::hour(teacher_usage$Usage.Time)
teacher_usage$whour = (teacher_usage$wday - 1)*24 + teacher_usage$hour


# Find total behavior per week per teacher
teacher_usage$Event.Type[teacher_usage$Event.Type == "Resource Downloaded"] <- paste("RD.",teacher_usage$Curriculum.Resource.Category[teacher_usage$Event.Type == "Resource Downloaded"])
teacher_usage_total <- teacher_usage %>% 
  group_by(Adult.User.ID,Event.Type,week,year) %>% 
  summarise(Freq=n()) %>% 
  pivot_wider(id_cols = c(Adult.User.ID,week,year), names_from = Event.Type, values_from = Freq)
# Replace NAs with 0
teacher_usage_total[is.na(teacher_usage_total)] <- 0

# Merge teacher and student data
teacher_student_usage_subset <- merge(classroom_student_usage_subset, teacher_usage_total, 
                                      by.x = c("Teacher.User.ID","week","year"), by.y = c("Adult.User.ID","week","year"))

# Adding the number of classrooms each teacher has
T <- data.frame(table(classroom_teacher_lookup$Teacher.User.ID))
T <- rename(.data = T, teacher.number.classes = Freq)
teacher_student_usage_subset <- merge(teacher_student_usage_subset, T, 
                                      by.x = c("Teacher.User.ID"), by.y = c("Var1"))
#grade.levels <- classroom_info[classroom_info$Students...Total > 5,]
# Making sure we have only one classroom ID per class
#grade.levels <- aggregate(Grade.Level ~ Classroom.ID, data=grade.levels, FUN = mean)
# Taking away conflicting grade levels
#grade.levels <- grade.levels[grade.levels$Grade.Level != 3.5,]
#grade.levels <- grade.levels[grade.levels$Grade.Level != 4.5,]

teacher_student_usage_subset <- merge(teacher_student_usage_subset, classroom_info, 
                                      by = c("Classroom.ID"))
teacher_student_usage_subset$Grade.Level <- factor(teacher_student_usage_subset$Grade.Level, ordered = TRUE)


la_usage_types$curriculum <- 0
la_usage_types$curriculum[la_usage_types$`Schools and Districts Usage Type` == "20-21 Curriculum"] <- 1
la_usage_types$curriculum[la_usage_types$`Schools and Districts Usage Type` == "Core Complement"] <- 1

teacher_student_usage_subset <- merge(teacher_student_usage_subset, la_usage_types, 
                                      by.x = c("MDR.School.ID"), by.y = c("Schools and Districts MDR School ID"),
                                      all.x = TRUE)
teacher_student_usage_subset <- teacher_student_usage_subset[-44]

teacher_student_usage_subset <- merge(teacher_student_usage_subset, school_info[c(1,7)], 
                                      by = c("MDR.School.ID"),
                                      all.x = TRUE)
teacher_student_usage_subset$Demographics...Zipcode.Median.Income <- factor(teacher_student_usage_subset$Demographics...Zipcode.Median.Income, ordered = TRUE)

###################################################
### STUDENTS
## Remove classes inactive for more than 7 months
# Remove June/July/August
# teacher_student_usage_subset$month = lubridate::month(teacher_student_usage_subset$Usage.Week)
# teacher_student_usage_subset <- subset(teacher_student_usage_subset, month != 6 & month != 7 & month != 8)
# teacher_student_usage_subset <- teacher_student_usage_subset[-46]
# Amount of data for each classroom
M <- table(teacher_student_usage_subset$Classroom.ID)
M <- data.frame(M)

active_classrooms <- M[M$Freq > 20,]$Var1
# Active for more than 5 months
teacher_student_usage_subset <- teacher_student_usage_subset[teacher_student_usage_subset$Classroom.ID %in% active_classrooms, ]
## We still have a lot of weird outliers. Let's look at the mean active students throughout the year.
average_student = aggregate(Active.Users...Total ~ Classroom.ID, data=teacher_student_usage_subset, FUN = mean)
# More than 5 active students on average:
teacher_student_usage_subset <- teacher_student_usage_subset[teacher_student_usage_subset$Classroom.ID %in% average_student$Classroom.ID[average_student$Active.Users...Total > 5],]
# Remove duplicate (classroom, week) pairs // I.E., remove classrooms with more than one teacher
duplicates <- teacher_student_usage_subset
duplicates$unique_id <- paste(duplicates$Classroom.ID,duplicates$Usage.Week) # concatenate to make unique ID
duplicates$duplicate = duplicated(duplicates$unique_id) # generate the duplicate variable
teacher_student_usage_subset <- subset(duplicates, duplicate=="FALSE") # delete the duplicates
# Replace NAs with 0
teacher_student_usage_subset[is.na(teacher_student_usage_subset$Badges.per.Active.User),]$Badges.per.Active.User <- 0
# Remove small classrooms
teacher_student_usage_subset <- subset(teacher_student_usage_subset, Students...Total>5) 


#STARTING BREAKDOWN FOR REGRESSION
LA_regression <- teacher_student_usage_subset %>% 
  select(MDR.School.ID,Classroom.ID, Teacher.User.ID, Usage.Week,
         Active.Users...Total, Minutes.per.Active.User, Badges.per.Active.User,
         teacher.number.classes, Grade.Level,Students...Total)

#grade levels
LA_regression <- LA_regression %>%
  filter(Grade.Level > 0)

#Forcing case 1 for teachers
print(paste('Total starting teachers:', length(unique(LA_regression$Teacher.User.ID))))

teach_num <- LA_regression %>%
  group_by(Classroom.ID) %>%
  summarize(teachers = n_distinct(Teacher.User.ID))

LA_regression <-  LA_regression %>%
  filter(Classroom.ID %in% subset(teach_num, teachers == 1)$Classroom.ID)

print(paste('Total starting teachers:', length(unique(LA_regression$Teacher.User.ID))))

#putting 0s in to get badges/minutes
LA_regression$Badges.per.Active.User[is.na(LA_regression$Badges.per.Active.User)] <- 0
LA_regression$Minutes.per.Active.User <- as.numeric(LA_regression$Minutes.per.Active.User)
LA_regression$Minutes.per.Active.User[is.na(LA_regression$Minutes.per.Active.User)] <- 0

#filtering some dates
LA_regression_subset <- LA_regression %>% 
  filter(Usage.Week >='2019-07-29') %>%  
  filter(Usage.Week <='2019-10-14') %>% 
  arrange(Usage.Week)

#getting bages, total students, total minutes
LA_sum_df <- LA_regression_subset %>% 
  group_by(Teacher.User.ID, Usage.Week) %>% 
  summarise(total_week_badges = sum(Badges.per.Active.User),
            total_week_minutes = sum(as.numeric(Minutes.per.Active.User)),
            total_students = sum(Students...Total))

LA_regression_subset <- left_join(LA_regression_subset, LA_sum_df, by =c('Teacher.User.ID', 'Usage.Week'))

#getting the preemptive covariates
LA_regression_baseline <- LA_regression_subset  %>% 
  filter(Usage.Week >='2019-07-29') %>%  
  filter(Usage.Week <='2019-09-09') %>% 
  arrange(Usage.Week)

LA_regression_baseline <- LA_regression_baseline %>% 
  group_by(Teacher.User.ID) %>% 
  summarise(baseline_badges = sum(Badges.per.Active.User),
            baseline_minutes = sum(Minutes.per.Active.User))

LA_regression_subset <- left_join(LA_regression_subset,LA_regression_baseline, by ='Teacher.User.ID')

#getting the DV of minutes and badges
LA_regression_dv <- LA_regression_subset  %>% 
  filter(Usage.Week >='2019-09-16') %>%  
  arrange(Usage.Week)

LA_regression_dv <- LA_regression_dv %>% 
  group_by(Teacher.User.ID) %>% 
  summarise(total_sum_badges = sum(Badges.per.Active.User),
            total_sum_minutes = sum(Minutes.per.Active.User))

LA_regression_subset <- left_join(LA_regression_subset,LA_regression_dv, by ='Teacher.User.ID')

###indicating missing baseline (both baseline minutes and badges not there)
LA_regression_subset <- LA_regression_subset %>% 
  mutate(missing_baseline = case_when(
    baseline_badges + baseline_minutes == 0 ~ 1,
    baseline_badges + baseline_minutes > 0 ~ 0
  ))

#random start date
start_dates <- sample(0:2, length(LA_regression_subset$Teacher.User.ID), replace = TRUE)

LA_regression_subset$start_dates <-start_dates

LA_regression_subset <- LA_regression_subset %>% 
  mutate(start_dates = case_when(
    start_dates == 2 ~ 29,
    start_dates == 1 ~ 36,
    start_dates == 0 ~ 43,
  ))




#gettinggrade eprcentage breakdowns
# LA_regression_subset <- LA_regression_subset%>% 
#   mutate(student_prop = Students...Total /total_students)

grade_props <- LA_regression_subset %>% 
  select(Teacher.User.ID, Grade.Level, total_students,Students...Total) %>% 
  group_by(Teacher.User.ID,Grade.Level) %>% 
  summarise(total_students = mean(total_students),
            Students...Total = mean(Students...Total)) %>% 
  mutate(student_prop = Students...Total/total_students)

grade_props <- grade_props %>%
  pivot_wider(names_from = Grade.Level, values_from = student_prop, values_fn = mean)


#fixing columns
grade_props$G1 <- grade_props$`1`
grade_props$G2 <- grade_props$`2`
grade_props$G3 <- grade_props$`3`
grade_props$G4 <- grade_props$`4`
grade_props$G5 <- grade_props$`5`
grade_props$G6 <- grade_props$`6`
grade_props$G7 <- grade_props$`7`
grade_props$G8 <- grade_props$`8`


#putting 0 in for NA
grade_props$G1[is.na(grade_props$G1)] <- 0
grade_props$G2[is.na(grade_props$G2)] <- 0
grade_props$G3[is.na(grade_props$G3)] <- 0
grade_props$G4[is.na(grade_props$G4)] <- 0
grade_props$G5[is.na(grade_props$G5)] <- 0
grade_props$G6[is.na(grade_props$G6)] <- 0
grade_props$G7[is.na(grade_props$G7)] <- 0
grade_props$G8[is.na(grade_props$G8)] <- 0


#grouping to get breakdown
grade_props <- grade_props %>% 
  group_by(Teacher.User.ID) %>% 
  summarise(G8prop = sum(G8),G7prop = sum(G7),G6prop = sum(G6),
            G5prop = sum(G5),G4prop = sum(G4),G3prop = sum(G3),G2prop = sum(G2),G1prop = sum(G1))

grade_props <- grade_props %>%  
  mutate(prop_total = G1prop + G2prop+ G3prop+ G4prop+ G5prop
         + G6prop + G7prop+ G8prop)

LA_regression_subset <- left_join(LA_regression_subset, grade_props, by = 'Teacher.User.ID')


LA_regression_subset <- LA_regression_subset %>%  
  mutate(prop_total = G1prop + G2prop+ G3prop+ G4prop+ G5prop
         + G6prop + G7prop+ G8prop)


#getting treatment randoms
treatment <- round(runif(length(LA_regression_subset$total_sum_badges), min=0, max=1))
LA_regression_subset $treatment <- treatment

#dropping more than 150?
LA_regression_subset  <- LA_regression_subset  %>% 
  filter(total_students <150)


LA_regression_subset<- LA_regression_subset[!duplicated(LA_regression_subset$Teacher.User.ID), ]


#unique school ID - fewer than 2?
school_id_table <- LA_regression_subset 
school_id_table <- data.frame(table(LA_regression_subset$MDR.School.ID))
school_id_table <- school_id_table %>% 
  filter(Freq == 1)

names(school_id_table)[1] = 'MDR.School.ID'

#unknown or less than school - just
LA_regression_subset <- LA_regression_subset %>% 
  mutate(MDR.School.ID = ifelse(
    MDR.School.ID %in% school_id_table$MDR.School.ID, 3,MDR.School.ID))

LA_regression_subset <- LA_regression_subset %>% 
  filter(prop_total <= 1)

library(lfe)
#regression!!
fit_badges <- felm(total_sum_badges ~ total_students + teacher.number.classes + start_dates + 
                     missing_baseline + baseline_badges + treatment +
                     G1prop + G2prop+ G3prop + G4prop + G5prop + G6prop + G7prop + G8prop | MDR.School.ID ,weights = LA_regression_subset$total_students, data = LA_regression_subset)
#n = 2286  teachers
summary(fit_badges)

fit_minutes <- felm(total_sum_minutes ~ total_students + teacher.number.classes + start_dates + 
                      missing_baseline + baseline_minutes + treatment +
                      G1prop + G2prop+ G3prop + G4prop + G5prop +G6prop + G7prop + G8prop | MDR.School.ID ,weights = LA_regression_subset$total_students, data = LA_regression_subset)

summary(fit_minutes)
table(teacher_student_usage_subset$Grade.Level)
#dv plots
ggplot(LA_regression_subset)+ 
  geom_point(aes(x = total_sum_minutes, y = total_sum_badges))
library(psych)

corel <- LA_regression_subset %>% drop_na()
cor(corel$total_sum_minutes, corel$total_sum_badges) # 0.8833854

#regression
#  DONEP ercentage of the teacher’s students at each grade level
# CANT An indicator for whether the teacher’s account is a free account
# CANT An indicator for whether the teacher logged into Zearn at least once between August 1, 2021 and September 14, 2021 (inclusive)
# DONEThe number of the teacher’s students 
# DONE The number of the teacher’s classrooms 
# CANT The number of days the teacher has had a Zearn account before September 14, 2021
# DONE The number of days between the start of the school year and September 14, 2021
# DONE Baseline data (i.e., from the start of the 2021-2022 school year to September 14, 2021) on the dependent variable
# DONE An indicator for missing baseline data 
# DONEFixed effect for school omitting the category of “unknown school” (and if a school has fewer than two teachers in the study, that school will be put into the “unknown school” category)
# CANT Indicator for whether the teacher opened the first of two studywide emails sent immediately prior to the intervention period 
# CANT Indicator for whether the teacher opened the second of two studywide emails sent immediately prior to the intervention period 
# DONE treatment!
# DONEfilter fewer than 150
# DONE get rid of doubles