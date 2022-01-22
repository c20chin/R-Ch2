# Chapter 8 Data Manipulation

library(dplyr)
library(tidyr)
library(ggplot2)

# ====================
# Exercise 8.1
# ====================
# When joining columns from y to x:

# inner_join()
# join all rows in x and y

# left_join()
# only join the rows included in x

# right_join()
# only joins the rows included in y

# full_join()
# join all rows included in x OR y

# anti_join()
# return all rows from x without matches in y
# ====================
# END
# ====================



# ====================
# Exercise 8.2
# ====================
# It is the manifestation of a Venn diagram:
# Let A = "math < 40" and B = "reading < 40",
# 1. Filter with the "&" would represent A ∩ B : 18 rows
# 2. Filter with the "|" would represent A ∪ B : 48 rows
# 3. Filter with the "xor" would represent A ^ B
# ( A or else B) : 30 rows 
# (A ∪ B) - (A ∩ B) = A ^ B
#  48 - 18 = 30 
# ====================
# END
# ====================



# ====================
# Exercise 8.3
# ====================
library(readr)
setwd("~/Working_Directory")
student <- read_csv("demo_data/data-students-performance.csv")
rename(student, 
       race = `race/ethnicity`,
       parent_edu = `parental level of education`,
       prep_course = `test preparation course`,
       math = `math score`,
       reading = `reading score`,
       writing = `writing score`) -> student1

student1 %>%
  count(parent_edu, gender) %>%
  group_by(parent_edu) %>% 
  mutate(percentage =paste0(round(n/sum(n),2)))

# ====================
# END
# ====================



# ====================
# Exercise 8.4
# ====================
# tidyr::separate(): turns a single character column into multiple columns.

# tidyr::unite(): to paste together multiple columns into one.

# ====================
# END
# ====================



# ====================
# Exercise 8.5
# ====================
student1 %>%
  select(gender,math) %>%
  filter(gender == "female" & math < 40)
# ====================
# END
# ====================



# ====================
# Exercise 8.6
# ====================
student1 %>%
  group_by(race) %>%
  summarise(
    math_mean = round(mean(math), 5),
    math_sd = round(sd(math), 5),
    N=n()
  ) %>%
  as.data.frame()
# ====================
# END
# ====================



# ====================
# Exercise 8.7
# ====================
stu3 = student1 %>%
  group_by(gender, parent_edu) %>%
  summarise(
    N=n(),
    math_mean = round(mean(math), 5),
    math_sd = round(sd(math), 5)
  ) %>%
  as.data.frame()
stu3
# ====================
# END
# ====================



# ====================
# Exercise 8.8
# ====================
stu3$parent_edu <-
  ordered(stu3$parent_edu, levels = c("some high school",
                                      "high school", 
                                      "some college", 
                                      "associate's degree", 
                                      "bachelor's degree", 
                                      "master's degree") )

order_new <- stu3[order(stu3[,1], stu3[,2]),]
order_new
# ====================
# END
# ====================



# ====================
# Exercise 8.9
# ====================
##PLOT 1##
order_new <-
  order_new %>%
  group_by(gender, parent_edu) %>%
  mutate(CI = 2*.9*(math_sd/((N)^(1/2))))

ggplot(order_new, aes(x=parent_edu, y=math_mean, group=gender, color=gender)) +
  geom_point(size=3) +
  geom_line(size=1.5) +
  geom_errorbar(aes(ymin= math_mean - CI, ymax= math_mean + CI, width=0.2)) +
  xlab("Parental Education Level") +
  ylab("Math Scores Means and CIs") +
  theme(axis.text.x = element_text(angle=90))



##PLOT 2##
plot2 <- student1 %>%
  select(1,5,6) %>%
  group_by(gender, prep_course) %>%
  summarise(
    N=n(),
    math_mean = round(mean(math), 5),
    math_sd = round(sd(math), 5)
  ) %>%
  mutate(CI = 2*.9*(math_sd/((N)^(1/2)))) %>%
  as.data.frame()

plot2$gender <- as.factor(plot2$gender)

ggplot(plot2, aes(x=prep_course, y=math_mean, fill=gender)) +
  geom_col(position = position_dodge(), color = "white", width = .5) +
  geom_errorbar(aes(ymin= math_mean - CI, ymax= math_mean + CI, width=0.1), 
                position = position_dodge(width = .5),
                color = "grey42") +
  xlab("Preparation Course") +
  ylab("Math Scores Means and CIs") +
  scale_fill_manual(values = c("hotpink1", "dodgerblue2"))



##PLOT 3##

#data preprocess + pivoting
plot3 <- student1 %>%
  select(1,5,6,7,8) %>%
  group_by(gender, prep_course) %>%
  summarise(
    N=n(),
    math_= round(mean(math), 5),
    math_sd = round(sd(math), 5),
    reading_ = round(mean(reading), 5),
    reading_sd = round(sd(reading), 5),
    writing_= round(mean(writing), 5),
    writing_sd = round(sd(writing), 5),
  ) %>%
    pivot_longer(cols=c("math_","writing_","reading_"),
               names_to = "subject",
              values_to = "mean") %>%
    pivot_longer(cols = c("math_sd", "reading_sd", "writing_sd"),
              names_to = "subsd",
              values_to = "sd") %>%
  subset((subject == "math_" & subsd == "math_sd") | 
           (subject == "reading_" & subsd == "reading_sd") |
           (subject == "writing_" & subsd == "writing_sd") )

#rename subject column
plot3$subject <- as.factor(plot3$subject)
fct_recode(plot3$subject, math="math_", reading = "reading_", writing="writing_" )
levels(plot3$subject) <- c("math", "reading", "writing")

#add in CI data
plot3 <- plot3 %>%
  mutate(CI = 2*.9*(sd/((N)^(1/2)))) %>%
  as.data.frame()

#plot
ggplot(plot3, aes(x=prep_course, y=mean, fill=gender)) +
  facet_grid(. ~ subject) +
  geom_col(stat= "identity", position = position_dodge(), color = "white", width = .5) +
  geom_errorbar(aes(ymin= mean - CI, ymax= mean + CI, width=0.1), 
                position = position_dodge(width = .5),
                color = "grey42") +
  xlab("Preparation Course") +
  ylab("Math Scores Means and CIs") +
  scale_fill_manual(values = c("hotpink1", "dodgerblue2"))




##PLOT 4##
student1$parent_edu <-
  ordered(student1$parent_edu, levels = c("some high school",
                                      "high school", 
                                      "some college", 
                                      "associate's degree", 
                                      "bachelor's degree", 
                                      "master's degree") )
plot4 <- student1 %>%
  group_by(gender, parent_edu)
  
ggplot(plot4, aes(x=parent_edu, y=math, fill=gender)) +
  geom_boxplot(outlier.size = 1, notch = FALSE, color = "grey42") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_fill_manual(values = c("hotpink1", "dodgerblue2")) +
  xlab("Parental Education Level") +
  ylab("Math Scores Distribution")

# ====================
# END
# ====================



# ====================
# Exercise 8.10
# ====================
require(readr)
word_freq <- read_csv("demo_data/data-word-freq.csv")
word_freq <- word_freq %>%
  pivot_wider(names_from = "CORPUS", 
               values_from = "FREQ")
word_freq
# ====================
# END
# ====================


