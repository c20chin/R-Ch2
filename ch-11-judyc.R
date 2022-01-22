# Chapter 11 Iteration
library(tidyverse)

# ====================
# Exercise 11.1
# ==================== 
exams.list <- list(
  class1 = round(runif(30, 0, 100)), # 30 tokens of random numbers in the range <0, 100>
  class2 = round(runif(30, 0, 100)),
  class3 = round(runif(30, 0, 100)),
  class4 = round(runif(30, 0, 100)),
  class5 = round(runif(30, 0, 100))
)

map_dbl(exams.list, median)

map_dbl(exams.list, sd)
# ====================
# END
# ====================


# ====================
# Exercise 11.2
# ==================== 
me <- map_df(exams.list, median)
sd <- map_df(exams.list, sd)

exam_df <- rbind(me,sd)
# ====================
# END
# ====================



# ====================
# Exercise 11.3
# ==================== 

##################################
## setting the data, don't move ##
con <- file(description = "demo_data/dict-ch-idiom.txt",
            encoding = "utf-8")
texts <- readLines(con)
close(con)
idiom <- tibble(string = texts)
##################################


idiom_new <- idiom %>%
  filter(str_detect(string, ".*(.).*\\1.*")) %>%
  mutate(duplicate = TRUE) %>%
  mutate(duplicate_num = ifelse(str_detect(string, "(.)\\1(.)\\2"), 2, 1))
idiom_new

# ====================
# END
# ====================


# ====================
# Exercise 11.4
# ==================== 
str_match(idiom_new$string, ".*(.).*\\1.*")

idiom_word <- idiom_new %>%
  mutate(duplicate_char = ifelse(idiom_new$duplicate_num == 2, 
                          str_replace(string, "(.)\\1(.)\\2", "\\1_\\2"), 
                          str_replace(string, ".*(.).*\\1.*", "\\1")))
  
idiom_word
# ====================
# END
# ====================



# ====================
# Exercise 11.5
# ==================== 
library(viridis)

idiom_pivot <- idiom_word %>%
  group_by(duplicate_char) %>%
  summarise(count=n())

idiom_pivot <- idiom_pivot[order(idiom_pivot$count, decreasing = TRUE),]

idiom_gram <- idiom_pivot[1:20,]
idiom_gram

ggplot(idiom_gram) +
  geom_bar(aes(x = reorder(duplicate_char,count), y = count, fill=count),
           position = "dodge",
           stat = "identity") +
  coord_flip() +
  scale_fill_viridis() +  
  theme(text = element_text(family = "Heiti TC Light")) +
  xlab("Most duplicated character")
# ====================
# END
# ====================


# ====================
# Exercise 11.6
# ==================== 
map_df(mtcars, class)
#?mtcars

# ====================
# END
# ====================



# ====================
# Exercise 11.7
# ==================== 

y <- c(1, 4, 6, 10, 20)
my_z <- function(x) {
  (x - mean(x))/ sd(x)
}
my_z(y)
# ====================
# END
# ====================



# ====================
# Exercise 11.8
# ==================== 
map_df(exams.list, my_z)

# ====================
# END
# ====================
