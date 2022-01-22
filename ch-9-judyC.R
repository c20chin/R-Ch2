# Chapter 9 String Manipulation
library(tidyverse)
library(tibble)

# ====================
# Exercise 9.1
# ====================
x <- fruit
str_view(x, "(.)(.)\\1\\2")
# ====================
# END
# ====================



# ====================
# Exercise 9.2
# ====================
str_view(x, "(.)(.)\\2\\1")
# ====================
# END
# ====================



# ====================
# Exercise 9.3
# ====================
str_view(x, "^(.).*\\1")
# ====================
# END
# ====================



# ====================
# Exercise 9.4
# ====================
str_match(x, "c([aeiou])")
# ====================
# END
# ====================



# ====================
# Exercise 9.5
# ====================
str_match(x, "(^[^aeiou])(.*)(\\1)")
# ====================
# END
# ====================



# ====================
# Exercise 9.6
# ====================
American.dates <- c("7/31/1976", "02.15.1970", "11-31-1986", "04/01.2020")
str_replace_all(string = American.dates, pattern = "(\\d+)([:punct:])(\\d+)", replacement = "\\3\\2\\1" )
# ====================
# END
# ====================


# ====================
# Exercise 9.7
# ====================
# alternative answer(just for my record)
# x = str_extract(string = sentences, pattern = "(be|am|are|is|was|were|been)\\s{1}([a-z]+(ed|en))\\b")
# Filter(Negate(is.na), x)

x <- str_extract_all(string = sentences, pattern = "(is|am|are|was|were|be|been)\\s\\w*(en|ed)\\b")
new <- unlist(x)
new
# ====================
# END
# ====================


# ====================
# Exercise 9.8
# ====================
x <- c("It's a three-legged char.", "The book (you read) was quite boring!")
str_split(str_replace_all(x, "[.()!]",""), pattern = "\\s")
# ====================
# END
# ====================


# ====================
# Exercise 9.9
# ====================
str_subset(sentences, ".+[-].+")
# ====================
# END
# ====================


# ====================
# Exercise 9.10
# ====================
x <- "中央(Nc)　流行(VH)　疫情(Na)　指揮(VC)　中心(Nc)　醫療(VC)　應變組(Nc)　副組長(Na)　羅一鈞(Nb)　今天(Nd)　說明(VE)　，(COMMACATEGORY)　截至(P)　12月(Nd)　1日(Nd)　全球(Nc)　累計(VJ)　至少(Da)　27(Neu)　國(Nc)　、(PAUSECATEGORY)　共有(VJ)　370(Neu)　例(Na)　確診(VA)　感染(VJ)　Omicron(FW)　變異株(Na)　，(COMMACATEGORY)　多(D)　來自(VJ)　南非(Nc)　或(Caa)　具(VJ)　非洲(Nc)　國家(Na)　旅遊史(Na)　。(PERIODCATEGORY)"
str_extract_all(x, "(?<=\\s|^).{1,4}(\\()N.+?\\)")

# ====================
# END
# ====================


# ====================
# Exercise 9.11
# ====================
str_replace_all(x, "\\(.+?\\)", "")
# ====================
# END
# ====================


# ====================
# Exercise 9.12
# ====================
x <- fruit[1:10]
str_replace_all(x, "a(?=(p|t|k|b|d|g))", "V")
# ====================
# END
# ====================


# ====================
# Exercise 9.13
# ====================
str_replace_all(x, "(?<=(p|t|k|b|d|g))[aeiou](?=(p|t|k|b|d|g))", "V")
# ====================
# END
# ====================


# ====================
# Exercise 9.14
# ====================
# separate(): Given either a regular expression or a vector of character positions, 
# it turns a single character column into multiple columns.
df <- data.frame(x = c(NA, "x.y", "x.z", "y.z"))
df %>% separate(x, c("A", "B"))

# extract(): Given a regular expression with capturing groups, extract() turns each group into a new column. If the groups don't match
# , or the input is NA, the output will be NA.
df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")

# unnest(): If you have a list-column, this makes each element of the list its own row.
df <- tibble(
  x = 1:2,
  y = list(
    tibble(z = 1),
    tibble(z = 3:4)
  )
)
df %>% unnest(y)
# ====================
# END
# ====================



# ====================
# Case 1
# ====================
dt <- tibble(
  x = 1:4,
  y = c("wk 3", "week-1", "7", "w#9")
)
dt <- dt %>% 
  mutate(z = str_extract(y, "([0-9]+)"))
dt
# ====================
# END
# ====================



# ====================
# Exercise 9.15
# ====================
dt <- tibble(
  WORD = fruit[1:10]
)
dt <- dt %>%
  mutate(VOWEL = str_replace_all(WORD, "[^aeiou]", "_")) %>%  #extract all word not vowels
  separate(VOWEL,c("1","2","3","4","5")) %>% # put vowels apart
  separate("2", c("6","7"), sep=1) #seperate two conjuncted vowels


dt[dt == ""] = NA  

dt <- dt %>%
  unite("VOWEL", c("1","6","7","3","4","5"), na.rm = TRUE, remove = FALSE) %>%
  select("WORD","VOWEL") %>%
  mutate(NUM_V = str_count(VOWEL, "[aeiou]"))

dt  
  # ====================
# END
# ====================



# ====================
# Case 2
# ====================
tb <- tibble(x = c("I我", "love愛", "you你"))
tb <- tb %>%
  separate(x, c("EN", "CH"), sep = -1)
tb
# ====================
# END
# ====================



# ====================
# Exercise 9.16
# ====================
df <- tibble(x = c("1-12周", "1-10周", "5-12周"))
df <- df %>%
  separate(x, c("START", "temp"), sep = "([-])") %>%
  separate(temp, c("END", "WEEK"), sep = -1)
df
# ====================
# END
# ====================



# ====================
# Case 3
# ====================
df <- tibble(
  x = c("1234", "B246", "217C", "2357f", "21WD4")
)
df |> mutate(SUM = sapply(str_extract_all(df$x,pattern = "\\d"),function(x){sum(as.integer(x))}))
# ====================
# END
# ====================



# ====================
# Exercise 9.17
# ====================
df <- tibble(
  x = c("12W34", "AB2C46", "B217C", "akTs6df", "21WD4")
)

#find all match(could be more than one)
df_list <- str_match_all(df$x, "(?<=[:upper:]{1})\\d+")

#set new dataframe
dfN <- data.frame(matrix(ncol = 2, nrow = 0))
a <- c("x","NUM")
colnames(dfN) <- a 

#set new row
for(i in (1:5)) {
  if(length(df_list[[i]]) == 1){
    newR = c(df$x[i], df_list[[i]])
    print(newR)
    dfN <- rbind(dfN,newR)
  }else if(length(df_list[[i]]) == 2){
    for (j in (1:2)) {
      newR = c(df$x[i], df_list[[i]][j])
      print(newR)
      dfN <- rbind(dfN,newR)
    }
  }
}

#reset colnames
colnames(dfN) <- a 

# outcome
# dfN
tibble(dfN)
# ====================
# END
# ====================


# ====================
# Exercise 9.18
# ====================
#new empty vector
charv <- vector()

#form new vector of chars
for (i in (1:5)) {
  loc = str_locate(dfN$x[i], dfN$NUM[i])  # locate the NUM, ouput a list of start/end
  index = loc[1][1]                       # get the location
  charv[i] <- substr(dfN$x[i], index-1, index-1)  # get the character, put in vector
}

charv


# add new vector in result data as new column
dfN_new <- dfN %>%
  add_column(CHAR = charv,
             .after = "x")

#outcome
tibble(dfN_new)
# ====================
# END
# ====================


# ====================
# Exercise 9.19
# ====================
setwd("~/Working_Directory")
idiom <- tibble(string = readLines("demo_data/dict-ch-idiom.txt"))

oneone <-idiom %>%
  filter(str_detect(string, "一.一.")) %>%
  mutate(pattern = str_replace(string, "一(.)一(.)", "\\1_\\2")) %>%
  separate(pattern, into = c("w1", "w2"), sep = "_")
oneone
# ====================
# END
# ====================


# ====================
# Exercise 9.20
# ====================
patternA <-idiom %>%
  filter(str_detect(string, "(.).\\1.")) %>%
  mutate(type = "A_A_")

patternB <-idiom %>%
  filter(str_detect(string, "(.)(.)(.)\\2")) %>%
  filter(!str_detect(string, "(.)(.)\\1\\2")) %>%
  mutate(type = "_A_A")

df <- rbind(patternA, patternB)
df

ggplot(df, aes(x=type, fill=type)) +
  geom_bar() +
  ylab(label = "n")
# ====================
# END
# ====================


# ====================
# Exercise 9.21
# ====================
patternA <- patternA %>%
  mutate(pattern = str_replace(string, ".(.).(.)", "\\1_\\2")) %>%
  separate(pattern, into = c("w1", "w2"), sep = "_")

patternB <- patternB %>%
  mutate(pattern = str_replace(string, "(.).(.).", "\\1_\\2")) %>%
  separate(pattern, into = c("w1", "w2"), sep = "_")

dt <- rbind(patternA,patternB)

dt <- dt %>%
  mutate(w1_w2 = if_else(w1 == w2, "SAME", "DIFF"))

dt_sum <- dt %>%
  group_by(type, w1_w2) %>%
  summarise(n=n())

dt_sum
# ====================
# END
# ====================