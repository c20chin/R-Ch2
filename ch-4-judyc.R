# Chapter 4 Subsetting


# ====================
# Exercise 4.1
# ====================
m  <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
m
# ====================
# END
# ====================



# ====================
# Exercise 4.2
# ====================
n <- sample(m)
n
# ====================
# END
# ====================



# ====================
# Exercise 4.3
# ====================
#empty vector for TRUE/FALSE value
o <- c()

#for loop to evaluate difference
for (i in 1:length(m)){
  if (m[i] == n[i]){
    o[i] <- TRUE
  }else{
    o[i] <- FALSE
  }
}
 
#outcome
table(o)

#print out the same char
for (i in 1:length(m)){
  if (o[i] == TRUE){
    print(m[i])
    }
}
# ====================
# END
# ====================



# ====================
# Exercise 4.4
# ====================
one <- c(seq(-4, 4, length.out= 20))
two <- matrix(c(F,T,T,T,F,T,T,F,F), byrow = F, nrow = 3)
for (i in 1:length(two)){
  if (i == 'T'){
    i <- TRUE
  }else{
    i <- FALSE
  }
}
three <- c('don','quixote')
four_vec <- c('LOW', 'MID', 'LOW', 'MID', 'MID', 'HIGH')
four <- factor(x = four_vec)


list.a <- list(one = one, two, three, four)
list.a
# ====================
# END
# ====================



# ====================
# Exercise 4.5
# ====================
list.a[[2]][2:1, 2:3]
# ====================
# END
# ====================



# ====================
# Exercise 4.6
# ====================
list.a[[1]][list.a[[1]] > 1]
# ====================
# END
# ====================



# ====================
# Exercise 4.7
# ====================
which(list.a[[4]] == "MID")
# ====================
# END
# ====================



# ====================
# Exercise 4.8
# ====================
dframe <- data.frame(
  person = c('Stan', 'Francine', 'Steve', 'Roger', 'Hayley', 'Klaus'),
  sex = factor(c('M', 'F', 'M', 'M', 'F', 'M' )),
  funny= factor(c('High', 'Mid', 'Low', 'High', 'Mid', 'Mid'))
)
dframe
# ====================
# END
# ====================



# ====================
# Exercise 4.9
# ====================
age <- c(41, 41, 15, 1600, 21, 60)
dframe$age <- age
dframe
# ====================
# END
# ====================



# ====================
# Exercise 4.10
# ====================
subset(dframe, dframe$sex == 'M' & dframe$funny != 'High')
# ====================
# END
# ====================



# ====================
# Exercise 4.11
# ====================
require(tibble)

test_tb <- tibble(
  person = c('Stan', 'Francine', 'Steve', 'Roger', 'Hayley', 'Klaus'),
  sex = c('M', 'F', 'M', 'M', 'F', 'M' ),
  funny= factor(c('High', 'Mid', 'Low', 'High', 'Mid', 'Mid'))
)

#COMPARISON
dframe
test_tb

#MAJOR DIFFERENCES:
# When tibble is printed out, it is more tidy and clear.
# 1. The structure of data frame stated on the top as row * column
# 2. Below the name of the columns, we see column data types printed out

# ====================
# END
# ====================




# ====================
# Exercise 4.12
# ====================
#SETUP & LOAD IN DATA
setwd("~/Working_Directory")
wf_data_first = read.csv(
  file = 'demo_data/data-word-freq.csv', 
  stringsAsFactors = T)
str(wf_data_first)

require(readr)
wf_data_sec = readr::read_csv(
  file = 'demo_data/data-word-freq.csv'
)

#COMPARISON
wf_data_first
wf_data_sec

#DIFFERENCE:

# 1. Overall, I think that using "readr" to load in data is a clearer way.
# "readr" gives a summarize on the data structure immediately. However,
# we would have to use "str()" to access those information 
# for the 'read.csv()' one.

# 2. When the data is printed out, 'readr' does not print out the whole thing,
# but only the first 10 rows of data. 
# In contrast, the data loaded in with 'read.csv()' automatically prints out
# everything.

# 3. 'readr' uses tibble to load in and structure its data frame.

# ====================
# END
# ====================