# Chapter 10 Data Import


# ====================
# Exercise 10.1
# ==================== 
x1 <- "U+20AC"
Encoding(x1)
x2 <- iconv(x1, to = "UTF-8")
Encoding(x2)
x1_decimal <- utf8ToInt(x1)
x1_hex <- as.hexmode(x1_decimal)
x1_hex
# ====================
# END
# ====================


# ====================
# Exercise 10.2
# ==================== 
x3 <- "我"
x4 <- "你"
x3_hex <- as.hexmode(utf8ToInt(x3))
x4_hex <- as.hexmode(utf8ToInt(x4))

x3_hex
x4_hex

# "我" has larger code points

# ====================
# END
# ====================



# ====================
# Exercise 10.3
# ==================== 
setwd("~/Working_Directory")
alice <- readLines(con = "demo_data/corp-alice.txt" )
# Each line in "alice" is a sentence

# ====================
# END
# ====================


# ====================
# Exercise 10.4
# ==================== 
sample(alice[nzchar(alice)],10)

# What this code do:
# Ramdomly choosing 10 non-empty sentences out of "alice", 
# sample(x, size) decides choosing what size out of x
# nzchar() returns TRUE if the element has non-zero length
# alice[nzchar(alice)] singles out the lines that are not zero-length


# ====================
# END
# ====================



# ====================
# Exercise 10.5
# ==================== 
library(tidyverse)

alice %>%
  as_tibble() %>%
  filter(nzchar(alice) == TRUE) -> output

output %>%
  as_vector() %>%
  sample(size = 10) %>%
  writeLines(con="corp-alice-2.txt")
# ====================
# END
# ====================


# ====================
# Exercise 10.6
# ==================== 
tx1 <- file(description = "demo_data/chinese_gb2312.txt",
               encoding = "gb2312") 
text_ch_gb2312 <- readLines(infile) 
text_ch_gb2312


tx2 <- file(description = "demo_data/chinese_big5.txt",
            encoding = "big5") 
text_big5 <- readLines(infile) 
text_big5


tx3 <- file(description = "demo_data/chinese_utf8.txt",
            encoding = "utf8") 
text_utf8 <- readLines(infile) 
text_utf8

# ====================
# END
# ====================



# ====================
# Exercise 10.7
# ==================== 
library(readr)
bigram <- read_csv("demo_data/data-bnc-bigram.csv")

bi_order <- bigram[order(bigram$bi.freq, decreasing = TRUE),]
new_bigram <- bi_order[1:20, ]
new_bigram
# ====================
# END
# ====================



# ====================
# Exercise 10.8
# ==================== 
write.csv(new_bigram, "data-bnc-bigram-10.csv")
# ====================
# END
# ====================



# ====================
# Exercise 10.9
# ==================== 
# file.create() : creates files with the given names if they do not already exist  
# dir.create() : creates the last element of the path
# unlink() :  deletes the file(s) or directories specified by x.
# basename() : removes all of the path up to and including the last path separator (if any)
# file.info() : extract information about files on user's file systems
# save() : writes an external representation of R objects to the specified file
# load() : can load R objects saved in the current or any earlier format.

# ====================
# END
# ====================



# ====================
# Exercise 10.10
# ==================== 
dir.create("./temp")
setwd("~/Working_Directory")
bigram_freq200 <- bi_order[1:200, ]
setwd("~/Working_Directory/temp")
write.csv(bigram_freq200, "data-bnc-bigram-freq200.csv")

setwd("~/Working_Directory")
bnc_bigram_freq200 <- read_csv("temp/data-bnc-bigram-freq200.csv")
bnc_bigram_freq200


# ====================
# END
# ====================
