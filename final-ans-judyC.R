##################
## Answer Sheet ##
##################

# PART I ====================================================== #

#########################
## Question 1-1        ##
#########################
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. 
library(tidyverse)    ## Please DO NOT remove this line. 

anno <- " I   hope  we all   can　finish the exam       in time.   "

clean <- function(x) {
  x <- str_trim(x,"left")
  x <- str_trim(x,"right")
  x<- str_replace_all(x, "\\s+"," ")
  return(x)
}

clean(anno)



#########################
## Question 1-2        ##
#########################
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. 
library(tidyverse)    ## Please DO NOT remove this line. 

txt<-"<w VVB>Introduce <w NP0>Brenda <w PNQ>who<w VBZ>'s <w VVG>going <w TO0>to <w VVI>speak <w PRP>to <w PNP>us <w AVP-PRP>on <w VVB>Make <w VDI>do <w CJC> and <w VVB>Mend <w CJC>and <w PNP>she<w VHZ>'s <w VVN>asked <w PNP>me <w TO0>to <w VVI>say <w CJT>that<c PUN>."

PAT <- "<[wc]\\s[TNPC].*?(?=<|$)"  ## <--- Please create your regular expression.

str_replace_all(txt, PAT, "")


#########################
## Question 1-3        ##
#########################
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. 
library(tidyverse)    ## Please DO NOT remove this line. 

txt<-"<w DPS>my <w NN1>mum <w CJC>and <w DPS>my <w NN1>aunt <w VVD>went <w PRP>into <w NN1>service" 

PAT <- "(?<=>).+?(?=\\s|$)" ## <--- Please create your own regular expression.
str_extract_all(txt, PAT)



#########################
## Question 1-4        ##
#########################
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. 
library(tidyverse)    ## Please DO NOT remove this line. 

alice <- readLines("demo_data/corp-alice.txt") ## Please DO NOT remove and change this line. 

alice2 <- tolower(alice)

pat = "(?<=\\b(is|am|are|was|were|be|been)\\s)\\w*ly\\s\\w*(en|ed)\\b"

#new df
alice_df <- data.frame(matrix(ncol = 3, nrow = 0))
a <- c("INDEX","CONSTRUCTION", "ORIGINAL_LINE")
colnames(alice_df) <- a

#new rows
for(i in (1:length(alice2))) {
  if(str_detect(alice2[i], pat) == TRUE){
    
    print(alice2[i])
    cons = str_extract(alice2[i],pat)
    
    sen = alice[i]
    newR = c(i, cons, sen)
    alice_df <- rbind(alice_df,newR)
  }
}

#reset colnames
colnames(alice_df) <- a

#outcome
alice_df

# PART II ====================================================== #


#########################
## Question 2-1        ##
#########################
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line.
library(tidyverse)    ## Please DO NOT remove this line.

peer_reviews <- read_csv("demo_data/peer-review.csv") ## Please DO NOT remove and change this line.


#rm duplicate
peer_reviews <- peer_reviews[ !duplicated(peer_reviews[, c("What is your name?", "Who's work are you evaluating?")], fromLast=T),]

#rename colnames
new_c <- c("Time", "Reviewer", "Creator", "ViQ", "VoQ", "NS", "FS", "Feedback")
colnames(peer_reviews) <- new_c

#change to score function

peer_reviews["ViQ"][peer_reviews["ViQ"] == "5. Amazing"] <- "5"
peer_reviews["ViQ"][peer_reviews["ViQ"] == "4. Well-done"] <- "4"
peer_reviews["ViQ"][peer_reviews["ViQ"] == "3. Average"] <- "3"
peer_reviews["ViQ"][peer_reviews["ViQ"] == "2. Below Average"] <- "2"

peer_reviews["VoQ"][peer_reviews["VoQ"] == "5. Amazing"] <- "5"
peer_reviews["VoQ"][peer_reviews["VoQ"] == "4. Well-done"] <- "4"
peer_reviews["VoQ"][peer_reviews["VoQ"] == "3. Average"] <- "3"
peer_reviews["VoQ"][peer_reviews["VoQ"] == "2. Below Average"] <- "2"

peer_reviews["NS"][peer_reviews["NS"] == "5. Amazing"] <- "5"
peer_reviews["NS"][peer_reviews["NS"] == "4. Well-done"] <- "4"
peer_reviews["NS"][peer_reviews["NS"] == "3. Average"] <- "3"
peer_reviews["NS"][peer_reviews["NS"] == "2. Below Average"] <- "2"

peer_reviews["FS"][peer_reviews["FS"] == "5. Amazing"] <- "5"
peer_reviews["FS"][peer_reviews["FS"] == "4. Well-done"] <- "4"
peer_reviews["FS"][peer_reviews["FS"] == "3. Average"] <- "3"
peer_reviews["FS"][peer_reviews["FS"] == "2. Below Average"] <- "2"


# adjusting columns
peer_r <- peer_reviews %>%
  mutate(ViQ = as.numeric(ViQ)) %>%
  mutate(VoQ = as.numeric(VoQ)) %>%
  mutate(NS = as.numeric(NS)) %>%
  mutate(FS = as.numeric(FS)) %>%
  mutate(Creator = as.factor(Creator)) %>%
  mutate(score = (ViQ + VoQ + NS + FS)*5) %>%
  group_by(Creator) %>%
  mutate(scoreA = round(mean(score),4)) %>%
  mutate(SDA = round(sd(score),4)) %>%
  mutate(Creator = fct_reorder(Creator, -scoreA))


# forming the dataframe
peer <- peer_r %>%
  group_by(Creator) %>%
  summarise(Num_Reviews = n(), Score = round(mean(score),4), SD = round(sd(score),4)) %>%
  mutate(Num_Reviews = as.numeric(Num_Reviews)) 

peer <- peer[order(peer$Score, decreasing = TRUE),]
  

#Final answer
peer

#graph
ggplot(peer_r, aes(x = fct_reorder(Creator, -scoreA), y = score))+
  geom_point(aes(color = "grey50"), size=1.2, alpha=0.3, position=position_jitter(w=0.1, h=0)) +
  theme(axis.text.x = element_text(angle=+90)) +
  geom_errorbar(aes(ymin= scoreA - SDA , ymax= scoreA + SDA, width=0.1,
                    color = Creator)) +
  theme(legend.position = "none") +
  stat_summary(fun = mean, geom = "point", size = 3, aes(color=Creator)) +
  labs(title = "Peer Review Scores of All Projects", 
       x = "Creator(Project Author's Name)", 
       y = "Peer Review Percentage Scores")
  



#########################
## Question 2-2        ##
#########################
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line.
library(tidyverse)    ## Please DO NOT remove this line.

generateReports <- function(path, newdir){
  x = path
  y = newdir
  peer_reviews <- read_csv(x)
  
  #rm duplicate
  peer_reviews <- peer_reviews[ !duplicated(peer_reviews[, c("What is your name?", "Who's work are you evaluating?")], fromLast=T),]
  
  #rename colnames
  new_c <- c("Time", "Reviewer", "Creator", "ViQ", "VoQ", "NS", "FS", "Feedback")
  colnames(peer_reviews) <- new_c
  
  #select name and feedback
  peer_n <- peer_reviews %>%
    select("Creator", "Feedback")
  
  Encoding(peer_n[["Feedback"]]) <- "UTF-8"
  
  
  #create new directory
  dir.create(y)
  
  #create new files
  for (i in 1:19){
    name = peer_n$Creator[i]   #one name
    temp <- peer_n %>% filter(Creator == name) # all feedback in this name
    for (j in 1:19) {  #paste feedback 19 times
      text = paste0("<===== REVIEWER No. ", j," =====>\n\n",temp$Feedback[j],"\n")
      write(text,file = paste0(y, name ,".doc"),append=TRUE)
    }
  }
}

## Your code should end with the following function call. 
## Please DO NOT remove and change this last line
generateReports(path = "demo_data/peer-review.csv",
                newdir = "demo_data/Project-Feedbacks/")

