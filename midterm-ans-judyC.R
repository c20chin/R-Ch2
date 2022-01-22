##################
## Answer Sheet ##
##################
library(tidyverse)
library(viridis)
library(showtext)
# PART I ====================================================== #
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. This is to ensure a fresh start for each Part.


#########################
## Question 1-1        ##
#########################
one <- c(seq(-2.0, 2.0, length.out= 9))
two <- matrix(c(40:51), byrow = F, nrow = 4)
three <- c("How about American?", "Taiwan can help!", "Is China helping?")
four_vec <- c("HighSchool", "Bachelor", "Master", "PhD", "Master", "Bachelor", "HighSchool")
four <- factor(x = four_vec)
four <- ordered(four, levels = c("HighSchool", "Bachelor", "Master", "PhD"))


myList <- list(one, two, three, four)
myList

#########################
## Question 1-2        ##
#########################
sortMyList <- function(x, descending = TRUE) {
  temp <- list()
  if (descending == TRUE) {
    for (i in 1:length(x)){
      if(is.matrix(x[[i]]) == TRUE){
        temp[[i]] = x[[i]][nrow(x[[i]]):1,]
      } else if(is.ordered(x[[i]]) == TRUE){
        temp[[i]]= x[[i]][order(x[[i]], decreasing = TRUE)]
      }  else {
        temp[[i]]= x[[i]][order(x[[i]], decreasing = TRUE)]
      }
    }
  }else {
    for (i in 1:length(x)){
      if(is.matrix(x[[i]]) == TRUE){
        temp[[i]] = matrix(x[[i]], byrow = F, nrow = 4)
      } else if(is.ordered(x[[i]]) == TRUE){
        temp[[i]]= x[[i]][order(x[[i]], decreasing = FALSE)]
      } else {
        temp[[i]]= x[[i]][order(x[[i]], decreasing = FALSE)]
      }
    }
  }
  return(temp)
}

sortMyList(myList, descending = TRUE)
sortMyList(myList, descending = FALSE)

# PART II ====================================================== #
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. This is to ensure a fresh start for each Part.
## <----- WANRING: DO NOT CHANGE THE PATH OF THE DATA FILE------>
songs <- read_csv("demo_data/data-top-spotify-song-2010-2019.csv")


#########################
## Question 2-1        ##
#########################
songs1 <- songs %>%
  group_by(artist) %>%
  mutate(N = n()) %>%
  filter(N >= 10)

songs2 <- songs1 %>%
  group_by(artist) %>%
  mutate(MEAN_BPM = mean(bpm))

songs2 <- songs2 %>%
  group_by(artist) %>%
  slice(which.max(bpm))

songs3 <- subset(songs2, select =  c(2,3,16,17))

songs3 <- songs3 %>% mutate(FASTEST_SONG = title)
songs3 <- subset(songs3, select =  c(2:5)) 

songs3 <- songs3[order(-songs3$MEAN_BPM),]

songs3


#########################
## Question 2-2        ##
#########################
yearDur = subset(songs, select = c("year", "dur"))

yearDur <- yearDur %>%
  group_by(year) %>%
  summarise(
    mean_dur = mean(dur),
    sd_dur = sd(dur),
    n = n(),
    upper = round(mean_dur + (1.96 * (sd_dur / (n)^0.5 )), 2) ,
    lower = round(mean_dur - (1.96 * (sd_dur / (n)^0.5 )), 2)
  )

yearDur$year <-as.factor(yearDur$year)

ggplot(yearDur, aes(x=year, y=mean_dur, fill = year, color = year)) +
  geom_point(size = 6) + 
  geom_segment(aes(x=year, xend=year, y=180, yend=mean_dur), size = 4) + 
  xlab("Year") + ylab("Average Duration") +
  theme(axis.title.x = element_text(margin = margin(l = 80))) +
  labs(title = "Average Song Duration by Year") +
  geom_errorbar(aes(ymin= lower, ymax= upper, width=0.1),
                color = "grey55")

# PART III ====================================================== #
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. This is to ensure a fresh start for each Part.
## <----- WANRING: DO NOT CHANGE THE PATH OF THE DATA FILE------>
noise <- read_csv("demo_data/data-noise.csv")


#########################
## Question 3-1        ##
#########################

noise1 <- noise%>%
  pivot_longer(cols=c("NONE","LOW","MEDIUM", "HIGH"),
               names_to = "levels",
               values_to = "value") %>%
  group_by(GENDER, levels)

noise1$levels <-
  ordered(noise1$levels, levels = c("NONE","LOW","MEDIUM", "HIGH") )

ggplot(noise1, aes(x=levels, y= value, fill= GENDER)) +
  geom_boxplot(outlier.size = 1, notch = FALSE) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  xlab("Noise Level") +
  ylab("Average Task Performance")


# PART IV ====================================================== #
rm(list=ls(all=TRUE)) ## Please DO NOT remove this line. This is to ensure a fresh start for each Part.
## <----- WANRING: DO NOT CHANGE THE PATH OF THE DATA FILE------>
covidtw <- read_csv("demo_data/data-taiwan-covid.csv")


#########################
## Question 4-1        ##
#########################

dom  = subset(covidtw, select = c(2,6,7,8))
dom$個案研判日 <- as.Date(dom$個案研判日)
dom <- with(dom, dom[(個案研判日 >= "2021-05-01" & 個案研判日 <= "2021-08-01"),])
dom2 <- dom %>%
  group_by(個案研判日,年齡層) %>%
  filter(年齡層 == "0" |
              年齡層 == "1" |
              年齡層 == "2" |
              年齡層 == "3" |
              年齡層 == "4") %>%
  group_by(個案研判日) %>% 
  summarise(N = sum(確定病例數)) %>%
  mutate(年齡層 = "0-4")

#Todo group age 0-4
dom3<-dom[!(dom$年齡層 == "0" |
              dom$年齡層 == "1" |
              dom$年齡層 == "2" |
              dom$年齡層 == "3" |
              dom$年齡層 == "4"),]

#sum up daily cases
dom3 <- dom3 %>%
  group_by(個案研判日, 年齡層) %>% 
  summarise(N = sum(確定病例數))

dom4 <- rbind(dom3,dom2)


dom4$年齡層 <- ordered(dom4$年齡層, levels = c("0-4",
                                         "5-9", "10-14", "15-19", "20-24",
                                         "25-29", "30-34", "35-39", "40-44", 
                                         "45-49", "50-54", "55-59", "60-64",
                                         "65-69", "70+"))

ggplot(dom4, aes(x=個案研判日, y= N)) +
  geom_col(aes(fill = N)) +
  xlab("個案研判日\n (Dates)") +
  ylab("確診總數 \n (Total Number of Confirmed Cases)") +
  labs(title = "Number of Confirmed Cases Across Different Age Groups")+
  facet_wrap(. ~ 年齡層) +
  scale_fill_viridis() +  
  geom_smooth(method = "loess", size = 0.5, fill = "grey30") +
  theme(text = element_text(family = "Heiti TC Light"))




#########################
## Question 4-2        ##
#########################
inCase = subset(covidtw, select = c(5,6,8))

inCase$是否為境外移入 <-
  ordered(inCase$是否為境外移入, levels = c("是","否") )

inCase <- inCase %>%
  group_by(性別, 是否為境外移入) %>%
  mutate(sum = sum(確定病例數))
  

inCase <- inCase %>%
  mutate(sump = paste0("(", sum, ")"))

inCase <- inCase %>%
  group_by(是否為境外移入) %>%
  mutate(outsum = sum(確定病例數)) %>%
  mutate(percent = round(sum/outsum, 4) *100) %>%
  mutate(percent = paste0(percent, "%"))

ggplot(inCase, aes(x = 是否為境外移入, y = sum, fill = 性別)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  ylim(0,8000) +
  theme(text = element_text(family = "Heiti TC Light")) +
  xlab("是否為境外移入？\n (ImportedCase?)") +
  ylab("確診總數 \n (Total Confirmed Cases)") +
  geom_text(aes(label = sump), color = "grey55", 
            position = position_dodge(width = .9), vjust = -0.5) + 
  geom_text(aes(label = percent, color = 性別), 
            position = position_dodge(width = .9), vjust = -2.5)
  
  
  
