install.packages("tidytuesdayR")
library(tidyverse)
library(skimr)
# Download all data files
tuesdata <- tidytuesdayR::tt_load("2021-07-27")

# Individual dataframes
olympics <- tuesdata$olympics

# A second dataframe is available
# It is not required, but may prove useful in certain cases
regions <- tuesdata$regions

#cleaning the dataset
olympics$medal <- factor(olympics$medal,levels = c("Gold","Silver","Bronze"))
olympics$season <- factor(olympics$season,levels = c("Summer","Winter"))
olympics$sex <-  factor(olympics$sex,levels = c("M","F"))



#--------------------an abstract look at the dataset------------------------

glimpse(olympics)    

olympics %>% skim_without_charts()

olympics %>% 
  select(sex,age,height,weight,year,season,medal)%>% 
  summary()

#list of all sports Olympics did cover since 1896?
sports <- olympics %>% 
  select(sport) %>% 
  distinct()

print.table(sports)


#list of Olympics events since 1896
events <- olympics %>% 
  select(event) %>% 
  distinct()

print.table(events)

#list of all Olympics games
games <- olympics %>% 
  select(games) %>% 
  distinct()%>% 
  arrange_all()

print.table(games)

#list of all years in which Olympics games happens
years <- olympics %>% 
  select(year) %>% 
  distinct() %>% 
  arrange_all()

print.table(years)



#how many times did Olympics happens ?

games %>% 
  count()


#average wight of all Olympics participants since 1896
olympics %>% 
  filter(!is.na(weight)) %>% 
  summarise(mean = mean(weight))



#average height of all Olympics participants since 1896
olympics %>% 
  filter(!is.na(height)) %>% 
  summarise(mean = mean(height))


#average age of all Olympics participants since 1896
olympics %>% 
  filter(!is.na(age)) %>% 
  summarise(mean = mean(age))


#number of males and females 






# does the avg height of the athlete increases over time ?

# did ww1 and ww2 affect the number of participated countries ?

# does the avg age of the athlete increases over time ?

#does the weather affect the performance of the country? for example, if the country has a cold weather it will perform better in the winter season

#does the winners of the sport have similar height, weight, and age over the years ?

#Is there a country dominating a certain event?











#--------------------------------------------
str(olympics)


art <- filter(olympics,sport == "Art Competitions")
art  


medal_counts_art <- art %>% filter(!is.na(medal))%>%
  
  group_by(team, medal) %>%
  ?summarize(Count=length(medal))
str(medal_counts_art)

# order Team by total medal count
levs_art <- medal_counts_art %>%
  group_by(team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>% 
  select(team)

medal_counts_art$team <- factor(medal_counts_art$team, levels=levs_art$team)

# plot
ggplot(medal_counts_art, aes(x=team, y=Count, fill=medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Historical medal counts from Art Competitions") +
  theme(plot.title = element_text(hjust = 0.5))+
  NULL