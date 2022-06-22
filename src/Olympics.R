<<<<<<< HEAD
library(tidyverse)
library(janitor)
library(GGally)
library(here)
library(kableExtra)
library(tidytuesdayR)



tuesdata <- tidytuesdayR::tt_load("2021-07-27")

olympics <- tuesdata$olympics
regions <- tuesdata$regions  #It is not required, but may prove useful in certain cases

# olympics %>% 
#   clean_names() -> olympics

glimpse(olympics)
summary(olympics)


counts_NOC <- olympics %>% filter(year %in% c(1936,1956,1976,1996,2016)) %>%
  group_by(year, noc, sex) %>%
  summarize(Count = length(unique(id))) %>%
  spread(sex, Count) %>%
  mutate(Total = sum(M,F,na.rm=T)) %>%
  filter(Total > 49) 
names(counts_NOC)[3:4] <- c("Male","Female")
counts_NOC$Male[is.na(counts_NOC$Male)] <- 0
counts_NOC$Female[is.na(counts_NOC$Female)] <- 0
counts_NOC$year <- as.factor(counts_NOC$year)

ggplot(counts_NOC, aes(x=Male, y=Female, group=year, color=year)) +
  geom_point(alpha=0.6) +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  geom_smooth(method="lm", se=FALSE) +
  labs(title = "Female vs. Male Olympians from participating NOCs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(reverse=TRUE))


#ggplot(olympics, aes(noc, team)) +
 # geom_boxplot()

olympics %>% group_by(name , team) %>%
  ggplot(aes(x=name, y=team, group=name, color=name)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=c("darkblue","red"))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Height/Weight data completeness from each Olympiad")


=======
# Install & loading packages
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




 
>>>>>>> e4ec7c19c67b5b7bb14fab185622ca0451252d2d
