# Install & loading packages
install.packages("tidytuesdayR")
install.packages("plyr", dependencies = TRUE)
install.packages("rworldmap", dependencies = TRUE)
install.packages("repr",dependencies = TRUE)

library(plyr)
library(dplyr)

library(tidyverse)
library(skimr)
library(rworldmap)
library(repr)
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
#list of all olympics teams since 1896
teams <- olympics %>% 
  select(team) %>% 
  distinct()

print.table(teams)

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


#-----------------------how did the numbers of female and male athlete changed over time------------------------

#count the number of females and males for each year
counts_sex <- olympics %>% 
  group_by(year, sex) %>%
  summarize(Count = length(unique(id))) %>%
  spread(sex, Count)

#clean the data
counts_sex$M[is.na(counts_sex$M)] <- 0
counts_sex$F[is.na(counts_sex$F)] <- 0
counts_sex$year <- as.factor(counts_sex$year)

#calculate the total on participants 
counts_sex <- mutate(counts_sex,Total = sum(M,F,na.rm=T)) 

#calculate the percentage of females and males from the total
counts_sex$M <- counts_sex$M/counts_sex$Total
counts_sex$F <- counts_sex$F/counts_sex$Total

#creating the graph
names(counts_sex)[2:3] <- c("Male","Female")
counts_sex <- gather(counts_sex,key= sex , value = Count,2:3)

ggplot(counts_sex,aes(x=year,y=Count,fill=sex)) +
    geom_col() +
    coord_flip() 
  
  
 

  

#----------------------------changes of athletes' height,weight and BMI over time---------------------------
# does the avg height of the athlete increases over time ?
olympics %>% 
  filter(!is.na(height)) %>% 
  group_by(year) %>% 
  summarise( height= mean(height))-> avgHight

ggplot(avgHight , aes(year , height)) + geom_point()+ geom_smooth(se = FALSE)

# does the avg weight of the athlete increases over time ?
olympics %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  summarise( weight= mean(weight))-> avgWeight

ggplot(avgWeight , aes(year , weight)) + geom_point()+ geom_smooth(se = FALSE)


# does the avg BMI of the athlete increases over time ?

avgBMI <- avgHight
avgBMI$weight <- avgWeight$weight

avgBMI <- avgBMI %>% 
  mutate(bmi = (weight)/(height/100 * height/100))

ggplot(avgBMI ,aes(year , bmi) )+ geom_point()+ geom_smooth(se = FALSE) 

 
#-----------------did WW1 and WW2 affect the number of participated countries ?--------------------------
  
  
#--------does the weather affect the performance of the country? for example, if the country has a cold weather it will perform better in the winter season-----------
#needs much more work
world <- map_data(map="world")
world <- world[world$region != "Antarctica",]
colnames(regions) <- c("NOC","Country","notes")
y=ddply(regions, .(Country), numcolwise(sum))
sPDF <- joinCountryData2Map( y
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "Country")

mapCountryData(sPDF
               ,nameColumnToPlot='MedalCount')
  ggplot() +
  geom_map(
     data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1  ) 
# y=ddply(olympics, .(team,medal), numcolwise(sum))
# PDF <- joinCountryData2Map( y
#                             ,joinCode = "ISO3"
#                             ,nameJoinColumn = "Country")



#--------------------does the age of the athlete effect on his/her performance?--------------------------

#the avg age in each year
olympics %>% 
  select(year , team,age) %>%
  filter(!is.na(age)) %>% 
  group_by(year) %>% 
  summarise(age = mean(age)) -> avrgAgeInEachYear 

ggplot(avrgAgeInEachYear , aes(year , age)) + geom_point()+ geom_smooth(se = FALSE)

winnersVslosers <- olympics %>% 
            mutate(winner = if_else(is.na(medal),"loser","winner"))

#how old are most of the winners and lossers?
winnersVslosers$winner <- factor(winnersVslosers$winner,levels = c("loser","winner")) 
winnersVslosers %>%
  filter(!is.na(age)) %>% 
  group_by(winner,age) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=age ,fill= winner)) +
  geom_density(alpha=0.25)


#--------------------------Is there a country dominating on an certain event?--------------------------------

event_team_prop <- olympics %>% 
                    filter(!is.na(medal)) %>% 
                    group_by(event,team) %>% 
                    summarise(count=n())



#-----------------top 10 teams with respect to number of medals---------------------------

# count number of medals for each team
medal_counts <- olympics %>% filter(!is.na(medal))%>%
  group_by(team, medal) %>%
  summarize(Count=length(medal))

# order team by total medal count
  levs_art <- medal_counts %>%
    group_by(team) %>%
    summarize(Total=sum(Count)) %>%
    arrange(Total) %>%
    select(team) %>% 
    tail(n=10)

  medal_counts$team <- factor(medal_counts$team, levels=levs_art$team)
  medal_counts <- medal_counts %>% filter(!is.na(team))
 
  
    # plot
  ggplot(medal_counts, aes(x=team, y=Count, fill=medal)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values=c("gold1","gray70","gold4")) +
    theme(plot.title = element_text(hjust = 0.5))+
    NULL




 
  
  
  
  
  
  
  
  

  

  

  