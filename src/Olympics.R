

# -----------------??-----------------------
counts_NOC <- olympics %>% filter(year %in% c(1936,1956,1976,1996,2016)) %>%
  group_by(year, noc, sex) %>%
  summarize(Count = length(unique(id))) %>%
  spread(sex, Count) %>%
  mutate(Total = sum(M,F,na.rm=T)) %>%
  filter(Total > 49) 
names(counts_NOC)[3:4] <- c("Male","Female")

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
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Install & loading packages
install.packages("kableExtra", dependencies = TRUE)
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
library(DT)
library(janitor)
library(GGally)
library(here)
library(kableExtra)
library(tidytuesdayR)
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
olympics %>% 
  select(sport) %>% 
  distinct()%>% 
  datatable()


#list of all olympics teams since 1896
olympics %>% 
  select(team) %>% 
  distinct()%>% 
  datatable()



#list of Olympics events since 1896
olympics %>% 
  select(event) %>% 
  distinct()%>% 
  datatable()


#list of all Olympics games
 olympics %>% 
  select(games) %>% 
  distinct()%>% 
  arrange_all()%>% 
  datatable()



#list of all years in which Olympics games happens
 olympics %>% 
  select(year) %>% 
  distinct() %>% 
  arrange_all()%>% 
  datatable()




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


#is there any kids(less than 13) got an Olympic medals?
olympics %>% 
  filter(age<13, !is.na(medal)) %>% 
  datatable()

#is there any kids(less than 13) got an Olympic medals?

olympics %>% 
        filter(weight>150, !is.na(medal)) %>% 
  datatable()


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
  coord_flip() +
  labs(x="Year",y="Male/Female Percentage")






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
  mutate(BMI = (weight)/(height/100 * height/100))

ggplot(avgBMI ,aes(year , BMI) )+ geom_point()+ geom_smooth(se = FALSE) 


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

#how old are most of the winners and losers?
winnersVslosers$winner <- factor(winnersVslosers$winner,levels = c("loser","winner")) 

winnersVslosers %>%
  filter(!is.na(age)) %>% 
  group_by(winner,age) %>% 
  summarise(count=n()) -> winnersVslosers

#plot
winnersVslosers %>% 
  ggplot(aes(x=age ,fill= winner)) +
  geom_density(alpha=0.25)


#--------------------------Is there a country dominating on an certain event?--------------------------------

event_team_prop <- olympics %>% 
  filter(!is.na(medal)) %>% 
  group_by(event,team) %>% 
  summarise(count=n()) %>% 
  group_by(event) %>% 
  summarise(event =event,team=team,count,total = sum(count))  %>% 
  mutate(prop = count/total) %>% 
  select(event,team,count,prop)
  
#dominating on an certain event means a team has more than 50% of the medals of an event that have happened at least 10 times

#plot
event_team_prop %>% 
 filter(prop>0.5,count>=10) 

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
  tail(n=20)

medal_counts$team <- factor(medal_counts$team, levels=levs_art$team)
medal_counts <- medal_counts %>% filter(!is.na(team))


# plot
ggplot(medal_counts, aes(x=team, y=Count, fill=medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Team",y="Number of Medals athletes since 1896")









#--------------------Number of football medals by each team from 2000-2016--------------------------
football_teams_medals<- olympics %>%
  filter(sport == "Football", year >= 2000,!is.na(medal)) %>%
  group_by(team) %>%
  summarise(count=n()) %>% 
  arrange(count) 

#plot
football_teams_medals %>% 
  ggplot(aes( x= reorder(team,count),y= count)) +
  geom_col() +
  coord_flip() +
  labs(x="Team",y="Number of Football Medals from 2000 to 2016")



#number of athletes per countery 
olympics %>%
  group_by(noc) %>%
  count(name) %>%
  summarise(count = sum(n)) %>% 
  arrange(count) %>% 
  tail(n=20) %>% 
  
#plot
ggplot(aes(x= reorder(noc,count), y=count)) +
 geom_col() +
 coord_flip() +
 labs(x="Team",y="Number of Olympics athletes since 1896")






#------------mean of age in year With respect of kind of model----------------------

medal <-
  olympics %>% 
  filter(!is.na(age),!is.na(medal)) %>% 
  group_by(year, medal) %>% 
  summarise(age = mean(age))

#plot
ggplot(medal , aes(x=year , y=age, color = medal)) + geom_line() +
scale_color_manual(values=c("gold1","gray70","gold4")) 

#---------- 
### this will show the first femail winner
#there are 3 winner in 1900
olympics %>%
  filter(sex == "F" , medal =="Gold" ) %>%
  arrange(year)
### the number of noc
### usa has 6
olympics %>%
  filter(year == 1904 , sex == "F") %>%
  distinct(name , .keep_all= TRUE) %>%
  group_by(noc) %>%
  summarise(noc ,length(medal) ) %>%
  distinct()
### but in 1908
### there 39 female participent
olympics %>%
  filter(year == 1908 , sex == "F") %>%
  distinct(name , .keep_all= TRUE) %>%
  group_by(noc) %>%
  summarise(noc ,length(medal) ) %>%
  distinct()
## this will show the number of participent in 2016 (F/M)
# there are 6223 F and 7465male meaning 45% are women
olympics %>%
  filter(year == 2016) %>%
  group_by(sex) %>%
  ئذsummarise(sex , length(id)) %>%
  distinct()
  

