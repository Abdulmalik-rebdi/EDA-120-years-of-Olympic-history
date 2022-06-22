# Install & loading packages
install.packages("tidytuesdayR")
install.packages("ggforce")
library(tidyverse)
library(skimr)
library(ggforce)
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





# did ww1 and ww2 affect the number of participated countries ?


#does the weather affect the performance of the country? for example, if the country has a cold weather it will perform better in the winter season

#does the winners of the sport have similar height, weight, and age over the years ?

#Is there a country dominating a certain event?



###

# counts_NOC <- olympics %>% filter(year %in% c(1936,1956,1976,1996,2016)) %>%
#   group_by(year, noc, sex) %>%
#   summarize(Count = length(unique(id))) %>%
#   spread(sex, Count) %>%
#   mutate(Total = sum(M,F,na.rm=T)) %>%
#   filter(Total > 49)
# names(counts_NOC)[3:4] <- c("Male","Female")
# counts_NOC$Male[is.na(counts_NOC$Male)] <- 0
# counts_NOC$Female[is.na(counts_NOC$Female)] <- 0
# counts_NOC$year <- as.factor(counts_NOC$year)
# ggplot(counts_NOC, aes(x=Male, y=Female, group=year, color=year)) +
#   geom_point(alpha=0.6) +
#   geom_abline(intercept=0, slope=1, linetype="dashed") +
#   geom_smooth(method="lm", se=FALSE) +
#   labs(title = "Female vs. Male Olympians from participating NOCs") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   guides(color=guide_legend(reverse=TRUE))

###







#--------------------------------------------
str(olympics)


art <- filter(olympics,sport == "Art Competitions")
art  


medal_counts_art <- art %>% filter(!is.na(medal))%>%
  group_by(team, medal) %>%
  summarize(Count = length(medal))

str(medal_counts_art)

# order Team by total medal count
levs_art <- medal_counts_art %>%
  group_by(team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(team)

medal_counts_art$team <- factor(medal_counts_art$team, levels=levs_art$team)

# plot
# ggplot(medal_counts_art, aes(x=team, y=Count, fill=medal)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_manual(values=c("gold1","gray70","gold4")) +
#   ggtitle("Historical medal counts from Art Competitions") +
#   theme(plot.title = element_text(hjust = 0.5))+
#   NULL

#----------------------------------- total medal in each year 



year_NOC_medal <- olympics %>% 
  mutate(Has_Medal =
           case_when(medal == "Gold" |
                       medal == "Silver" |
                       medal == "Bronze" ~ 1)) %>% 
  select(year, noc, Has_Medal) %>% # 
  group_by(year, noc) %>% 
  summarise(total_medals = sum(Has_Medal, na.rm = TRUE))

view(year_NOC_medal) 

year_NOC_medal[order(year_NOC_medal$total_medals ,decreasing = TRUE),] ##highest win per year 
#------------------



# ggplot(year_NOC_medal, aes(total_medals,year , color = noc)) +
#   geom_jitter(alpha = 0.15, shape = 16) +
#   coord_fixed() +
#   theme(legend.position =  "remove")

#------------------ the avg age in each year
olympics %>% 
  drop_na() %>% 
  select(year , team,age) %>% 
  group_by(year) %>% 
  summarise(age = mean(age) , team = team) -> avrgAgeInEachYear 


#----------------------------
# does the avg height of the athlete increases over time ?
olympics %>% 
  drop_na() %>% 
  group_by(year) %>% 
  summarise(year , height= mean(height) , weight = mean(weight) )%>%
  distinct()  -> avgHight
#nrow(avgHight)

avgHight <- avgHight %>% 
  mutate(bmi = (weight)/(height/100 * height/100))

ggplot(avgHight ,aes(year , bmi) )+geom_bar(stat = "identity") ## is this plot ok ? 
ggplot(avgHight , aes(year , height)) + geom_bar(stat = "identity")

#-------------------
# does the avg age of the athlete increases over time ?
olympics %>% 
  drop_na() %>% 
  group_by(year) %>% 
  summarise(year , Age= mean(age)  )%>%
  distinct()  -> avgAge
#nrow(avgAge)

ggplot(avgAge , aes(year , Age)) + geom_bar(stat = "identity")
ggplot(avgAge , aes(year , Age)) + geom_line() ## is it accurate  ? 

#-----with respect with medal type (gold , silver ,bronze)

medal_counts <- olympics %>% filter(!is.na(medal))%>%
  group_by(team, medal) %>%
  summarize(Count = length(medal)) 


#---------- with no respect of which medal
medal_countsALL <- olympics %>% filter(!is.na(medal))%>%
  group_by(team) %>%
  summarize(Count = length(medal)) 

medal_countsALL <- medal_countsALL[order(medal_countsALL$Count, decreasing = TRUE), ] %>% head(10)

ggplot(medal_countsALL , aes(Count , team )) + geom_bar(stat =  "identity")
# ggplot(medal_countsALL , aes(Count , team )) + geom_area()

##------ mean of age in year With respect of kind of model  

##----- will show number of events per year

medal2 <-
  olympics %>% 
  group_by(year, medal = medal=="Gold" , season) %>% #the idea here is when we group by medal = gold this will show us the number of games (one medal per game)
  summarise(
    TotalMedal = length(medal) ## i dont know whay na.rm do yet but when i remove it the graph change a lot
  )

# <<<<<<< HEAD
# medal %>% 
#   filter( medal == TRUE) %>%
#   ggplot(aes(x=year , y= TotalMedal  , color = season)) + geom_line()
# =======

medal2 %>% 
  filter(medal , medal == TRUE)    ->medal ##the idea here is will count the number of true(medal == gold) in each year summer or 

ggplot(medal , aes(year , TotalMedal  , color = season)) + geom_line()

  ###------

# 
# medal %>% 
#   filter(medal , medal == TRUE)    ->medal2
# medal2 %>%  filter(medal , season == "Winter") ->medal2
# ggplot(medal2 , aes(year , TotalMedal  , color = medal)) + geom_line()


#------------------------- how did the Olympics suffer from the WWII



medal2 <-
  olympics %>% 
  group_by(year, medal = medal=="Gold" , season) %>% #the idea here is when we group by medal = gold this will show us the number of games (one medal per game)
  
  summarise(
    TotalMedal = length(medal) ## i dont know whay na.rm do yet but when i remove it the graph change a lot
  )


medal2 %>% 
  filter(medal , medal == TRUE)    ->medal ##the idea here is will count the number of true(medal == gold) in each year summer or 

ggplot(medal , aes(year , TotalMedal  , color = season)) + geom_line() +
  geom_point(aes(x=1920, y=470), size=30, shape=1, color="gold4") +
  geom_point(aes(x=1945, y=300), size=30, shape=1, color="gold4") +
  labs(title ="WWII & Olympcs " ,y = "Total Games", x = "YEAR of the event" , caption = "*the circles shows the start and the end of the war \n this also shows that the winter olympics started in the middle of the war  ") +
  # please change the title to good name
  theme_bw() + theme(legend.position = c(.05,.92) , )


#--------------------
## germany in wrold war 2 
olympics %>% 
  group_by(team , year ) %>% 
  filter(grepl("Germany", team)) %>% 
  
  # filter(team == "Germany" | team == "*Germany" ) %>% 
  summarise( team , year , numberOfParticipants= length(name) ,season) %>% 
  distinct() -> GermanyDataset

GermanyDataset  %>% 
  ggplot(aes(year ,numberOfParticipants , fill = team )) + geom_bar(stat = "identity")+ ##  I dont like this plot but  
  
  facet_grid(. ~ season) +
  labs(title = "Germany during WWII era" , caption = " between 1920 to 1945 Germany have not played any olympics except 3   ") +scale_color_brewer(palette = "Dark3")

#-----

#the most person with medals (any) 


olympics %>% 
  filter(!is.na(medal)) %>% 
  group_by(name) %>% 
  summarise(name , total = length(medal)) %>%  distinct() %>% 
  arrange( desc(total)  ) %>%  head(10)  -> medalyPerson


medalyPerson %>% 
ggplot(aes(name , total)) + geom_col() 

#----

#the most person with GOLD medals
olympics %>% 
  filter(medal == "Gold") %>% 
  group_by(name ) %>% 
  summarise(name , total = length(medal)) %>%  distinct() %>% 
  arrange( desc(total)  ) %>%  head(10) -> GoldenPerson

GoldenPerson %>% 
  ggplot(aes(name , total)) + geom_col()






