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


